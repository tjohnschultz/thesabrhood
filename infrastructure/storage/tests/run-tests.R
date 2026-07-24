script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]
  dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
}

tests_root <- script_path()
storage_root <- dirname(tests_root)
source(file.path(storage_root, "R", "release_store.R"))
source(file.path(storage_root, "R", "supabase_storage.R"))

fixture_root <- tempfile("sabrhood-release-fixture-")
dir.create(fixture_root, recursive = TRUE)
on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

fixture_files <- list(
  ".private-data/pbp/2026/current.rds" = "private pbp",
  ".private-data/sources/fangraphs/hitters.rds" = "private source",
  ".private-data/projection-ledger/ledger.csv" = "private ledger",
  ".private-data/lf-checkout/should-not-publish.txt" = "excluded checkout",
  "data/derived/manifest.csv" = "file,rows\nsample.csv,1\n",
  "data/derived/refresh-health.csv" =
    "product_group,status\ncompleted_game_pbp,current\n",
  "data/derived/sample.csv" = "player,value\nExample,1\n",
  "images/graphics-feed/sample.png" = "fake png",
  "docs/index.html" = "<html><body>fixture</body></html>"
)

for (relative_path in names(fixture_files)) {
  destination <- file.path(fixture_root, relative_path)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  writeLines(fixture_files[[relative_path]], destination, useBytes = TRUE)
}

store_root <- file.path(fixture_root, ".backend", "release-store")
release <- build_local_release(
  repository_root = fixture_root,
  store_root = store_root,
  release_key = "test-release-1"
)

stopifnot(
  identical(release$release_key, "test-release-1"),
  file.exists(file.path(release$path, "manifest.json")),
  file.exists(file.path(
    release$path,
    "components",
    "private_state",
    ".private-data",
    "pbp",
    "2026",
    "current.rds"
  )),
  !file.exists(file.path(
    release$path,
    "components",
    "private_state",
    ".private-data",
    "lf-checkout",
    "should-not-publish.txt"
  )),
  !file.exists(file.path(store_root, "current.json"))
)

promote_local_release(store_root, "test-release-1")
current <- jsonlite::read_json(
  file.path(store_root, "current.json"),
  simplifyVector = TRUE
)
stopifnot(identical(current$release_key, "test-release-1"))

writeLines(
  "product_group,status\ncompleted_game_pbp,stale\n",
  file.path(fixture_root, "data", "derived", "refresh-health.csv")
)
failed_key <- "test-release-failing"
failed <- try(
  build_local_release(
    repository_root = fixture_root,
    store_root = store_root,
    release_key = failed_key
  ),
  silent = TRUE
)
current_after_failure <- jsonlite::read_json(
  file.path(store_root, "current.json"),
  simplifyVector = TRUE
)
stopifnot(
  inherits(failed, "try-error"),
  !dir.exists(file.path(store_root, "releases", failed_key)),
  identical(current_after_failure$release_key, "test-release-1")
)

cat("Local release-store contract test passed.\n")

remote_fixture_root <- tempfile("sabrhood-remote-release-")
dir.create(remote_fixture_root, recursive = TRUE)
on.exit(unlink(remote_fixture_root, recursive = TRUE, force = TRUE), add = TRUE)
writeLines(
  '{"release_key":"remote-test-1","status":"staged"}',
  file.path(remote_fixture_root, "manifest.json"),
  useBytes = TRUE
)
large_fixture <- file.path(remote_fixture_root, "large.bin")
large_fixture_body <- as.raw(rep(0:255, length.out = 2500L))
writeBin(large_fixture_body, large_fixture)

uploaded_objects <- new.env(parent = emptyenv())
fake_upload <- function(
    config,
    object_path,
    body,
    content_type = "application/octet-stream",
    upsert = FALSE,
    timeout_seconds = 180L) {
  if (exists(object_path, envir = uploaded_objects, inherits = FALSE) &&
      !isTRUE(upsert)) {
    stop("Fake storage refuses an immutable object overwrite.", call. = FALSE)
  }
  assign(
    object_path,
    list(body = body, content_type = content_type, upsert = upsert),
    envir = uploaded_objects
  )
  invisible(NULL)
}
fake_config <- list(
  url = "https://fixture.supabase.co",
  secret_key = "not-used-by-fake-upload",
  bucket = "pipeline-releases"
)
remote_release <- upload_staged_release(
  remote_fixture_root,
  config = fake_config,
  chunk_bytes = 1024L,
  upload = fake_upload
)
uploaded_names <- ls(uploaded_objects)
chunk_names <- sort(grep("[.]chunks/part-", uploaded_names, value = TRUE))
reassembled <- do.call(
  c,
  lapply(chunk_names, function(object_path) {
    get(object_path, envir = uploaded_objects, inherits = FALSE)$body
  })
)
stopifnot(
  length(chunk_names) == 3L,
  identical(reassembled, large_fixture_body),
  remote_release$release_key == "remote-test-1",
  remote_release$remote_manifest_path %in% uploaded_names,
  !"current.json" %in% uploaded_names
)

promote_supabase_release(
  release_key = remote_release$release_key,
  remote_manifest_path = remote_release$remote_manifest_path,
  config = fake_config,
  upload = fake_upload
)
stopifnot(
  "current.json" %in% ls(uploaded_objects),
  isTRUE(get("current.json", envir = uploaded_objects)$upsert)
)

failing_objects <- new.env(parent = emptyenv())
upload_attempt <- 0L
failing_upload <- function(config, object_path, body, ...) {
  upload_attempt <<- upload_attempt + 1L
  if (upload_attempt == 2L) {
    stop("Simulated network interruption.", call. = FALSE)
  }
  assign(object_path, body, envir = failing_objects)
  invisible(NULL)
}
failed_remote <- try(
  upload_staged_release(
    remote_fixture_root,
    config = fake_config,
    chunk_bytes = 1024L,
    upload = failing_upload
  ),
  silent = TRUE
)
stopifnot(
  inherits(failed_remote, "try-error"),
  !"current.json" %in% ls(failing_objects)
)

invalid_public_key <- try(
  supabase_storage_config(
    url = "https://fixture.supabase.co",
    secret_key = "sb_publishable_not_allowed",
    bucket = "pipeline-releases"
  ),
  silent = TRUE
)
stopifnot(inherits(invalid_public_key, "try-error"))

cat("Supabase storage adapter contract test passed.\n")
