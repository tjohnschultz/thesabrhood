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
    paste0(
      "product_group,status,blocks_publication\n",
      "completed_game_pbp,current,TRUE\n",
      "fangraphs_season,stale,FALSE\n"
    ),
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
  file.exists(file.path(release$path, "packages", "private_state.tar.gz")),
  file.exists(file.path(release$path, "packages", "public_data.tar.gz")),
  file.exists(file.path(release$path, "packages", "site.tar.gz")),
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

unpacked_private_state <- tempfile("sabrhood-unpacked-private-")
dir.create(unpacked_private_state)
utils::untar(
  file.path(release$path, "packages", "private_state.tar.gz"),
  exdir = unpacked_private_state,
  tar = "internal"
)
stopifnot(
  file.exists(file.path(
    unpacked_private_state,
    "components",
    "private_state",
    ".private-data",
    "pbp",
    "2026",
    "current.rds"
  )),
  !file.exists(file.path(
    unpacked_private_state,
    "components",
    "private_state",
    ".private-data",
    "lf-checkout",
    "should-not-publish.txt"
  ))
)

promote_local_release(store_root, "test-release-1")
current <- jsonlite::read_json(
  file.path(store_root, "current.json"),
  simplifyVector = TRUE
)
stopifnot(identical(current$release_key, "test-release-1"))

writeLines(
  paste0(
    "product_group,status,blocks_publication\n",
    "completed_game_pbp,stale,TRUE\n",
    "fangraphs_season,stale,FALSE\n"
  ),
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
  grepl("completed_game_pbp", as.character(failed), fixed = TRUE),
  !grepl("fangraphs_season", as.character(failed), fixed = TRUE),
  !dir.exists(file.path(store_root, "releases", failed_key)),
  identical(current_after_failure$release_key, "test-release-1")
)

cat("Local release-store contract test passed.\n")

remote_fixture_root <- tempfile("sabrhood-remote-release-")
dir.create(remote_fixture_root, recursive = TRUE)
on.exit(unlink(remote_fixture_root, recursive = TRUE, force = TRUE), add = TRUE)
remote_component_relative <- ".private-data/pbp/fixture.rds"
remote_component_path <- file.path(
  remote_fixture_root,
  "components",
  "private_state",
  remote_component_relative
)
dir.create(dirname(remote_component_path), recursive = TRUE)
set.seed(42L)
large_fixture_body <- as.raw(sample.int(256L, 5000L, replace = TRUE) - 1L)
writeBin(large_fixture_body, remote_component_path)
dir.create(file.path(remote_fixture_root, "packages"))
large_fixture <- file.path(
  remote_fixture_root,
  "packages",
  "private_state.tar.gz"
)
prior_directory <- setwd(remote_fixture_root)
utils::tar(
  "packages/private_state.tar.gz",
  "components/private_state",
  compression = "gzip",
  tar = "internal"
)
setwd(prior_directory)
stopifnot(as.numeric(file.info(large_fixture)$size) > 1024L)
remote_local_manifest <- list(
  contract_version = 2L,
  release_key = "remote-test-1",
  status = "staged",
  components = list(
    private_state = list(
      files = 1L,
      bytes = length(large_fixture_body),
      entries = list(list(
        path = remote_component_relative,
        bytes = length(large_fixture_body),
        sha256 = release_sha256(remote_component_path)
      ))
    )
  ),
  packages = list(list(
    component = "private_state",
    path = "packages/private_state.tar.gz",
    bytes = as.numeric(file.info(large_fixture)$size),
    sha256 = release_sha256(large_fixture)
  ))
)
jsonlite::write_json(
  remote_local_manifest,
  file.path(remote_fixture_root, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
large_package_body <- readBin(
  large_fixture,
  "raw",
  n = as.numeric(file.info(large_fixture)$size)
)
expected_package_parts <- ceiling(length(large_package_body) / 1024L)

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
  length(chunk_names) == expected_package_parts,
  identical(reassembled, large_package_body),
  remote_release$release_key == "remote-test-1",
  remote_release$remote_manifest_path %in% uploaded_names,
  all(vapply(
    uploaded_names,
    function(object_path) {
      isTRUE(get(object_path, envir = uploaded_objects)$upsert)
    },
    logical(1)
  )),
  !"current.json" %in% uploaded_names
)

retried_remote_release <- upload_staged_release(
  remote_fixture_root,
  config = fake_config,
  chunk_bytes = 1024L,
  upload = fake_upload
)
stopifnot(
  retried_remote_release$release_key == "remote-test-1",
  !"current.json" %in% ls(uploaded_objects)
)

fake_download <- function(config, object_path, timeout_seconds = 600L) {
  get(object_path, envir = uploaded_objects, inherits = FALSE)$body
}
verified_remote <- verify_supabase_release(
  release_key = remote_release$release_key,
  config = fake_config,
  local_release_root = remote_fixture_root,
  download = fake_download
)
stopifnot(
  verified_remote$status == "verified",
  verified_remote$files == 2L,
  verified_remote$objects == expected_package_parts + 2L,
  verified_remote$bytes > length(large_fixture_body)
)

fake_list <- function(config, prefix, limit, offset) {
  stopifnot(
    identical(prefix, "releases"),
    identical(limit, 1000L),
    identical(offset, 0L)
  )
  list(
    list(name = remote_release$release_key),
    list(name = "incomplete-release")
  )
}
release_inventory <- list_supabase_releases(
  config = fake_config,
  list_objects = fake_list,
  download = fake_download
)
complete_inventory <- release_inventory[
  release_inventory$release_key == remote_release$release_key,
  ,
  drop = FALSE
]
incomplete_inventory <- release_inventory[
  release_inventory$release_key == "incomplete-release",
  ,
  drop = FALSE
]
stopifnot(
  nrow(release_inventory) == 2L,
  nrow(complete_inventory) == 1L,
  complete_inventory$status == "staged",
  complete_inventory$integrity == "incomplete",
  complete_inventory$files == verified_remote$files,
  complete_inventory$objects == verified_remote$objects,
  complete_inventory$bytes == verified_remote$bytes,
  complete_inventory$private_files == 1L,
  complete_inventory$public_files == 0L,
  complete_inventory$site_files == 0L,
  nrow(incomplete_inventory) == 1L,
  incomplete_inventory$status == "unreadable",
  incomplete_inventory$integrity == "unreadable",
  is.na(incomplete_inventory$bytes)
)

retention_fixture <- data.frame(
  release_key = c("complete-new", "incomplete-old", "complete-original"),
  uploaded_at_utc = c(
    "2026-07-24T12:00:00Z",
    "2026-07-24T11:00:00Z",
    "2026-07-24T10:00:00Z"
  ),
  bytes = c(200, 50, 200),
  status = rep("staged", 3L),
  integrity = c("complete", "incomplete", "complete"),
  stringsAsFactors = FALSE
)
retention_plan <- plan_supabase_retention(
  retention_fixture,
  keep_complete = 1L,
  protect = "complete-original"
)
stopifnot(
  retention_plan$action[
    retention_plan$release_key == "complete-new"
  ] == "retain",
  retention_plan$action[
    retention_plan$release_key == "complete-original"
  ] == "retain",
  retention_plan$action[
    retention_plan$release_key == "incomplete-old"
  ] == "delete-candidate"
)

delete_request_paths <- character()
fake_delete_request <- function(url, headers, body, timeout_seconds) {
  stopifnot(
    identical(
      url,
      "https://fixture.supabase.co/storage/v1/object/pipeline-releases"
    ),
    identical(headers[["apikey"]], fake_config$secret_key)
  )
  delete_request_paths <<- c(delete_request_paths, body$prefixes)
  list(status = "deleted")
}
deleted_batch <- supabase_storage_delete_objects(
  config = fake_config,
  object_paths = c(
    "releases/remote-test-1/object-a",
    "releases/remote-test-1/object-b"
  ),
  request = fake_delete_request
)
stopifnot(
  identical(deleted_batch, delete_request_paths),
  length(deleted_batch) == 2L
)

wrong_confirmation <- try(
  delete_incomplete_supabase_release(
    release_key = remote_release$release_key,
    confirm_release_key = "different-release",
    config = fake_config,
    list_objects = fake_list,
    download = fake_download
  ),
  silent = TRUE
)
captured_deletion_paths <- character()
fake_delete_objects <- function(config, object_paths) {
  captured_deletion_paths <<- object_paths
  object_paths
}
deleted_incomplete <- delete_incomplete_supabase_release(
  release_key = remote_release$release_key,
  confirm_release_key = remote_release$release_key,
  config = fake_config,
  list_objects = fake_list,
  download = fake_download,
  delete_objects = fake_delete_objects
)
stopifnot(
  inherits(wrong_confirmation, "try-error"),
  deleted_incomplete$status == "deleted",
  deleted_incomplete$release_key == remote_release$release_key,
  deleted_incomplete$objects == length(captured_deletion_paths),
  paste0(
    "releases/",
    remote_release$release_key,
    "/remote-manifest.json"
  ) %in% captured_deletion_paths,
  all(startsWith(
    captured_deletion_paths,
    paste0("releases/", remote_release$release_key, "/")
  )),
  !"current.json" %in% captured_deletion_paths
)

restore_target <- tempfile("sabrhood-restored-release-")
restored_remote <- restore_supabase_release(
  release_key = remote_release$release_key,
  target_root = restore_target,
  components = "private_state",
  config = fake_config,
  download = fake_download
)
restored_component <- file.path(restore_target, remote_component_relative)
stopifnot(
  restored_remote$status == "restored",
  restored_remote$files == 1L,
  restored_remote$bytes == length(large_fixture_body),
  identical(
    readBin(restored_component, "raw", n = length(large_fixture_body)),
    large_fixture_body
  )
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

corrupted_object <- chunk_names[[1L]]
original_corrupted_body <- get(
  corrupted_object,
  envir = uploaded_objects,
  inherits = FALSE
)
corrupted_body <- original_corrupted_body
corrupted_body$body[[1L]] <- as.raw(
  bitwXor(as.integer(corrupted_body$body[[1L]]), 1L)
)
assign(corrupted_object, corrupted_body, envir = uploaded_objects)
corrupt_verification <- try(
  verify_supabase_release(
    release_key = remote_release$release_key,
    config = fake_config,
    local_release_root = remote_fixture_root,
    download = fake_download
  ),
  silent = TRUE
)
stopifnot(inherits(corrupt_verification, "try-error"))
assign(
  corrupted_object,
  original_corrupted_body,
  envir = uploaded_objects
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
