script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]
  dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
}

tests_root <- script_path()
storage_root <- dirname(tests_root)
source(file.path(storage_root, "R", "release_store.R"))

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
