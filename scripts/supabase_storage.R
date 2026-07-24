#!/usr/bin/env Rscript

script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]
  dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
}

parse_options <- function(arguments) {
  positional <- arguments[!grepl("^--", arguments)]
  flags <- arguments[grepl("^--", arguments)]
  values <- list()
  for (flag in flags) {
    parts <- strsplit(sub("^--", "", flag), "=", fixed = TRUE)[[1L]]
    values[[parts[[1L]]]] <- if (length(parts) == 1L) {
      TRUE
    } else {
      paste(parts[-1L], collapse = "=")
    }
  }
  list(positional = positional, values = values)
}

scripts_root <- script_path()
repository_root <- dirname(scripts_root)
storage_r_root <- file.path(repository_root, "infrastructure", "storage", "R")
source(file.path(storage_r_root, "release_store.R"))
source(file.path(storage_r_root, "supabase_storage.R"))

options <- parse_options(commandArgs(trailingOnly = TRUE))
command <- if (length(options$positional)) options$positional[[1L]] else "help"
store_root <- options$values[["store-root"]]
if (is.null(store_root)) {
  store_root <- file.path(repository_root, ".backend", "release-store")
}
release_key <- options$values[["release-key"]]

if (identical(command, "help")) {
  cat(
    paste(
      "The SABRhood Supabase Storage tool",
      "",
      "Commands:",
      "  probe",
      "  plan --release-key=KEY [--store-root=PATH]",
      "  upload --release-key=KEY [--store-root=PATH]",
      "  promote --release-key=KEY [--store-root=PATH]",
      "",
      "Required for network commands:",
      "  SABRHOOD_SUPABASE_URL",
      "  SABRHOOD_SUPABASE_SECRET_KEY",
      "  SABRHOOD_SUPABASE_BUCKET (defaults to pipeline-releases)",
      "",
      "upload stages immutable objects and a manifest; it does not promote.",
      sep = "\n"
    ),
    "\n"
  )
  quit(status = 0L)
}

if (identical(command, "probe")) {
  object_path <- supabase_storage_probe()
  cat("Private storage write succeeded: ", object_path, "\n", sep = "")
  quit(status = 0L)
}

if (is.null(release_key)) {
  stop(command, " requires --release-key=KEY.", call. = FALSE)
}
validate_release_key(release_key)
release_root <- file.path(store_root, "releases", release_key)

if (identical(command, "plan")) {
  plan <- release_upload_plan(release_root)
  cat("Release: ", release_key, "\n", sep = "")
  cat("Files: ", nrow(plan), "\n", sep = "")
  cat(
    "Bytes: ",
    format(sum(plan$bytes), big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  cat("Remote objects before manifest: ", sum(plan$parts), "\n", sep = "")
  oversized <- plan[plan$parts > 1L, c("path", "bytes", "parts"), drop = FALSE]
  if (nrow(oversized)) {
    cat("\nFiles split into 40 MiB parts:\n")
    print(oversized, row.names = FALSE)
  }
  quit(status = 0L)
}

if (identical(command, "upload")) {
  result <- upload_staged_release(release_root)
  cat("Staged Supabase release: ", result$release_key, "\n", sep = "")
  cat("Manifest: ", result$remote_manifest_path, "\n", sep = "")
  cat("The current release pointer was not changed.\n")
  quit(status = 0L)
}

if (identical(command, "promote")) {
  promote_supabase_release(
    release_key = release_key,
    remote_manifest_path = paste0(
      "releases/",
      release_key,
      "/remote-manifest.json"
    )
  )
  cat("Promoted Supabase release: ", release_key, "\n", sep = "")
  quit(status = 0L)
}

stop("Unknown command: ", command, call. = FALSE)
