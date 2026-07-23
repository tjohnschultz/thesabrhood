#!/usr/bin/env Rscript

script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]
  if (!length(file_arg)) return(normalizePath(".", mustWork = TRUE))
  dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
}

cloud_run_root <- script_path()
source(file.path(cloud_run_root, "R", "adapter_contract.R"))
source(file.path(cloud_run_root, "R", "retrosheet_adapter.R"))
source(file.path(cloud_run_root, "R", "database.R"))
source(file.path(cloud_run_root, "R", "pipeline.R"))

parse_options <- function(arguments) {
  positional <- arguments[!grepl("^--", arguments)]
  flags <- arguments[grepl("^--", arguments)]
  values <- list()
  for (flag in flags) {
    stripped <- sub("^--", "", flag)
    parts <- strsplit(stripped, "=", fixed = TRUE)[[1L]]
    values[[parts[[1L]]]] <- if (length(parts) == 1L) TRUE else paste(parts[-1L], collapse = "=")
  }
  list(positional = positional, values = values)
}

options <- parse_options(commandArgs(trailingOnly = TRUE))
command <- options$positional[[1L]] %||% "help"

if (identical(command, "help")) {
  cat(
    paste(
      "The SABRhood backend job",
      "",
      "Commands:",
      "  retrosheet --season=2025 [--archive=path] [--dry-run] [--publish]",
      "",
      "DATABASE_URL is required unless --dry-run is supplied.",
      "Publishing is always explicit; a normal database run only stages a release.",
      sep = "\n"
    ),
    "\n"
  )
  quit(status = 0L)
}

if (!identical(command, "retrosheet")) {
  stop("Unknown command: ", command, call. = FALSE)
}

season <- as.integer(options$values$season %||% format(Sys.Date(), "%Y"))
archive_path <- options$values$archive %||% NULL
dry_run <- isTRUE(options$values[["dry-run"]])
publish <- isTRUE(options$values$publish)

run_retrosheet_pipeline(
  season = season,
  archive_path = archive_path,
  dry_run = dry_run,
  publish = publish
)
