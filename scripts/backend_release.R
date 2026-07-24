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
source(file.path(
  repository_root,
  "infrastructure",
  "storage",
  "R",
  "release_store.R"
))

options <- parse_options(commandArgs(trailingOnly = TRUE))
command <- if (length(options$positional)) options$positional[[1L]] else "help"
store_root <- options$values[["store-root"]]
if (is.null(store_root)) {
  store_root <- file.path(repository_root, ".backend", "release-store")
}

if (identical(command, "help")) {
  cat(
    paste(
      "The SABRhood release-store tool",
      "",
      "Commands:",
      "  inspect",
      "  stage [--release-key=KEY] [--store-root=PATH]",
      "  promote --release-key=KEY [--store-root=PATH]",
      "",
      "inspect is read-only. stage writes an immutable local release.",
      "promote changes only the local current-release pointer.",
      sep = "\n"
    ),
    "\n"
  )
  quit(status = 0L)
}

if (identical(command, "inspect")) {
  inventory <- release_inventory(repository_root, include_hashes = FALSE)
  for (component_name in names(inventory)) {
    component <- inventory[[component_name]]
    cat(
      sprintf(
        "%-16s %6d files %9.2f MiB\n",
        component_name,
        nrow(component),
        sum(component$bytes) / 1024^2
      )
    )
  }
  quit(status = 0L)
}

release_key <- options$values[["release-key"]]
if (identical(command, "stage")) {
  if (is.null(release_key)) {
    release_key <- release_key_default()
  }
  release <- build_local_release(
    repository_root = repository_root,
    store_root = store_root,
    release_key = release_key
  )
  cat("Staged release: ", release$release_key, "\n", sep = "")
  cat("Release path: ", release$path, "\n", sep = "")
  quit(status = 0L)
}

if (identical(command, "promote")) {
  if (is.null(release_key)) {
    stop("promote requires --release-key=KEY.", call. = FALSE)
  }
  promote_local_release(store_root, release_key)
  cat("Promoted release: ", release_key, "\n", sep = "")
  quit(status = 0L)
}

stop("Unknown command: ", command, call. = FALSE)
