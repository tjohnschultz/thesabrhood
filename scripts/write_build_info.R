args <- commandArgs(trailingOnly = TRUE)

output_path <- if (length(args)) args[[1L]] else file.path("docs", "build-info.json")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

json_escape <- function(value) {
  value <- enc2utf8(as.character(value %||% ""))
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub("\"", "\\\"", value, fixed = TRUE)
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  value
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || is.na(x[[1L]]) || !nzchar(x[[1L]])) y else x[[1L]]
}

source_sha <- Sys.getenv("GITHUB_SHA", unset = "local")
run_id <- Sys.getenv("GITHUB_RUN_ID", unset = "local")
repository <- Sys.getenv("GITHUB_REPOSITORY", unset = "local")
run_url <- if (identical(run_id, "local") || identical(repository, "local")) {
  ""
} else {
  sprintf("https://github.com/%s/actions/runs/%s", repository, run_id)
}

values <- c(
  source_commit = source_sha,
  rendered_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  workflow_run = run_url
)

lines <- c(
  "{",
  paste0(
    "  \"", names(values), "\": \"", vapply(values, json_escape, character(1L)), "\"",
    c(",", ",", "")
  ),
  "}"
)

writeLines(lines, output_path, useBytes = TRUE)
message("Wrote frontend build record to ", normalizePath(output_path, winslash = "/", mustWork = FALSE))
