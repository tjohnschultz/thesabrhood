site_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
arguments <- commandArgs(trailingOnly = TRUE)
allow_stale <- "--allow-stale-data" %in% arguments

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) {
  candidate <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!file.exists(candidate)) stop("Rscript executable was not found.", call. = FALSE)
  rscript <- candidate
}

run_script <- function(script, args = character()) {
  path <- file.path(site_root, script)
  if (!file.exists(path)) stop("Missing site finalization script: ", script, call. = FALSE)
  status <- system2(rscript, c("--vanilla", shQuote(path), args))
  if (!identical(status, 0L)) {
    stop("Rendered-site finalization failed in ", script, ".", call. = FALSE)
  }
}

run_script("scripts/restore_legacy_assets.R")
run_script("scripts/normalize_legacy_articles.R")
run_script("scripts/prune_retired_site_outputs.R")
run_script("scripts/write_build_info.R")

validation_args <- "--rendered"
if (allow_stale) validation_args <- c(validation_args, "--allow-stale-data")
run_script("scripts/validate_site.R", validation_args)

cat(
  "Rendered site finalized: archive snapshots restored, page chrome normalized,",
  "retired outputs pruned, build record written, and validation passed.\n"
)
