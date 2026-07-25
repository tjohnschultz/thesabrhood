workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
validator <- file.path(
  workspace,
  "scripts",
  "validate_fangraphs_reconciliation.R"
)
source_dir <- file.path(workspace, "data", "derived")
fixture_root <- tempfile("sabrhood-fangraphs-contract-")
baseline_dir <- file.path(fixture_root, "baseline")
current_dir <- file.path(fixture_root, "current")
dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(current_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

files <- c(
  "fangraphs-season-hitters.csv",
  "fangraphs-season-pitchers.csv"
)
for (name in files) {
  source_path <- file.path(source_dir, name)
  stopifnot(file.exists(source_path))
  stopifnot(file.copy(source_path, file.path(baseline_dir, name)))
  stopifnot(file.copy(source_path, file.path(current_dir, name)))
}

refresh_timestamp <- function(path) {
  data <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data$source_acquired_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
}
invisible(lapply(file.path(current_dir, files), refresh_timestamp))

run_validator <- function() {
  command <- file.path(R.home("bin"), "Rscript")
  previous_derived <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = NA_character_)
  on.exit({
    if (is.na(previous_derived)) {
      Sys.unsetenv("SABRHOOD_DERIVED_DIR")
    } else {
      Sys.setenv(SABRHOOD_DERIVED_DIR = previous_derived)
    }
  }, add = TRUE)
  Sys.setenv(SABRHOOD_DERIVED_DIR = current_dir)
  output <- system2(
    command,
    c(
      "--vanilla",
      shQuote(validator),
      shQuote(paste0("--baseline=", baseline_dir))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  list(
    status = if (is.null(attr(output, "status"))) 0L else attr(output, "status"),
    output = output
  )
}

passing <- run_validator()
if (!identical(passing$status, 0L)) {
  stop(
    "Expected unchanged reconciliation fixture to pass:\n",
    paste(passing$output, collapse = "\n"),
    call. = FALSE
  )
}
stopifnot(file.exists(file.path(
  current_dir,
  "fangraphs-reconciliation-status.csv"
)))

regressed_path <- file.path(current_dir, "fangraphs-season-hitters.csv")
regressed <- utils::read.csv(
  regressed_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
regressed$pa <- suppressWarnings(as.numeric(regressed$pa)) * 0.5
utils::write.csv(regressed, regressed_path, row.names = FALSE, na = "")
failing <- suppressWarnings(run_validator())
if (identical(failing$status, 0L)) {
  stop("Expected aggregate playing-time regression to fail.", call. = FALSE)
}

cat("FanGraphs reconciliation contract test passed.\n")
