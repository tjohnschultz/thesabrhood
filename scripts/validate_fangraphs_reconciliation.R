args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (!length(match)) return(default)
  sub(prefix, "", match[[length(match)]], fixed = TRUE)
}

workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
baseline_dir <- option(
  "baseline",
  file.path(workspace, ".backend", "fangraphs-baseline")
)
derived_dir <- Sys.getenv(
  "SABRHOOD_DERIVED_DIR",
  unset = file.path(workspace, "data", "derived")
)

read_table <- function(path, label) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
  data <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!nrow(data)) stop(label, " is empty.", call. = FALSE)
  data
}

validate_table <- function(data, label, volume_column) {
  required <- c(
    "season",
    "player_id",
    "player_name",
    volume_column,
    "source_acquired_at_utc"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  player_ids <- as.character(data$player_id)
  if (any(is.na(player_ids) | !nzchar(player_ids))) {
    stop(label, " contains a missing player identifier.", call. = FALSE)
  }
  if (anyDuplicated(player_ids)) {
    stop(label, " contains duplicate player identifiers.", call. = FALSE)
  }
  acquired <- suppressWarnings(as.POSIXct(
    data$source_acquired_at_utc,
    tz = "UTC"
  ))
  if (all(is.na(acquired))) {
    stop(label, " lacks a usable acquisition timestamp.", call. = FALSE)
  }
  invisible(data)
}

summarize_table <- function(data, volume_column) {
  values <- suppressWarnings(as.numeric(data[[volume_column]]))
  values[!is.finite(values)] <- 0
  list(rows = nrow(data), volume = sum(values))
}

compare_table <- function(current, baseline, label, volume_column) {
  current_summary <- summarize_table(current, volume_column)
  baseline_summary <- summarize_table(baseline, volume_column)
  minimum_rows <- max(100L, floor(baseline_summary$rows * 0.9))
  if (current_summary$rows < minimum_rows) {
    stop(
      label,
      " row count regressed from ",
      baseline_summary$rows,
      " to ",
      current_summary$rows,
      ".",
      call. = FALSE
    )
  }
  minimum_volume <- baseline_summary$volume * 0.99
  if (current_summary$volume < minimum_volume) {
    stop(
      label,
      " aggregate ",
      volume_column,
      " regressed from ",
      baseline_summary$volume,
      " to ",
      current_summary$volume,
      ".",
      call. = FALSE
    )
  }
  data.frame(
    table = label,
    baseline_rows = baseline_summary$rows,
    current_rows = current_summary$rows,
    baseline_volume = baseline_summary$volume,
    current_volume = current_summary$volume,
    content_advanced = current_summary$volume > baseline_summary$volume,
    stringsAsFactors = FALSE
  )
}

hitters_name <- "fangraphs-season-hitters.csv"
pitchers_name <- "fangraphs-season-pitchers.csv"
current_hitters <- read_table(
  file.path(derived_dir, hitters_name),
  "current FanGraphs hitter product"
)
current_pitchers <- read_table(
  file.path(derived_dir, pitchers_name),
  "current FanGraphs pitcher product"
)
baseline_hitters <- read_table(
  file.path(baseline_dir, hitters_name),
  "baseline FanGraphs hitter product"
)
baseline_pitchers <- read_table(
  file.path(baseline_dir, pitchers_name),
  "baseline FanGraphs pitcher product"
)

validate_table(current_hitters, "current FanGraphs hitter product", "pa")
validate_table(current_pitchers, "current FanGraphs pitcher product", "batters_faced")
comparisons <- rbind(
  compare_table(
    current_hitters,
    baseline_hitters,
    "hitters",
    "pa"
  ),
  compare_table(
    current_pitchers,
    baseline_pitchers,
    "pitchers",
    "batters_faced"
  )
)

acquired_values <- c(
  as.character(current_hitters$source_acquired_at_utc),
  as.character(current_pitchers$source_acquired_at_utc)
)
acquired <- suppressWarnings(as.POSIXct(acquired_values, tz = "UTC"))
latest_acquired <- max(acquired, na.rm = TRUE)
age_hours <- as.numeric(difftime(Sys.time(), latest_acquired, units = "hours"))
if (!is.finite(age_hours) || age_hours > 6) {
  stop("FanGraphs reconciliation timestamp is not current.", call. = FALSE)
}

status <- data.frame(
  checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  source_acquired_at_utc = format(latest_acquired, tz = "UTC", usetz = TRUE),
  hitter_rows = nrow(current_hitters),
  pitcher_rows = nrow(current_pitchers),
  hitter_pa = sum(suppressWarnings(as.numeric(current_hitters$pa)), na.rm = TRUE),
  pitcher_batters_faced = sum(
    suppressWarnings(as.numeric(current_pitchers$batters_faced)),
    na.rm = TRUE
  ),
  content_advanced = any(comparisons$content_advanced),
  status = "pass",
  stringsAsFactors = FALSE
)
utils::write.csv(
  status,
  file.path(derived_dir, "fangraphs-reconciliation-status.csv"),
  row.names = FALSE
)

print(comparisons, row.names = FALSE)
if (!status$content_advanced[[1L]]) {
  message(
    "FanGraphs products passed structural checks but did not advance ",
    "beyond the published baseline."
  )
}
cat("FanGraphs reconciliation contract passed.\n")
