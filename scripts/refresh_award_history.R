suppressPackageStartupMessages({
  library(baseballr)
  library(sabrhoodR)
})

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
opening_date <- as.Date(Sys.getenv("SABRHOOD_OPENING_DATE", unset = paste0(season_year, "-03-26")))
configured_end <- Sys.getenv("SABRHOOD_HISTORY_END", unset = "")
end_date <- as.Date(if (nzchar(configured_end)) configured_end else Sys.Date() - 1L)
if (is.na(opening_date) || is.na(end_date) || end_date < opening_date) {
  stop("Invalid award-history date range.", call. = FALSE)
}

source_dir <- file.path(".private-data", "sources")
checkpoint_dir <- file.path(source_dir, paste0("fangraphs-checkpoints-", season_year))
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
season_path <- file.path(source_dir, paste0("fangraphs-season-", season_year, ".rds"))
if (!file.exists(season_path)) stop("Current FanGraphs source snapshot is missing.", call. = FALSE)

first_checkpoint <- opening_date + 10L
checkpoint_dates <- seq.Date(first_checkpoint, end_date, by = "7 days")
if (!end_date %in% checkpoint_dates) checkpoint_dates <- sort(unique(c(checkpoint_dates, end_date)))

fetch_checkpoint <- function(checkpoint_date) {
  cache_path <- file.path(checkpoint_dir, paste0("checkpoint-", checkpoint_date, ".rds"))
  if (file.exists(cache_path)) {
    cat("Using cached FanGraphs checkpoint", as.character(checkpoint_date), "\n")
    return(readRDS(cache_path))
  }
  cat("Pulling cumulative FanGraphs checkpoint through", as.character(checkpoint_date), "...\n")
  shared <- list(
    startseason = as.character(season_year),
    endseason = as.character(season_year),
    startdate = as.character(opening_date),
    enddate = as.character(checkpoint_date),
    qual = "0",
    ind = "0",
    pageitems = "10000"
  )
  checkpoint <- list(
    date = checkpoint_date,
    opening_date = opening_date,
    acquired_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = "baseballr_fangraphs_cumulative_date_range",
    hitters = do.call(baseballr::fg_batter_leaders, shared),
    pitchers = do.call(baseballr::fg_pitcher_leaders, shared)
  )
  saveRDS(checkpoint, cache_path, compress = TRUE)
  checkpoint
}

checkpoints <- lapply(checkpoint_dates, function(checkpoint_date) {
  tryCatch(fetch_checkpoint(checkpoint_date), error = function(error) {
    message("Checkpoint failed for ", checkpoint_date, ": ", conditionMessage(error))
    NULL
  })
})
checkpoints <- checkpoints[!vapply(checkpoints, is.null, logical(1))]
if (length(checkpoints) < 2L) stop("Fewer than two FanGraphs history checkpoints were acquired.", call. = FALSE)

season_source <- readRDS(season_path)
prior <- standardize_fangraphs_season(
  season_source$prior_hitters,
  season_source$prior_pitchers,
  season_year - 1L
)
compact_checkpoints <- lapply(checkpoints, function(item) {
  compact <- standardize_fangraphs_season(item$hitters, item$pitchers, season_year)
  list(date = as.Date(item$date), hitters = compact$hitters, pitchers = compact$pitchers)
})
history <- build_award_race_history(
  compact_checkpoints,
  prior$hitters,
  prior$pitchers,
  award = "MVP",
  top_n = 8L,
  opening_date = opening_date
)

acquired_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
for (name in c("history", "display", "events", "leaders")) {
  history[[name]]$source_acquired_at_utc <- acquired_at
  history[[name]]$source_through <- as.character(end_date)
  history[[name]]$source_note <- paste(
    "Cumulative FanGraphs date-range checkpoints via BaseballR;",
    "each rating uses only data available through its checkpoint."
  )
}

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
utils::write.csv(history$history, file.path(output_dir, "award-race-history.csv"), row.names = FALSE, na = "")
utils::write.csv(history$display, file.path(output_dir, "award-race-display.csv"), row.names = FALSE, na = "")
utils::write.csv(history$events, file.path(output_dir, "award-race-events.csv"), row.names = FALSE, na = "")
utils::write.csv(history$leaders, file.path(output_dir, "award-race-current-leaders.csv"), row.names = FALSE, na = "")
cat("Built", nrow(history$history), "award-history rows through", as.character(end_date), ".\n")
