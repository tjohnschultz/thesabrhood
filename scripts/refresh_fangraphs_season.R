workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(baseballr))

season_year <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
if (!is.finite(season_year)) stop("SABRHOOD_SEASON must be a four-digit year.", call. = FALSE)

source_dir <- file.path(workspace, ".private-data", "sources")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

cat("Pulling FanGraphs hitter season data for", season_year, "...\n")
hitters <- baseballr::fg_batter_leaders(
  startseason = as.character(season_year),
  endseason = as.character(season_year),
  qual = "0",
  ind = "1",
  pageitems = "10000"
)

cat("Pulling FanGraphs pitcher season data for", season_year, "...\n")
pitchers <- baseballr::fg_pitcher_leaders(
  startseason = as.character(season_year),
  endseason = as.character(season_year),
  qual = "0",
  ind = "1",
  pageitems = "10000"
)

history_start <- season_year - 1L
history_end <- season_year - 1L
cat("Pulling FanGraphs prior-season eligibility screen for", history_end, "...\n")
prior_hitters <- baseballr::fg_batter_leaders(
  startseason = as.character(history_start), endseason = as.character(history_end),
  qual = "0", ind = "1", pageitems = "10000"
)
prior_pitchers <- baseballr::fg_pitcher_leaders(
  startseason = as.character(history_start), endseason = as.character(history_end),
  qual = "0", ind = "1", pageitems = "10000"
)

snapshot <- list(
  season = season_year,
  acquired_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  source = "baseballr_fangraphs_leaderboards",
  hitters = hitters,
  pitchers = pitchers,
  prior_hitters = prior_hitters,
  prior_pitchers = prior_pitchers,
  rookie_history_start = history_start
)
saveRDS(snapshot, file.path(source_dir, paste0("fangraphs-season-", season_year, ".rds")))
cat("Saved", nrow(hitters), "current hitter rows,", nrow(pitchers), "current pitcher rows, and prior-career eligibility tables to the private source cache.\n")
