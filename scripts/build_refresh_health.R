derived_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
configured_date <- Sys.getenv("SABRHOOD_DATE", unset = "")
reference_date <- as.Date(if (nzchar(configured_date)) configured_date else Sys.Date())
if (is.na(reference_date)) stop("SABRHOOD_DATE must use YYYY-MM-DD.", call. = FALSE)

read_product <- function(name) {
  path <- file.path(derived_dir, name)
  if (!file.exists(path)) stop("Missing freshness input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
date_value <- function(data, candidates) {
  column <- candidates[candidates %in% names(data)][1L]
  if (is.na(column)) return(as.Date(NA))
  values <- as.Date(substr(as.character(data[[column]]), 1L, 10L))
  values <- values[!is.na(values)]
  if (!length(values)) as.Date(NA) else max(values)
}

pbp_status <- read_product("data-refresh-status.csv")
hitters <- read_product("hitter-performance-summary.csv")
stories <- read_product("daily-story-queue.csv")
history <- read_product("historical-anniversary-notes.csv")
fangraphs <- read_product("fangraphs-season-hitters.csv")
aaa <- read_product("aaa-hitter-watch.csv")
graphics <- read_product("graphics-feed-manifest.csv")
games <- read_product("daily-game-inputs.csv")
lineups <- read_product("daily-batting-orders.csv")
projections <- read_product("daily-projections-live.csv")
matchup_model <- read_product("daily-matchup-event-model-card.csv")
state_model <- read_product("daily-state-simulation-model-card.csv")
baserunning_model <- read_product("baserunning-model-card.csv")
slate_status <- read_product("daily-slate-status.csv")
award_history <- read_product("award-race-history.csv")
standings <- read_product("mlb-standings-current.csv")
newsletter <- read_product("daily-newsletter-edition.csv")

status_date <- date_value(slate_status, "report_date")
off_day <- nrow(slate_status) > 0L && identical(as.character(slate_status$slate_state[[1L]]), "no_games_scheduled") && identical(status_date, reference_date)
slate_date <- if (off_day) status_date else date_value(games, "game_date")
projection_date <- if (off_day) status_date else date_value(projections, "game_date")
matchup_date <- if (off_day) status_date else date_value(matchup_model, "game_date")
state_date <- if (off_day) status_date else date_value(state_model, "game_date")
lineup_models_gated <- !off_day && nrow(lineups) < 9L
matchup_expected_date <- if (lineup_models_gated && !is.na(matchup_date)) {
  matchup_date
} else {
  reference_date
}
state_expected_date <- if (lineup_models_gated && !is.na(state_date)) {
  state_date
} else {
  reference_date
}

pbp_date <- date_value(pbp_status, "source_through")
rows <- data.frame(
  product_group = c(
    "completed_game_pbp", "pbp_analysis", "editorial_story_engine", "history_engine",
    "fangraphs_season", "triple_a_watch", "graphics_feed", "daily_slate",
    "daily_projections", "daily_matchup_model", "daily_state_simulation",
    "phase4_baserunning", "award_race_history", "standings_movement",
    "daily_newsletter"
  ),
  source_through = as.Date(c(
    pbp_date,
    date_value(hitters, c("source_through", "last_game")),
    date_value(stories, "source_through"),
    date_value(history, "report_date"),
    date_value(fangraphs, "source_acquired_at_utc"),
    date_value(aaa, "source_acquired_at_utc"),
    date_value(graphics, "source_acquired_at_utc"),
    slate_date,
    projection_date,
    matchup_date,
    state_date,
    date_value(baserunning_model, "source_end_date"),
    date_value(award_history, c("source_through", "checkpoint_date")),
    date_value(standings, "source_through"),
    date_value(newsletter, "edition_date")
  ), origin = "1970-01-01"),
  expected_through = as.Date(c(
    reference_date - 1L,
    pbp_date,
    pbp_date,
    reference_date,
    reference_date,
    reference_date,
    reference_date,
    reference_date,
    reference_date,
    matchup_expected_date,
    state_expected_date,
    pbp_date,
    reference_date,
    reference_date - 1L,
    reference_date
  ), origin = "1970-01-01"),
  max_lag_days = c(4L, 0L, 0L, 0L, 2L, 3L, 2L, 0L, 0L, 0L, 0L, 0L, 8L, 1L, 0L),
  cadence = c(
    "daily", "daily", "daily", "daily", "daily", "daily", "daily",
    "daily", "daily", "lineup-dependent", "lineup-dependent", "daily", "weekly",
    "daily", "daily"
  ),
  stringsAsFactors = FALSE
)
rows$lag_days <- as.integer(rows$expected_through - rows$source_through)
rows$status <- ifelse(
  is.na(rows$source_through),
  "missing_date",
  ifelse(rows$lag_days <= rows$max_lag_days, "current", "stale")
)
rows$checked_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
rows$reference_date <- as.character(reference_date)
utils::write.csv(rows, file.path(derived_dir, "refresh-health.csv"), row.names = FALSE, na = "")

print(rows[, c("product_group", "source_through", "expected_through", "lag_days", "max_lag_days", "status")], row.names = FALSE)
configured_gate <- trimws(Sys.getenv("SABRHOOD_HEALTH_GROUPS", unset = ""))
gate_groups <- if (nzchar(configured_gate)) {
  trimws(strsplit(configured_gate, ",", fixed = TRUE)[[1L]])
} else {
  rows$product_group
}
missing_gate_groups <- setdiff(gate_groups, rows$product_group)
if (length(missing_gate_groups)) {
  stop(
    "Unknown freshness gate group(s): ",
    paste(missing_gate_groups, collapse = ", "),
    call. = FALSE
  )
}
gate_rows <- rows[rows$product_group %in% gate_groups, , drop = FALSE]
if (any(gate_rows$status != "current")) {
  stop(
    "Freshness gate failed for: ",
    paste(gate_rows$product_group[gate_rows$status != "current"], collapse = ", "),
    call. = FALSE
  )
}
cat(
  "Freshness gate passed for: ",
  paste(gate_rows$product_group, collapse = ", "),
  ".\n",
  sep = ""
)
