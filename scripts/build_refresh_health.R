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
projections <- read_product("daily-projections-live.csv")
slate_status <- read_product("daily-slate-status.csv")
award_history <- read_product("award-race-history.csv")

status_date <- date_value(slate_status, "report_date")
off_day <- nrow(slate_status) > 0L && identical(as.character(slate_status$slate_state[[1L]]), "no_games_scheduled") && identical(status_date, reference_date)
slate_date <- if (off_day) status_date else date_value(games, "game_date")
projection_date <- if (off_day) status_date else date_value(projections, "game_date")

pbp_date <- date_value(pbp_status, "source_through")
rows <- data.frame(
  product_group = c(
    "completed_game_pbp", "pbp_analysis", "editorial_story_engine", "history_engine",
    "fangraphs_season", "triple_a_watch", "graphics_feed", "daily_slate",
    "daily_projections", "award_race_history"
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
    date_value(award_history, c("source_through", "checkpoint_date"))
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
    reference_date
  ), origin = "1970-01-01"),
  max_lag_days = c(4L, 0L, 0L, 0L, 2L, 3L, 2L, 0L, 0L, 8L),
  cadence = c("daily", "daily", "daily", "daily", "daily", "daily", "daily", "daily", "daily", "weekly"),
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
if (any(rows$status != "current")) {
  stop(
    "Freshness gate failed for: ",
    paste(rows$product_group[rows$status != "current"], collapse = ", "),
    call. = FALSE
  )
}
cat("All required public product groups passed the freshness gate.\n")
