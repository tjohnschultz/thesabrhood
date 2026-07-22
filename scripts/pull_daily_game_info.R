workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages({
  library(sabrhoodR)
  library(baseballr)
})
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite before fetching weather.")

output_dir <- file.path(workspace, "data", "derived")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
target_date <- as.Date(Sys.getenv("SABRHOOD_DATE", unset = as.character(Sys.Date())))
if (is.na(target_date)) stop("SABRHOOD_DATE must use YYYY-MM-DD.", call. = FALSE)
season_year <- as.integer(format(target_date, "%Y"))
no_slate_marker <- file.path(workspace, ".private-data", "no-slate")
if (file.exists(no_slate_marker)) unlink(no_slate_marker)
slate_status_path <- file.path(output_dir, "daily-slate-status.csv")
write_slate_status <- function(state, game_count) {
  utils::write.csv(
    data.frame(
      report_date = as.character(target_date),
      slate_state = state,
      game_count = as.integer(game_count),
      generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      stringsAsFactors = FALSE
    ),
    slate_status_path,
    row.names = FALSE,
    na = ""
  )
}

bind_safely <- function(items) {
  items <- items[vapply(items, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(items)) return(data.frame())
  dplyr::bind_rows(items)
}

cat("Pulling MLB schedule for", as.character(target_date), "...\n")
schedule <- baseballr::mlb_game_pks(as.character(target_date), level_ids = 1)
if (!is.data.frame(schedule) || !nrow(schedule)) {
  dir.create(dirname(no_slate_marker), recursive = TRUE, showWarnings = FALSE)
  file.create(no_slate_marker)
  write_slate_status("no_games_scheduled", 0L)
  cat("No MLB slate returned for", as.character(target_date), "; preserving the last published board.\n")
  quit(save = "no", status = 0L)
}
if (!"game_pk" %in% names(schedule) && "gamePk" %in% names(schedule)) schedule$game_pk <- schedule$gamePk
game_pks <- unique(as.character(schedule$game_pk))

cat("Pulling probable starters and posted batting orders for", length(game_pks), "games...\n")
probables <- bind_safely(lapply(game_pks, function(game_pk) {
  tryCatch(baseballr::mlb_probables(game_pk), error = function(error) {
    message("Probables unavailable for ", game_pk, ": ", conditionMessage(error))
    data.frame()
  })
}))
orders <- bind_safely(lapply(game_pks, function(game_pk) {
  result <- tryCatch(baseballr::mlb_batting_orders(game_pk, type = "starting"), error = function(error) {
    message("Batting order unavailable for ", game_pk, ": ", conditionMessage(error))
    data.frame()
  })
  if (is.data.frame(result) && nrow(result) && !"game_pk" %in% names(result)) result$game_pk <- game_pk
  result
}))
inputs <- assemble_baseballr_game_inputs(schedule, probables, orders)
games <- inputs$games

team_reference <- unique(rbind(
  data.frame(team_id = games$away_team_id, team_name = games$away_team, stringsAsFactors = FALSE),
  data.frame(team_id = games$home_team_id, team_name = games$home_team, stringsAsFactors = FALSE)
))
team_reference <- team_reference[!is.na(team_reference$team_id) & team_reference$team_id != "", , drop = FALSE]
cat("Pulling", nrow(team_reference), "active rosters...\n")
raw_rosters <- bind_safely(lapply(team_reference$team_id, function(team_id) {
  result <- tryCatch(
    baseballr::mlb_rosters(team_id = team_id, season = season_year, roster_type = "active"),
    error = function(error) {
      message("Active roster unavailable for team ", team_id, ": ", conditionMessage(error))
      data.frame()
    }
  )
  if (is.data.frame(result) && nrow(result)) result$team_id <- as.character(team_id)
  result
}))
active_rosters <- standardize_baseballr_rosters(raw_rosters, target_date)
team_index <- match(active_rosters$team_id, team_reference$team_id)
active_rosters$team_name <- team_reference$team_name[team_index]

roster_count <- active_rosters |>
  dplyr::filter(.data$is_active) |>
  dplyr::count(.data$team_id, name = "active_roster_count")
roster_status <- function(team_ids) {
  index <- match(team_ids, roster_count$team_id)
  ifelse(!is.na(index) & roster_count$active_roster_count[index] > 0, "confirmed", "missing")
}
games$away_roster_status <- roster_status(games$away_team_id)
games$home_roster_status <- roster_status(games$home_team_id)

park_path <- file.path(workspace, "packages", "sabrhoodR", "inst", "extdata", "mlb-park-weather.csv")
parks <- utils::read.csv(park_path, stringsAsFactors = FALSE, check.names = FALSE)
park_index <- match(tolower(games$venue_name), tolower(parks$venue_name))
fallback_index <- match(tolower(games$home_team), tolower(parks$team_name))
park_index[is.na(park_index)] <- fallback_index[is.na(park_index)]
games$weather_location <- parks$weather_location[park_index]
games$latitude <- parks$latitude[park_index]
games$longitude <- parks$longitude[park_index]
games$roof_type <- parks$roof_type[park_index]

translate_weather <- function(code) {
  ifelse(code == 0, "Clear",
    ifelse(code %in% c(1, 2), "Partly cloudy",
      ifelse(code == 3, "Cloudy",
        ifelse(code %in% c(45, 48), "Fog",
          ifelse(code %in% 51:67, "Rain or drizzle",
            ifelse(code %in% 71:86, "Snow or showers",
              ifelse(code %in% c(95, 96, 99), "Thunderstorm", "Mixed conditions")))))))
}

weather_rows <- lapply(seq_len(nrow(games)), function(index) {
  game <- games[index, , drop = FALSE]
  if (is.na(game$latitude) || is.na(game$longitude)) {
    return(data.frame(game_id = game$game_id, weather_status = "missing", weather_note = "No park coordinate match"))
  }
  if (identical(tolower(game$roof_type), "fixed_dome")) {
    return(data.frame(
      game_id = game$game_id, weather_status = "indoors", weather_note = "Fixed dome",
      weather_location = game$weather_location, roof_type = game$roof_type,
      temperature_f = NA_real_, apparent_temperature_f = NA_real_, wind_mph = NA_real_,
      wind_gust_mph = NA_real_, wind_direction = NA_real_, precipitation_probability = NA_real_,
      precipitation_in = NA_real_, conditions = "Indoor environment", forecast_hours = 0L
    ))
  }
  url <- build_open_meteo_url(game$latitude, game$longitude, target_date, target_date, timezone = "auto")
  forecast <- tryCatch(jsonlite::fromJSON(url), error = function(error) NULL)
  if (is.null(forecast) || is.null(forecast$hourly)) {
    return(data.frame(game_id = game$game_id, weather_status = "missing", weather_note = "Forecast request failed"))
  }
  hourly <- as.data.frame(forecast$hourly, stringsAsFactors = FALSE)
  local_times <- as.POSIXct(hourly$time, tz = forecast$timezone)
  first_pitch_utc <- as.POSIXct(game$game_time_utc, tz = "UTC")
  first_pitch_local <- as.POSIXct(format(first_pitch_utc, tz = forecast$timezone, usetz = TRUE), tz = forecast$timezone)
  start_index <- which.min(abs(as.numeric(difftime(local_times, first_pitch_local, units = "secs"))))
  window_index <- seq.int(start_index, min(start_index + 4L, nrow(hourly)))
  window <- hourly[window_index, , drop = FALSE]
  data.frame(
    game_id = game$game_id,
    weather_status = "available",
    weather_note = ifelse(tolower(game$roof_type) == "retractable", "Forecast available; roof decision can supersede it", "Outdoor forecast"),
    weather_location = game$weather_location,
    roof_type = game$roof_type,
    temperature_f = mean(window$temperature_2m, na.rm = TRUE),
    apparent_temperature_f = mean(window$apparent_temperature, na.rm = TRUE),
    wind_mph = mean(window$wind_speed_10m, na.rm = TRUE),
    wind_gust_mph = max(window$wind_gusts_10m, na.rm = TRUE),
    wind_direction = mean(window$wind_direction_10m, na.rm = TRUE),
    precipitation_probability = max(window$precipitation_probability, na.rm = TRUE),
    precipitation_in = sum(window$precipitation, na.rm = TRUE),
    conditions = translate_weather(window$weather_code[[1L]]),
    forecast_hours = length(window_index),
    forecast_timezone = forecast$timezone,
    forecast_method = "open_meteo_ballpark_game_window_v1"
  )
})
weather <- dplyr::bind_rows(weather_rows)
weather_status_index <- match(games$game_id, weather$game_id)
games$weather_status <- weather$weather_status[weather_status_index]
games$park_factor <- ifelse(is.na(park_index), NA_real_, 1)
games$park_factor_note <- ifelse(is.na(park_index), "park unresolved", "neutral placeholder; factor model pending")
games <- validate_projection_game_inputs(games, allow_projected_lineups = TRUE)

availability_path <- file.path(output_dir, "bullpen-availability.csv")
active_bullpens <- data.frame()
active_bullpen_selector <- data.frame()
if (file.exists(availability_path)) {
  availability <- utils::read.csv(availability_path, stringsAsFactors = FALSE, check.names = FALSE)
  probable_ids <- unique(c(games$away_starter_id, games$home_starter_id))
  active_bullpens <- filter_active_roster_bullpen(availability, active_rosters, probable_ids)
}
selector_path <- file.path(output_dir, "bullpen-matchup-selector.csv")
if (file.exists(selector_path) && nrow(active_bullpens)) {
  selector <- utils::read.csv(selector_path, stringsAsFactors = FALSE, check.names = FALSE)
  active_bullpen_selector <- selector |>
    dplyr::mutate(pitcher_id = as.character(.data$pitcher_id)) |>
    dplyr::filter(.data$pitcher_id %in% unique(active_bullpens$pitcher_id))
  active_bullpen_selector$active_roster_verified <- TRUE
  active_bullpen_selector$roster_gate_date <- as.Date(target_date)
  active_bullpen_selector$selector_roster_method <- "baseballr_active_roster_plus_workload_selector_v1"
}

utils::write.csv(games, file.path(output_dir, "daily-game-inputs.csv"), row.names = FALSE, na = "")
utils::write.csv(inputs$lineups, file.path(output_dir, "daily-batting-orders.csv"), row.names = FALSE, na = "")
utils::write.csv(inputs$probables, file.path(output_dir, "daily-probable-starters.csv"), row.names = FALSE, na = "")
utils::write.csv(active_rosters, file.path(output_dir, "active-rosters.csv"), row.names = FALSE, na = "")
utils::write.csv(active_bullpens, file.path(output_dir, "active-roster-bullpens.csv"), row.names = FALSE, na = "")
utils::write.csv(active_bullpen_selector, file.path(output_dir, "active-roster-bullpen-selector.csv"), row.names = FALSE, na = "")
utils::write.csv(weather, file.path(output_dir, "daily-park-weather.csv"), row.names = FALSE, na = "")
write_slate_status("games_scheduled", nrow(games))
cat("Built", nrow(games), "daily games,", nrow(inputs$lineups), "posted lineup rows,", nrow(active_rosters), "active roster rows,", nrow(active_bullpens), "verified bullpen rows, and", nrow(active_bullpen_selector), "roster-gated selector rows.\n")
