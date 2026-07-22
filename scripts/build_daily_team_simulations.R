workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- file.path(workspace, "data", "derived")
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing projection input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
num <- function(value) suppressWarnings(as.numeric(value))
weighted_mean_safe <- function(value, weight, fallback) {
  value <- num(value); weight <- num(weight)
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(keep)) return(fallback)
  stats::weighted.mean(value[keep], weight[keep])
}
clamp <- function(value, lower, upper) pmin(pmax(value, lower), upper)

games <- read_product("daily-game-inputs.csv")
lineups <- read_product("daily-batting-orders.csv")
hitters <- read_product("fangraphs-season-hitters.csv")
pitchers <- read_product("fangraphs-season-pitchers.csv")
weather <- read_product("daily-park-weather.csv")
active_rosters <- read_product("active-rosters.csv")
bullpen_usage <- read_product("active-roster-bullpens.csv")

team_to_abbr <- c(
  "Arizona Diamondbacks" = "ARI", "Athletics" = "ATH", "Atlanta Braves" = "ATL",
  "Baltimore Orioles" = "BAL", "Boston Red Sox" = "BOS", "Chicago Cubs" = "CHC",
  "Chicago White Sox" = "CHW", "Cincinnati Reds" = "CIN", "Cleveland Guardians" = "CLE",
  "Colorado Rockies" = "COL", "Detroit Tigers" = "DET", "Houston Astros" = "HOU",
  "Kansas City Royals" = "KCR", "Los Angeles Angels" = "LAA", "Los Angeles Dodgers" = "LAD",
  "Miami Marlins" = "MIA", "Milwaukee Brewers" = "MIL", "Minnesota Twins" = "MIN",
  "New York Mets" = "NYM", "New York Yankees" = "NYY", "Philadelphia Phillies" = "PHI",
  "Pittsburgh Pirates" = "PIT", "San Diego Padres" = "SDP", "San Francisco Giants" = "SFG",
  "Seattle Mariners" = "SEA", "St. Louis Cardinals" = "STL", "Tampa Bay Rays" = "TBR",
  "Texas Rangers" = "TEX", "Toronto Blue Jays" = "TOR", "Washington Nationals" = "WSN"
)

hitters$team <- as.character(hitters$team)
pitchers$team <- as.character(pitchers$team)
hitters$player_id <- as.character(hitters$player_id)
pitchers$player_id <- as.character(pitchers$player_id)
lineups$player_id <- as.character(lineups$player_id)
lineups$game_id <- as.character(lineups$game_id)
games$game_id <- as.character(games$game_id)

league_wrc <- weighted_mean_safe(hitters$wrc_plus, hitters$pa, 100)
league_woba <- weighted_mean_safe(hitters$woba, hitters$pa, 0.320)
league_fip <- weighted_mean_safe(pitchers$fip, pitchers$innings_outs, 4.20)
league_xfip <- weighted_mean_safe(pitchers$xfip, pitchers$innings_outs, league_fip)
league_era <- weighted_mean_safe(pitchers$era, pitchers$innings_outs, 4.20)
league_pitching_index <- 1
league_runs <- num(Sys.getenv("SABRHOOD_RUN_ENVIRONMENT", unset = "4.40"))
if (!is.finite(league_runs) || league_runs <= 0) league_runs <- 4.40
n_sims <- as.integer(Sys.getenv("SABRHOOD_N_SIMS", unset = "20000"))
if (!is.finite(n_sims) || n_sims < 1000L) n_sims <- 20000L

active_pitcher_ids <- unique(as.character(active_rosters$player_id[
  as.logical(active_rosters$is_active) & as.logical(active_rosters$is_pitcher)
]))

team_hitting <- lapply(unique(unname(team_to_abbr)), function(team) {
  rows <- hitters[hitters$team == team & num(hitters$pa) > 0, , drop = FALSE]
  data.frame(
    team = team,
    wrc_plus = weighted_mean_safe(rows$wrc_plus, rows$pa, league_wrc),
    woba = weighted_mean_safe(rows$woba, rows$pa, league_woba),
    pa = sum(num(rows$pa), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
team_hitting <- do.call(rbind, team_hitting)

bullpen_rows <- pitchers[
  pitchers$player_id %in% active_pitcher_ids &
    num(pitchers$games) >= 3 &
    (num(pitchers$starts) / pmax(num(pitchers$games), 1)) < 0.50,
  , drop = FALSE
]
team_bullpen <- lapply(unique(unname(team_to_abbr)), function(team) {
  rows <- bullpen_rows[bullpen_rows$team == team, , drop = FALSE]
  composite <- 0.55 * num(rows$fip) + 0.30 * num(rows$xfip) + 0.15 * num(rows$era)
  data.frame(
    team = team,
    bullpen_pitchers = nrow(rows),
    bullpen_fip = weighted_mean_safe(composite, rows$innings_outs, league_fip),
    bullpen_war = sum(num(rows$war), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
team_bullpen <- do.call(rbind, team_bullpen)

usage_dates <- suppressWarnings(as.Date(bullpen_usage$as_of_date))
game_date <- suppressWarnings(as.Date(games$game_date[[1L]]))
usage_age_days <- if (length(usage_dates) && any(!is.na(usage_dates))) {
  as.numeric(game_date - max(usage_dates, na.rm = TRUE))
} else {
  Inf
}
usage_is_current <- is.finite(usage_age_days) && usage_age_days <= 1

lineup_profile <- function(game_id, team_name, team_abbr) {
  rows <- lineups[lineups$game_id == game_id & lineups$team_name == team_name, , drop = FALSE]
  team_row <- team_hitting[team_hitting$team == team_abbr, , drop = FALSE]
  team_wrc <- if (nrow(team_row)) num(team_row$wrc_plus[[1L]]) else league_wrc
  if (!nrow(rows)) {
    return(list(wrc_plus = team_wrc, count = 0L, status = "missing_team_fallback", matched = 0L))
  }
  match_index <- match(rows$player_id, hitters$player_id)
  player_wrc <- num(hitters$wrc_plus[match_index])
  player_pa <- num(hitters$pa[match_index])
  player_wrc[!is.finite(player_wrc) | player_pa < 20] <- team_wrc
  slot <- pmin(pmax(as.integer(rows$batting_order), 1L), 9L)
  slot_weight <- c(1.09, 1.06, 1.04, 1.02, 1.00, 0.97, 0.94, 0.91, 0.88)[slot]
  lineup_wrc <- stats::weighted.mean(player_wrc, slot_weight, na.rm = TRUE)
  blended <- 0.70 * lineup_wrc + 0.30 * team_wrc
  list(
    wrc_plus = blended,
    count = nrow(rows),
    status = if (nrow(rows) >= 9L) "confirmed" else "partial",
    matched = sum(!is.na(match_index))
  )
}

starter_profile <- function(player_id) {
  row <- pitchers[pitchers$player_id == as.character(player_id), , drop = FALSE]
  if (!nrow(row)) {
    return(list(name = "League-average fallback", fip = league_fip, innings = 5.0, index = league_pitching_index, matched = FALSE))
  }
  row <- row[order(-num(row$innings_outs)), , drop = FALSE][1L, , drop = FALSE]
  composite <- 0.50 * num(row$fip[[1L]]) + 0.30 * num(row$xfip[[1L]]) + 0.20 * num(row$era[[1L]])
  if (!is.finite(composite)) composite <- league_fip
  starts <- num(row$starts[[1L]])
  innings <- if (is.finite(starts) && starts > 0) num(row$innings_outs[[1L]]) / 3 / starts else 5.0
  innings <- clamp(innings, 3.8, 6.5)
  list(name = row$player_name[[1L]], fip = composite, innings = innings, index = clamp(composite / league_fip, 0.58, 1.55), matched = TRUE)
}

bullpen_profile <- function(team_abbr) {
  row <- team_bullpen[team_bullpen$team == team_abbr, , drop = FALSE]
  if (!nrow(row)) return(list(fip = league_fip, index = 1, pitchers = 0L, availability = NA_real_))
  availability <- NA_real_
  if (usage_is_current) {
    full_name <- names(team_to_abbr)[match(team_abbr, team_to_abbr)]
    usage <- bullpen_usage[bullpen_usage$team == full_name, , drop = FALSE]
    availability <- mean(num(usage$availability_score), na.rm = TRUE)
    if (!is.finite(availability)) availability <- NA_real_
  }
  availability_penalty <- if (is.finite(availability)) clamp((0.80 - availability) * 0.18, -0.025, 0.07) else 0
  index <- clamp(num(row$bullpen_fip[[1L]]) / league_fip + availability_penalty, 0.65, 1.45)
  list(fip = num(row$bullpen_fip[[1L]]), index = index, pitchers = as.integer(row$bullpen_pitchers[[1L]]), availability = availability)
}

weather_profile <- function(game_id) {
  row <- weather[as.character(weather$game_id) == game_id, , drop = FALSE]
  if (!nrow(row)) return(list(multiplier = 1, label = "weather unavailable", resolved = FALSE))
  indoors <- row$weather_status[[1L]] == "indoors" || row$roof_type[[1L]] == "fixed_dome"
  temperature <- num(row$temperature_f[[1L]])
  temperature_effect <- if (indoors || !is.finite(temperature)) 0 else clamp((temperature - 70) * 0.0015, -0.035, 0.045)
  list(
    multiplier = 1 + temperature_effect,
    label = if (indoors) "indoor neutral" else if (is.finite(temperature)) paste0(round(temperature), " F temperature adjustment") else "neutral weather fallback",
    resolved = row$weather_status[[1L]] %in% c("available", "indoors", "not_required")
  )
}

simulation_rows <- list()
margin_rows <- list()
score_rows <- list()
driver_rows <- list()
component_rows <- list()

for (index in seq_len(nrow(games))) {
  game <- games[index, , drop = FALSE]
  game_id <- as.character(game$game_id[[1L]])
  away_abbr <- unname(team_to_abbr[game$away_team[[1L]]])
  home_abbr <- unname(team_to_abbr[game$home_team[[1L]]])
  away_lineup <- lineup_profile(game_id, game$away_team[[1L]], away_abbr)
  home_lineup <- lineup_profile(game_id, game$home_team[[1L]], home_abbr)
  away_starter <- starter_profile(game$away_starter_id[[1L]])
  home_starter <- starter_profile(game$home_starter_id[[1L]])
  away_bullpen <- bullpen_profile(away_abbr)
  home_bullpen <- bullpen_profile(home_abbr)
  environment <- weather_profile(game_id)

  home_starter_share <- home_starter$innings / 9
  away_starter_share <- away_starter$innings / 9
  home_pitching_index <- home_starter_share * home_starter$index + (1 - home_starter_share) * home_bullpen$index
  away_pitching_index <- away_starter_share * away_starter$index + (1 - away_starter_share) * away_bullpen$index
  away_offense_index <- clamp(away_lineup$wrc_plus / league_wrc, 0.70, 1.35)
  home_offense_index <- clamp(home_lineup$wrc_plus / league_wrc, 0.70, 1.35)
  park_factor <- num(game$park_factor[[1L]])
  if (!is.finite(park_factor) || park_factor <= 0) park_factor <- 1
  away_expected <- clamp(league_runs * away_offense_index * home_pitching_index * park_factor * environment$multiplier, 2.30, 7.00)
  home_expected <- clamp(league_runs * home_offense_index * away_pitching_index * park_factor * environment$multiplier * 1.025, 2.30, 7.00)

  seed <- 260721L + suppressWarnings(as.integer(game_id) %% 100000L)
  if (!is.finite(seed)) seed <- 260721L + index
  result <- simulate_game_projection(
    away_expected_runs = away_expected,
    home_expected_runs = home_expected,
    n_sims = n_sims,
    seed = seed,
    away_team = game$away_team[[1L]],
    home_team = game$home_team[[1L]]
  )
  summary <- as.data.frame(result$projection_summary, stringsAsFactors = FALSE)
  summary$game_id <- game_id
  summary$game_date <- game$game_date[[1L]]
  summary$game_time_utc <- game$game_time_utc[[1L]]
  summary$display_order <- index
  summary$winner_probability <- pmax(summary$away_win_probability, summary$home_win_probability)
  summary$feature_game <- FALSE
  summary$input_completeness <- num(game$input_completeness[[1L]])
  summary$lineup_status <- if (away_lineup$status == "confirmed" && home_lineup$status == "confirmed") "confirmed" else "conditional"
  summary$starter_status <- if (away_starter$matched && home_starter$matched) "matched" else "fallback"
  summary$bullpen_status <- if (usage_is_current) "quality_and_current_usage" else "active_roster_quality_usage_stale"
  summary$park_status <- if (grepl("placeholder", game$park_factor_note[[1L]], fixed = TRUE)) "neutral_placeholder" else "modeled"
  summary$weather_status <- game$weather_status[[1L]]
  summary$model_version <- "scheduled_game_simulator_development_v1"
  summary$publication_status <- "development_uncalibrated"
  summary$input_status <- paste(summary$lineup_status, summary$starter_status, summary$bullpen_status, summary$park_status, sep = "; ")

  neutral_bullpen_away <- clamp(league_runs * away_offense_index * (home_starter_share * home_starter$index + (1 - home_starter_share)) * park_factor * environment$multiplier, 2.30, 7.00)
  neutral_bullpen_home <- clamp(league_runs * home_offense_index * (away_starter_share * away_starter$index + (1 - away_starter_share)) * park_factor * environment$multiplier * 1.025, 2.30, 7.00)
  neutral <- simulate_game_projection(neutral_bullpen_away, neutral_bullpen_home, n_sims = 5000L,
    seed = seed + 900000L, away_team = game$away_team[[1L]], home_team = game$home_team[[1L]])
  summary$home_win_probability_without_bullpen <- neutral$projection_summary$home_win_probability
  summary$bullpen_home_win_swing <- summary$home_win_probability - summary$home_win_probability_without_bullpen
  simulation_rows[[index]] <- summary

  margins <- as.data.frame(result$margin_distribution, stringsAsFactors = FALSE)
  margins$game_id <- game_id
  margins$display_order <- seq_len(nrow(margins))
  margin_rows[[index]] <- margins[, c("game_id", "display_order", "margin_group", "simulations", "probability")]
  scores <- utils::head(as.data.frame(result$score_distribution, stringsAsFactors = FALSE), 5L)
  scores$game_id <- game_id
  scores$score_rank <- seq_len(nrow(scores))
  score_rows[[index]] <- scores[, c("game_id", "score_rank", "away_runs", "home_runs", "simulations", "probability")]

  driver_rows[[length(driver_rows) + 1L]] <- data.frame(game_id = game_id, driver_order = 1L, driver_label = "Posted-lineup offense", advantage_team = if (away_offense_index >= home_offense_index) game$away_team[[1L]] else game$home_team[[1L]], driver_detail = paste0(round(away_lineup$wrc_plus), " vs ", round(home_lineup$wrc_plus), " blended wRC+"))
  driver_rows[[length(driver_rows) + 1L]] <- data.frame(game_id = game_id, driver_order = 2L, driver_label = "Probable starter", advantage_team = if (away_starter$index <= home_starter$index) game$away_team[[1L]] else game$home_team[[1L]], driver_detail = paste0(game$away_starter_name[[1L]], " ", round(away_starter$fip, 2), " vs ", game$home_starter_name[[1L]], " ", round(home_starter$fip, 2), " run estimator"))
  driver_rows[[length(driver_rows) + 1L]] <- data.frame(game_id = game_id, driver_order = 3L, driver_label = "Active-roster bullpen", advantage_team = if (away_bullpen$index <= home_bullpen$index) game$away_team[[1L]] else game$home_team[[1L]], driver_detail = paste0(round(away_bullpen$fip, 2), " vs ", round(home_bullpen$fip, 2), " roster-weighted FIP blend"))
  driver_rows[[length(driver_rows) + 1L]] <- data.frame(game_id = game_id, driver_order = 4L, driver_label = "Park and weather", advantage_team = "Run environment", driver_detail = paste0(environment$label, "; park factor ", round(park_factor, 3)))

  component_rows[[index]] <- data.frame(
    game_id = game_id, game_date = game$game_date[[1L]], away_team = game$away_team[[1L]], home_team = game$home_team[[1L]],
    away_lineup_wrc_plus = away_lineup$wrc_plus, home_lineup_wrc_plus = home_lineup$wrc_plus,
    away_lineup_count = away_lineup$count, home_lineup_count = home_lineup$count,
    away_starter_fip_blend = away_starter$fip, home_starter_fip_blend = home_starter$fip,
    away_starter_expected_innings = away_starter$innings, home_starter_expected_innings = home_starter$innings,
    away_bullpen_fip_blend = away_bullpen$fip, home_bullpen_fip_blend = home_bullpen$fip,
    away_active_relievers = away_bullpen$pitchers, home_active_relievers = home_bullpen$pitchers,
    weather_multiplier = environment$multiplier, park_factor = park_factor,
    away_expected_runs = away_expected, home_expected_runs = home_expected,
    usage_snapshot_age_days = usage_age_days, usage_current = usage_is_current,
    stringsAsFactors = FALSE
  )
}

projections <- do.call(rbind, simulation_rows)
projections <- projections[order(projections$game_time_utc), , drop = FALSE]
projections$display_order <- seq_len(nrow(projections))
feature_index <- which.min(abs(projections$winner_probability - 0.5))
projections$feature_game[feature_index] <- TRUE
margins <- do.call(rbind, margin_rows)
scores <- do.call(rbind, score_rows)
drivers <- do.call(rbind, driver_rows)
components <- do.call(rbind, component_rows)

readiness <- data.frame(
  game_date = game_date,
  games = nrow(games),
  games_with_confirmed_lineups = sum(projections$lineup_status == "confirmed"),
  games_with_matched_starters = sum(projections$starter_status == "matched"),
  games_with_weather = sum(projections$weather_status %in% c("available", "indoors", "not_required")),
  active_roster_bullpen_model = TRUE,
  bullpen_usage_current = usage_is_current,
  bullpen_usage_age_days = usage_age_days,
  empirical_park_factors = all(projections$park_status == "modeled"),
  chronological_calibration_complete = FALSE,
  publication_approved = FALSE,
  publication_status = "development only; chronological calibration, current PBP usage, and empirical park factors required",
  model_version = "scheduled_game_simulator_development_v1",
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

utils::write.csv(projections, file.path(output_dir, "daily-projections-live.csv"), row.names = FALSE, na = "")
utils::write.csv(margins, file.path(output_dir, "daily-projection-margin-live.csv"), row.names = FALSE, na = "")
utils::write.csv(scores, file.path(output_dir, "daily-projection-scorelines-live.csv"), row.names = FALSE, na = "")
utils::write.csv(drivers, file.path(output_dir, "daily-projection-drivers-live.csv"), row.names = FALSE, na = "")
utils::write.csv(components, file.path(output_dir, "daily-projection-components-live.csv"), row.names = FALSE, na = "")
utils::write.csv(readiness, file.path(output_dir, "projection-publication-readiness.csv"), row.names = FALSE, na = "")

cat("Simulated", nrow(projections), "scheduled games with", n_sims, "draws per game.\n")
cat("Confirmed lineups:", readiness$games_with_confirmed_lineups, "/", readiness$games, "; matched starters:", readiness$games_with_matched_starters, "/", readiness$games, ".\n")
cat("Publication status:", readiness$publication_status, "\n")
