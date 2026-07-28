workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- file.path(workspace, "data", "derived")
read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing state-simulation input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

games <- read_product("daily-game-inputs.csv")
lineups <- read_product("daily-batting-orders.csv")
starter_probs <- read_product("daily-matchup-event-probabilities.csv")
bullpens <- read_product("active-roster-bullpens.csv")
hitters <- read_product("hitter-performance-summary.csv")
pitchers <- read_product("pitcher-performance-summary.csv")
hitter_platoon <- read_product("hitter-platoon-summary.csv")
pitcher_platoon <- read_product("pitcher-platoon-summary.csv")
hitter_form <- read_product("hitter-recent-form.csv")
pitcher_form <- read_product("pitcher-recent-form.csv")
fangraphs_pitchers <- read_product("fangraphs-season-pitchers.csv")
pitcher_game_lines <- read_product("current-season-pitcher-game-lines.csv")
weather <- read_product("daily-park-weather.csv")
hook_coefficients <- read_product("manager-hook-model.csv")
hook_validation <- read_product("manager-hook-validation-metrics.csv")
baserunning_league <- read_product("baserunning-league-rates.csv")
runner_profiles <- read_product("baserunning-runner-profiles.csv")
pitcher_hold_profiles <- read_product("baserunning-pitcher-hold-profiles.csv")
park_factors <- read_product("baserunning-park-factors.csv")

event_cols <- c("p_BB", "p_HBP", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
n_sims <- suppressWarnings(as.integer(Sys.getenv("SABRHOOD_STATE_N_SIMS", "1000")))
if (!is.finite(n_sims) || n_sims < 100L) n_sims <- 1000L

park_baserunning_profile <- function(venue_name) {
  empirical <- park_factors[
    tolower(as.character(park_factors$venue_name)) ==
      tolower(as.character(venue_name)),
    ,
    drop = FALSE
  ]
  if (nrow(empirical)) {
    multiplier <- function(metric) {
      row <- empirical[
        as.character(empirical$opportunity_type) == metric,
        ,
        drop = FALSE
      ]
      value <- suppressWarnings(as.numeric(row$empirical_multiplier))
      if (!length(value) || !is.finite(value[[1L]])) 1 else value[[1L]]
    }
    return(list(
      profile = list(
        single_second_score = multiplier("single_second_scores"),
        single_first_to_third = multiplier("single_first_to_third"),
        double_first_score = multiplier("double_first_scores"),
        steal_attempt = 1
      ),
      tier = "empirical_shrunk_2026",
      method = "descriptive_empirical_bayes_v1",
      reliability = mean(
        suppressWarnings(as.numeric(empirical$reliability)),
        na.rm = TRUE
      )
    ))
  }
  venue <- tolower(as.character(venue_name))
  expansive <- grepl(
    "coors|kauffman|comerica|oracle|chase|pnc|target",
    venue
  )
  quirky <- grepl("fenway|wrigley|camden|sutter health", venue)
  compact <- grepl(
    "yankee|citizens bank|great american|daikin|minute maid",
    venue
  )
  tier <- if (expansive) "expansive_outfield" else if (quirky) {
    "quirky_outfield"
  } else if (compact) {
    "compact_outfield"
  } else {
    "neutral_geometry"
  }
  values <- switch(
    tier,
    expansive_outfield = c(1.08, 1.10, 1.08, 1.03),
    quirky_outfield = c(1.04, 1.06, 1.04, 1.01),
    compact_outfield = c(0.97, 0.96, 0.97, 0.99),
    neutral_geometry = c(1, 1, 1, 1)
  )
  list(
    profile = list(
      single_second_score = unname(values[[1L]]),
      single_first_to_third = unname(values[[2L]]),
      double_first_score = unname(values[[3L]]),
      steal_attempt = unname(values[[4L]])
    ),
    tier = tier,
    method = "transparent_ballpark_geometry_heuristic_v1",
    reliability = 0
  )
}

league_baserunning_profile <- function() {
  rate <- function(metric, success_when_attempted = FALSE, fallback) {
    row <- baserunning_league[
      as.character(baserunning_league$opportunity_type) == metric,
      ,
      drop = FALSE
    ]
    column <- if (success_when_attempted) {
      "success_rate_when_attempted"
    } else {
      "modeled_rate"
    }
    value <- suppressWarnings(as.numeric(row[[column]]))
    if (!length(value) || !is.finite(value[[1L]])) fallback else value[[1L]]
  }
  list(
    single_second_scores = rate("single_second_scores", FALSE, 0.60),
    single_first_to_third = rate("single_first_to_third", FALSE, 0.30),
    double_first_scores = rate("double_first_scores", FALSE, 0.55),
    ground_ball_double_play = rate("ground_ball_double_play", FALSE, 0.115),
    sac_fly_scores = rate("sac_fly_scores", FALSE, 0.30),
    second_to_third_on_out = rate("second_to_third_on_out", FALSE, 0.20),
    first_to_second_on_out = rate("first_to_second_on_out", FALSE, 0.08),
    steal_second_attempt = rate("steal_second", FALSE, 0.04),
    steal_second_success = rate("steal_second", TRUE, 0.78),
    steal_third_attempt = rate("steal_third", FALSE, 0.012),
    steal_third_success = rate("steal_third", TRUE, 0.72)
  )
}

lineup_runner_profiles <- function(lineup) {
  runner_profiles[
    as.character(runner_profiles$runner_id) %in%
      as.character(lineup$player_id),
    ,
    drop = FALSE
  ]
}

pitcher_hold_multiplier <- function(team, starter_id) {
  rows <- pitcher_hold_profiles[
    as.character(pitcher_hold_profiles$fielding_team) == as.character(team) &
      as.character(pitcher_hold_profiles$opportunity_type) == "steal_second",
    ,
    drop = FALSE
  ]
  starter <- rows[
    as.character(rows$pitcher_id) == as.character(starter_id),
    ,
    drop = FALSE
  ]
  if (nrow(starter)) {
    value <- suppressWarnings(as.numeric(starter$attempt_suppression_index[[1L]])) / 100
    if (is.finite(value)) return(pmin(pmax(value, 0.75), 1.25))
  }
  if (nrow(rows)) {
    weights <- suppressWarnings(as.numeric(rows$runner_windows))
    values <- suppressWarnings(as.numeric(rows$attempt_suppression_index)) / 100
    valid <- is.finite(weights) & weights > 0 & is.finite(values)
    if (any(valid)) {
      return(pmin(pmax(stats::weighted.mean(values[valid], weights[valid]), 0.75), 1.25))
    }
  }
  1
}

num <- function(value) suppressWarnings(as.numeric(value))
clamp <- function(value, lower, upper) pmin(pmax(value, lower), upper)

hook_validation_passed <- nrow(hook_validation) > 0L &&
  num(hook_validation$validation_rows[[1L]]) >= 500 &&
  num(hook_validation$roc_auc[[1L]]) >= 0.65 &&
  is.finite(num(hook_validation$brier_score[[1L]]))
validated_hook_coefficients <- if (hook_validation_passed) {
  hook_coefficients[, c("term", "estimate"), drop = FALSE]
} else {
  NULL
}

starter_workload_profile <- function(player_id, game_date) {
  row <- fangraphs_pitchers[
    as.character(fangraphs_pitchers$player_id) == as.character(player_id),
    ,
    drop = FALSE
  ]
  season_bf <- 21
  if (nrow(row)) {
    starts <- num(row$starts[[1L]])
    batters_faced <- num(row$batters_faced[[1L]])
    if (is.finite(starts) && starts > 0 && is.finite(batters_faced)) {
      season_bf <- clamp(batters_faced / starts, 15, 28)
    }
  }

  recent <- pitcher_game_lines[
    as.character(pitcher_game_lines$player_id) == as.character(player_id) &
      as.Date(pitcher_game_lines$game_date) < as.Date(game_date),
    ,
    drop = FALSE
  ]
  recent$game_date_value <- as.Date(recent$game_date)
  recent <- recent[order(recent$game_date_value, decreasing = TRUE), , drop = FALSE]
  innings_outs <- num(recent$innings_outs)
  starter_like <- recent[
    is.finite(innings_outs) & innings_outs >= 6,
    ,
    drop = FALSE
  ]
  if (nrow(starter_like)) recent <- starter_like
  recent <- utils::head(recent, 5L)
  recent_bf <- if (nrow(recent)) {
    mean(num(recent$innings_outs) / 3 * 4.28, na.rm = TRUE)
  } else {
    NA_real_
  }
  reliability <- if (is.finite(recent_bf)) nrow(recent) / (nrow(recent) + 4) else 0
  expected_bf <- (1 - 0.35 * reliability) * season_bf +
    0.35 * reliability * ifelse(is.finite(recent_bf), recent_bf, season_bf)
  last_date <- if (nrow(recent)) max(recent$game_date_value, na.rm = TRUE) else as.Date(NA)
  rest_days <- as.numeric(as.Date(game_date) - last_date)
  rest_adjustment <- if (is.finite(rest_days) && rest_days <= 3) -1.5 else 0
  expected_bf <- clamp(expected_bf + rest_adjustment, 14, 28)
  pitch_limit <- clamp(expected_bf * 4.05, 68, 108)
  list(
    expected_bf = expected_bf,
    pitch_limit = pitch_limit,
    season_bf = season_bf,
    recent_bf = recent_bf,
    recent_games = nrow(recent),
    rest_days = rest_days,
    method = if (nrow(recent)) {
      "season_bf_plus_shrunk_recent_outs_v1"
    } else {
      "season_bf_fallback_v1"
    }
  )
}

game_environment_profile <- function(game) {
  game_id <- as.character(game$game_id[[1L]])
  row <- weather[as.character(weather$game_id) == game_id, , drop = FALSE]
  park_factor <- num(game$park_factor[[1L]])
  if (!is.finite(park_factor) || park_factor <= 0) park_factor <- 1
  if (!nrow(row)) {
    return(list(
      multiplier = clamp(park_factor, 0.82, 1.22),
      weather_multiplier = 1,
      temperature_f = NA_real_,
      wind_mph = NA_real_,
      status = "park_only_weather_missing"
    ))
  }
  indoors <- as.character(row$weather_status[[1L]]) == "indoors" ||
    as.character(row$roof_type[[1L]]) == "fixed_dome"
  temperature <- num(row$temperature_f[[1L]])
  temperature_effect <- if (indoors || !is.finite(temperature)) {
    0
  } else {
    clamp((temperature - 70) * 0.0015, -0.035, 0.045)
  }
  weather_multiplier <- 1 + temperature_effect
  list(
    multiplier = clamp(park_factor * weather_multiplier, 0.82, 1.22),
    weather_multiplier = weather_multiplier,
    temperature_f = temperature,
    wind_mph = num(row$wind_mph[[1L]]),
    status = if (indoors) "indoor_neutral" else "temperature_adjusted"
  )
}

candidate_relievers <- function(team, starter_id) {
  pool <- bullpens[
    as.character(bullpens$team) == as.character(team) &
      as.character(bullpens$pitcher_id) != as.character(starter_id),
    ,
    drop = FALSE
  ]
  if (!nrow(pool)) return(pool)
  score <- suppressWarnings(as.numeric(pool$availability_score))
  score[!is.finite(score)] <- 0.35
  pool$simulation_weight <- pmax(score, 0.05)

  reliever_flag <- tolower(as.character(pool$pitcher_role)) == "reliever"
  preferred <- pool[reliever_flag & pool$simulation_weight >= 0.20, , drop = FALSE]
  if (nrow(preferred) >= 3L) pool <- preferred
  pool <- pool[order(-pool$simulation_weight), , drop = FALSE]
  utils::head(pool, 6L)
}

build_bullpen_matchups <- function(offense_lineup, defense_team, starter_id) {
  candidates <- candidate_relievers(defense_team, starter_id)
  if (!nrow(candidates)) {
    fallback <- starter_probs[
      as.character(starter_probs$game_id) == as.character(offense_lineup$game_id[[1L]]) &
        as.character(starter_probs$team_side) == as.character(offense_lineup$team_side[[1L]]),
      ,
      drop = FALSE
    ]
    fallback$bullpen_pitcher_count <- 0L
    fallback$bullpen_mean_availability <- NA_real_
    fallback$selected_relief_pitchers <- "starter distribution fallback"
    fallback$bullpen_input_status <- "no_active_bullpen_candidates"
    return(list(aggregate = fallback, individual = data.frame()))
  }

  pair_rows <- lapply(seq_len(nrow(offense_lineup)), function(i) {
    hitter <- offense_lineup[i, , drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(candidates)), function(j) {
      reliever <- candidates[j, , drop = FALSE]
      data.frame(
        game_id = as.character(hitter$game_id[[1L]]),
        team_side = as.character(hitter$team_side[[1L]]),
        batting_order = as.integer(hitter$batting_order[[1L]]),
        batter_id = as.character(hitter$player_id[[1L]]),
        batter_name = as.character(hitter$player_name[[1L]]),
        batter_team = as.character(hitter$team_name[[1L]]),
        pitcher_id = as.character(reliever$pitcher_id[[1L]]),
        pitcher_name = as.character(reliever$pitcher_name[[1L]]),
        pitcher_team = as.character(defense_team),
        stringsAsFactors = FALSE
      )
    }))
  })
  pair_rows <- do.call(rbind, pair_rows)

  pair_probs <- build_matchup_event_probabilities(
    matchups = pair_rows,
    hitters = hitters,
    pitchers = pitchers,
    hitter_platoon = hitter_platoon,
    pitcher_platoon = pitcher_platoon,
    hitter_form = hitter_form,
    pitcher_form = pitcher_form,
    hitter_prior_pa = 200,
    pitcher_prior_pa = 250,
    split_prior_pa = 125,
    form_strength = 1
  )
  pair_probs$simulation_weight <- candidates$simulation_weight[
    match(as.character(pair_probs$pitcher_id), as.character(candidates$pitcher_id))
  ]
  pair_probs$simulation_weight[!is.finite(pair_probs$simulation_weight)] <- 0.05
  candidate_index <- match(as.character(pair_probs$pitcher_id), as.character(candidates$pitcher_id))
  pair_probs$throws <- as.character(candidates$throws[candidate_index])
  pair_probs$pitcher_role <- as.character(candidates$pitcher_role[candidate_index])
  pair_probs$availability_score <- suppressWarnings(as.numeric(candidates$availability_score[candidate_index]))

  batter_groups <- split(pair_probs, as.character(pair_probs$batter_id))
  rows <- lapply(batter_groups, function(group) {
    group <- group[order(-group$simulation_weight), , drop = FALSE]
    weights <- group$simulation_weight / sum(group$simulation_weight)
    output <- group[1L, c(
      "game_id", "team_side", "batting_order", "batter_id", "batter_name",
      "batter_team", "pitcher_team"
    ), drop = FALSE]
    for (column in event_cols) {
      output[[column]] <- sum(suppressWarnings(as.numeric(group[[column]])) * weights)
    }
    output$bullpen_pitcher_count <- nrow(group)
    output$bullpen_mean_availability <- sum(group$simulation_weight * weights)
    output$selected_relief_pitchers <- paste(unique(group$pitcher_name), collapse = " | ")
    output$bullpen_input_status <- "availability_weighted_active_relievers"
    output
  })
  result <- do.call(rbind, rows)
  result <- result[order(result$batting_order), , drop = FALSE]
  rownames(result) <- NULL
  list(aggregate = result, individual = as.data.frame(pair_probs))
}

game_outputs <- list()
hitter_outputs <- list()
bullpen_outputs <- list()
reliever_input_outputs <- list()
reliever_outputs <- list()
event_outputs <- list()
result_outputs <- list()

ready_games <- games[
  games$projection_ready %in% c(TRUE, "TRUE", 1, "1") &
    suppressWarnings(as.integer(games$away_lineup_count)) >= 9L &
    suppressWarnings(as.integer(games$home_lineup_count)) >= 9L,
  ,
  drop = FALSE
]
if (!nrow(ready_games)) stop("No games have complete Phase 3 inputs.", call. = FALSE)

for (index in seq_len(nrow(ready_games))) {
  game <- ready_games[index, , drop = FALSE]
  game_id <- as.character(game$game_id[[1L]])
  away_lineup <- lineups[
    as.character(lineups$game_id) == game_id & tolower(lineups$team_side) == "away",
    ,
    drop = FALSE
  ]
  home_lineup <- lineups[
    as.character(lineups$game_id) == game_id & tolower(lineups$team_side) == "home",
    ,
    drop = FALSE
  ]
  away_lineup <- away_lineup[order(away_lineup$batting_order), , drop = FALSE]
  home_lineup <- home_lineup[order(home_lineup$batting_order), , drop = FALSE]
  if (nrow(away_lineup) != 9L || nrow(home_lineup) != 9L) next

  away_starter <- starter_probs[
    as.character(starter_probs$game_id) == game_id & tolower(starter_probs$team_side) == "away",
    ,
    drop = FALSE
  ]
  home_starter <- starter_probs[
    as.character(starter_probs$game_id) == game_id & tolower(starter_probs$team_side) == "home",
    ,
    drop = FALSE
  ]
  away_starter <- away_starter[order(away_starter$batting_order), , drop = FALSE]
  home_starter <- home_starter[order(home_starter$batting_order), , drop = FALSE]
  if (nrow(away_starter) != 9L || nrow(home_starter) != 9L) next

  away_workload <- starter_workload_profile(
    game$away_starter_id[[1L]],
    game$game_date[[1L]]
  )
  home_workload <- starter_workload_profile(
    game$home_starter_id[[1L]],
    game$game_date[[1L]]
  )
  environment <- game_environment_profile(game)

  away_bullpen_input <- build_bullpen_matchups(
    away_lineup,
    as.character(game$home_team[[1L]]),
    as.character(game$home_starter_id[[1L]])
  )
  home_bullpen_input <- build_bullpen_matchups(
    home_lineup,
    as.character(game$away_team[[1L]]),
    as.character(game$away_starter_id[[1L]])
  )
  away_bullpen <- away_bullpen_input$aggregate
  home_bullpen <- home_bullpen_input$aggregate
  bullpen_outputs[[length(bullpen_outputs) + 1L]] <- away_bullpen
  bullpen_outputs[[length(bullpen_outputs) + 1L]] <- home_bullpen
  if (nrow(away_bullpen_input$individual)) {
    reliever_input_outputs[[length(reliever_input_outputs) + 1L]] <- away_bullpen_input$individual
  }
  if (nrow(home_bullpen_input$individual)) {
    reliever_input_outputs[[length(reliever_input_outputs) + 1L]] <- home_bullpen_input$individual
  }

  park_profile <- park_baserunning_profile(game$venue_name[[1L]])
  simulation <- simulate_game_state_phase5(
    away_starter_probs = away_starter,
    home_starter_probs = home_starter,
    away_bullpen_probs = away_bullpen,
    home_bullpen_probs = home_bullpen,
    away_reliever_probs = away_bullpen_input$individual,
    home_reliever_probs = home_bullpen_input$individual,
    n_sims = n_sims,
    away_starter_bf = away_workload$expected_bf,
    home_starter_bf = home_workload$expected_bf,
    away_starter_pitch_limit = away_workload$pitch_limit,
    home_starter_pitch_limit = home_workload$pitch_limit,
    manager_hook_coefficients = validated_hook_coefficients,
    run_environment_multiplier = environment$multiplier,
    seed = as.integer(abs(as.numeric(game_id)) %% .Machine$integer.max),
    park_baserunning = park_profile$profile,
    baserunning_rates = league_baserunning_profile(),
    away_runner_profiles = lineup_runner_profiles(away_lineup),
    home_runner_profiles = lineup_runner_profiles(home_lineup),
    defense_steal_multipliers = c(
      away = pitcher_hold_multiplier(
        as.character(game$away_team[[1L]]),
        as.character(game$away_starter_id[[1L]])
      ),
      home = pitcher_hold_multiplier(
        as.character(game$home_team[[1L]]),
        as.character(game$home_starter_id[[1L]])
      )
    )
  )

  summary <- cbind(
    game[, c(
      "game_id", "game_date", "game_time_utc", "away_team", "home_team",
      "away_starter_id", "away_starter_name", "home_starter_id", "home_starter_name",
      "lineups_ready", "weather_ready", "rosters_ready"
    ), drop = FALSE],
    simulation$game_summary
  )
  calibrator_path <- file.path(workspace, ".private-data", "models", "state-engine-calibrator.rds")
  summary$calibrated_home_win_probability <- NA_real_
  summary$calibrated_away_win_probability <- NA_real_
  summary$calibrated_away_mean_runs <- NA_real_
  summary$calibrated_home_mean_runs <- NA_real_
  summary$calibration_applied <- FALSE
  if (file.exists(calibrator_path)) {
    calibrator <- tryCatch(readRDS(calibrator_path), error = function(error) NULL)
    if (is.list(calibrator) &&
        all(c(
          "probability_model", "away_run_model", "home_run_model", "model_version"
        ) %in% names(calibrator)) &&
        identical(
          as.character(calibrator$model_version),
          as.character(summary$model_version[[1L]])
        )) {
      calibrated_home <- predict_calibrated_probability(
        calibrator$probability_model,
        summary$home_win_probability
      )
      summary$calibrated_home_win_probability <- calibrated_home
      summary$calibrated_away_win_probability <- 1 - calibrated_home
      summary$calibrated_away_mean_runs <- pmax(
        as.numeric(stats::predict(calibrator$away_run_model, newdata = summary)),
        0
      )
      summary$calibrated_home_mean_runs <- pmax(
        as.numeric(stats::predict(calibrator$home_run_model, newdata = summary)),
        0
      )
      summary$calibration_applied <- TRUE
    }
  }
  summary$publication_status <- "shadow development; current public probabilities remain unchanged"
  summary$away_starter_expected_bf_input <- away_workload$expected_bf
  summary$home_starter_expected_bf_input <- home_workload$expected_bf
  summary$away_starter_pitch_limit_input <- away_workload$pitch_limit
  summary$home_starter_pitch_limit_input <- home_workload$pitch_limit
  summary$away_starter_recent_bf <- away_workload$recent_bf
  summary$home_starter_recent_bf <- home_workload$recent_bf
  summary$away_starter_recent_games <- away_workload$recent_games
  summary$home_starter_recent_games <- home_workload$recent_games
  summary$away_starter_rest_days <- away_workload$rest_days
  summary$home_starter_rest_days <- home_workload$rest_days
  summary$away_starter_workload_method <- away_workload$method
  summary$home_starter_workload_method <- home_workload$method
  summary$weather_run_multiplier <- environment$weather_multiplier
  summary$game_run_environment_multiplier <- environment$multiplier
  summary$weather_temperature_f <- environment$temperature_f
  summary$weather_wind_mph <- environment$wind_mph
  summary$run_environment_status <- environment$status
  summary$manager_hook_validation_passed <- hook_validation_passed
  summary$park_baserunning_tier <- park_profile$tier
  summary$park_baserunning_method <- park_profile$method
  summary$park_baserunning_reliability <- park_profile$reliability
  summary$away_defense_steal_multiplier <- pitcher_hold_multiplier(
    as.character(game$away_team[[1L]]),
    as.character(game$away_starter_id[[1L]])
  )
  summary$home_defense_steal_multiplier <- pitcher_hold_multiplier(
    as.character(game$home_team[[1L]]),
    as.character(game$home_starter_id[[1L]])
  )
  summary$away_runner_profile_coverage <- mean(
    as.character(away_lineup$player_id) %in%
      as.character(runner_profiles$runner_id)
  )
  summary$home_runner_profile_coverage <- mean(
    as.character(home_lineup$player_id) %in%
      as.character(runner_profiles$runner_id)
  )
  summary$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  game_outputs[[length(game_outputs) + 1L]] <- summary

  hitters_game <- simulation$hitter_summary
  hitters_game$game_id <- game_id
  hitters_game$game_date <- as.character(game$game_date[[1L]])
  hitters_game$team <- ifelse(
    hitters_game$team_side == "away",
    as.character(game$away_team[[1L]]),
    as.character(game$home_team[[1L]])
  )
  hitters_game$opponent <- ifelse(
    hitters_game$team_side == "away",
    as.character(game$home_team[[1L]]),
    as.character(game$away_team[[1L]])
  )
  hitters_game$model_version <- as.character(simulation$game_summary$model_version[[1L]])
  hitters_game$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  hitter_outputs[[length(hitter_outputs) + 1L]] <- hitters_game

  relievers_game <- simulation$reliever_summary
  if (nrow(relievers_game)) {
    relievers_game$game_id <- game_id
    relievers_game$game_date <- as.character(game$game_date[[1L]])
    relievers_game$team <- ifelse(
      relievers_game$defense_side == "away",
      as.character(game$away_team[[1L]]),
      as.character(game$home_team[[1L]])
    )
    relievers_game$opponent <- ifelse(
      relievers_game$defense_side == "away",
      as.character(game$home_team[[1L]]),
      as.character(game$away_team[[1L]])
    )
    relievers_game$model_version <- as.character(simulation$game_summary$model_version[[1L]])
    relievers_game$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    reliever_outputs[[length(reliever_outputs) + 1L]] <- relievers_game
  }
  events_game <- simulation$event_summary
  events_game$game_id <- game_id
  events_game$game_date <- as.character(game$game_date[[1L]])
  event_outputs[[length(event_outputs) + 1L]] <- events_game

  results <- simulation$game_results
  results$game_id <- game_id
  result_outputs[[length(result_outputs) + 1L]] <- results
}

game_output <- do.call(rbind, game_outputs)
hitter_output <- do.call(rbind, hitter_outputs)
bullpen_output <- do.call(rbind, bullpen_outputs)
reliever_input_output <- if (length(reliever_input_outputs)) do.call(rbind, reliever_input_outputs) else data.frame()
reliever_output <- if (length(reliever_outputs)) do.call(rbind, reliever_outputs) else data.frame()
event_output <- if (length(event_outputs)) do.call(rbind, event_outputs) else data.frame()
simulation_results <- do.call(rbind, result_outputs)
if (is.null(game_output) || !nrow(game_output)) stop("Phase 3 produced no games.", call. = FALSE)

model_card <- data.frame(
  game_date = as.character(ready_games$game_date[[1L]]),
  games_simulated = nrow(game_output),
  simulations_per_game = n_sims,
  hitter_rows = nrow(hitter_output),
  bullpen_input_rows = nrow(bullpen_output),
  active_reliever_match_rate = mean(bullpen_output$bullpen_pitcher_count > 0),
  mean_relievers_per_bullpen_matchup = mean(bullpen_output$bullpen_pitcher_count),
  mean_bullpen_pa_share = mean(game_output$bullpen_pa_share),
  mean_extra_innings_probability = mean(game_output$extra_innings_probability),
  mean_double_plays = mean(game_output$mean_double_plays),
  mean_productive_out_runs = mean(game_output$mean_productive_out_runs),
  mean_steal_attempts = mean(game_output$mean_steal_attempts),
  mean_stolen_bases = mean(game_output$mean_stolen_bases),
  mean_relievers_used = mean(game_output$mean_relievers_used),
  maximum_tie_probability = max(game_output$tie_probability),
  mean_runner_profile_coverage = mean(c(
    game_output$away_runner_profile_coverage,
    game_output$home_runner_profile_coverage
  )),
  empirical_park_match_rate = mean(
    game_output$park_baserunning_method == "descriptive_empirical_bayes_v1"
  ),
  manager_hook_validation_passed = hook_validation_passed,
  manager_hook_validation_rows = if (nrow(hook_validation)) {
    num(hook_validation$validation_rows[[1L]])
  } else {
    0
  },
  manager_hook_roc_auc = if (nrow(hook_validation)) {
    num(hook_validation$roc_auc[[1L]])
  } else {
    NA_real_
  },
  mean_away_starter_expected_bf = mean(game_output$away_starter_expected_bf_input),
  mean_home_starter_expected_bf = mean(game_output$home_starter_expected_bf_input),
  mean_run_environment_multiplier = mean(game_output$game_run_environment_multiplier),
  model_version = as.character(game_output$model_version[[1L]]),
  publication_status = "shadow development; current public probabilities remain unchanged",
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

utils::write.csv(
  game_output,
  file.path(output_dir, "daily-state-simulation-games.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  hitter_output,
  file.path(output_dir, "daily-state-simulation-hitters.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  bullpen_output,
  file.path(output_dir, "daily-state-simulation-bullpen-inputs.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  model_card,
  file.path(output_dir, "daily-state-simulation-model-card.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  reliever_input_output,
  file.path(output_dir, "daily-state-simulation-reliever-inputs.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  reliever_output,
  file.path(output_dir, "daily-state-simulation-relievers.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  event_output,
  file.path(output_dir, "daily-state-simulation-events.csv"),
  row.names = FALSE,
  na = ""
)

if (identical(tolower(Sys.getenv("SABRHOOD_KEEP_STATE_DRAWS", "false")), "true")) {
  utils::write.csv(
    simulation_results,
    file.path(output_dir, "daily-state-simulation-draws.csv"),
    row.names = FALSE,
    na = ""
  )
}

cat(
  "Phase 5 simulated", nrow(game_output), "games x", n_sims,
  "draws with", nrow(hitter_output), "player projections.\n"
)
cat(
  "Mean bullpen PA share:",
  sprintf("%.1f%%", 100 * model_card$mean_bullpen_pa_share),
  "| named relievers used per game:",
  sprintf("%.1f", model_card$mean_relievers_used),
  "| maximum unresolved tie probability:",
  sprintf("%.2f%%", 100 * model_card$maximum_tie_probability), "\n"
)
