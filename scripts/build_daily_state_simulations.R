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

starter_bf_estimate <- function(player_id) {
  row <- fangraphs_pitchers[
    as.character(fangraphs_pitchers$player_id) == as.character(player_id),
    ,
    drop = FALSE
  ]
  if (!nrow(row)) return(21)
  starts <- suppressWarnings(as.numeric(row$starts[[1L]]))
  batters_faced <- suppressWarnings(as.numeric(row$batters_faced[[1L]]))
  if (!is.finite(starts) || starts <= 0 || !is.finite(batters_faced)) return(21)
  pmin(pmax(batters_faced / starts, 15), 27)
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
  simulation <- simulate_game_state_phase4(
    away_starter_probs = away_starter,
    home_starter_probs = home_starter,
    away_bullpen_probs = away_bullpen,
    home_bullpen_probs = home_bullpen,
    away_reliever_probs = away_bullpen_input$individual,
    home_reliever_probs = home_bullpen_input$individual,
    n_sims = n_sims,
    away_starter_bf = starter_bf_estimate(game$away_starter_id[[1L]]),
    home_starter_bf = starter_bf_estimate(game$home_starter_id[[1L]]),
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
  model_version = "plate_appearance_state_machine_phase4_baserunning_v1",
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
  "Phase 4 simulated", nrow(game_output), "games x", n_sims,
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
