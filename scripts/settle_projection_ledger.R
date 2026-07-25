workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(sabrhoodR))

output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path(workspace, "data", "derived"))
ledger_dir <- Sys.getenv("SABRHOOD_LEDGER_DIR", unset = file.path(workspace, ".private-data", "projection-ledger"))
private_model_dir <- Sys.getenv("SABRHOOD_MODEL_DIR", unset = file.path(workspace, ".private-data", "models"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(private_model_dir, recursive = TRUE, showWarnings = FALSE)

snapshot_files <- list.files(file.path(ledger_dir, "snapshots"), pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
if (!length(snapshot_files)) {
  game_metrics <- data.frame(
    settled_games = 0L, eligible_unsettled_games = 0L, calibration_minimum_games = 300L,
    calibration_status = "awaiting first pregame snapshot", brier_score = NA_real_,
    log_loss = NA_real_, classification_accuracy = NA_real_, calibration_bias = NA_real_,
    away_runs_mae = NA_real_, home_runs_mae = NA_real_, total_runs_mae = NA_real_,
    model_version = "none eligible yet", stringsAsFactors = FALSE
  )
  game_calibration <- data.frame(
    probability_bin = NA_integer_, bin_lower = NA_real_, bin_upper = NA_real_, observations = 0L,
    mean_probability = NA_real_, observed_rate = NA_real_, calibration_gap = NA_real_,
    calibration_status = "awaiting first pregame snapshot", stringsAsFactors = FALSE
  )
  player_metrics <- data.frame(
    metric_id = "awaiting_eligible_forecasts", metric_label = "Player-event calibration has not started",
    settled_predictions = 0L, mean_probability = NA_real_, observed_rate = NA_real_,
    brier_score = NA_real_, calibration_bias = NA_real_,
    calibration_status = "awaiting first pregame snapshot", stringsAsFactors = FALSE
  )
  matchup_metrics <- data.frame(
    eligible_matchups = 0L, settled_matchups = 0L, observed_plate_appearances = 0L,
    multiclass_log_loss = NA_real_, multiclass_brier_score = NA_real_,
    predicted_woba = NA_real_, observed_woba = NA_real_,
    calibration_status = "awaiting first pregame matchup snapshot",
    model_version = "none eligible yet", stringsAsFactors = FALSE
  )
  matchup_event_metrics <- data.frame(
    event = c("BB", "HBP", "K", "X1B", "X2B", "X3B", "HR", "OUT"),
    observations = 0L, mean_probability = NA_real_, observed_rate = NA_real_,
    brier_score = NA_real_, calibration_bias = NA_real_,
    calibration_status = "awaiting first pregame matchup snapshot",
    stringsAsFactors = FALSE
  )
  state_metrics <- data.frame(
    eligible_game_forecasts = 0L, settled_games = 0L,
    brier_score = NA_real_, away_runs_mae = NA_real_,
    home_runs_mae = NA_real_, total_runs_mae = NA_real_,
    mean_tie_probability = NA_real_,
    calibration_status = "awaiting first pregame game-state snapshot",
    model_version = "none eligible yet", stringsAsFactors = FALSE
  )
  state_calibration_status <- data.frame(
    settled_games = 0L, training_games = 0L, holdout_games = 0L,
    raw_holdout_brier = NA_real_, calibrated_holdout_brier = NA_real_,
    raw_holdout_runs_mae = NA_real_, calibrated_holdout_runs_mae = NA_real_,
    probability_improvement = NA_real_, runs_improvement = NA_real_,
    deployment_approved = FALSE,
    status = "awaiting 300 settled versioned state-engine pregame forecasts",
    method = "chronological_70_30_state_gate_v2",
    model_version = "none eligible yet",
    stringsAsFactors = FALSE
  )
  state_reliever_metrics <- data.frame(
    eligible_reliever_forecasts = 0L, settled_reliever_forecasts = 0L,
    mean_appearance_probability = NA_real_, observed_appearance_rate = NA_real_,
    appearance_brier_score = NA_real_, calibration_bias = NA_real_,
    status = "awaiting first pregame state-engine reliever forecast",
    model_version = "none eligible yet", stringsAsFactors = FALSE
  )
  ledger_status <- data.frame(
    snapshots = 0L, archived_game_rows = 0L, eligible_unique_games = 0L, settled_games = 0L,
    archived_player_rows = 0L, eligible_unique_player_rows = 0L, settled_player_rows = 0L,
    archived_matchup_rows = 0L, eligible_unique_matchup_rows = 0L,
    settled_matchup_plate_appearances = 0L,
    next_action = "archive the first eligible pregame forecast",
    leakage_rule = "late snapshots excluded; first eligible snapshot wins; model versions evaluated separately",
    stringsAsFactors = FALSE
  )
  utils::write.csv(game_metrics, file.path(output_dir, "projection-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(game_calibration, file.path(output_dir, "projection-feedback-calibration.csv"), row.names = FALSE, na = "")
  utils::write.csv(player_metrics, file.path(output_dir, "player-projection-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(matchup_metrics, file.path(output_dir, "matchup-event-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(matchup_event_metrics, file.path(output_dir, "matchup-event-feedback-by-event.csv"), row.names = FALSE, na = "")
  utils::write.csv(state_metrics, file.path(output_dir, "state-simulation-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(state_calibration_status, file.path(output_dir, "state-simulation-calibration-status.csv"), row.names = FALSE, na = "")
  utils::write.csv(state_reliever_metrics, file.path(output_dir, "state-reliever-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(ledger_status, file.path(output_dir, "projection-feedback-ledger.csv"), row.names = FALSE, na = "")
  cat("No projection snapshots are available yet; initialized the feedback ledger.\n")
  quit(save = "no", status = 0L)
}
snapshots <- lapply(snapshot_files, readRDS)
game_ledger <- do.call(rbind, lapply(snapshots, `[[`, "game_predictions"))
player_items <- lapply(snapshots, `[[`, "player_predictions")
player_items <- player_items[vapply(player_items, nrow, integer(1)) > 0L]
player_ledger <- if (length(player_items)) do.call(rbind, player_items) else data.frame(
  game_id = character(), player_id = character(), role = character(), pregame_eligible = logical(),
  archived_at_utc = character(), model_version = character(), metric_id = character(), stringsAsFactors = FALSE
)
matchup_items <- lapply(snapshots, function(snapshot) snapshot$matchup_predictions)
matchup_items <- matchup_items[vapply(matchup_items, function(item) is.data.frame(item) && nrow(item) > 0L, logical(1))]
matchup_ledger <- if (length(matchup_items)) do.call(rbind, matchup_items) else data.frame(
  game_id = character(), batter_id = character(), pitcher_id = character(),
  pregame_eligible = logical(), archived_at_utc = character(), model_version = character(),
  stringsAsFactors = FALSE
)
state_items <- lapply(snapshots, function(snapshot) {
  if (!is.null(snapshot$state_predictions)) snapshot$state_predictions else data.frame()
})
state_items <- state_items[vapply(state_items, function(item) is.data.frame(item) && nrow(item) > 0L, logical(1))]
state_ledger <- if (length(state_items)) dplyr::bind_rows(state_items) else data.frame(
  game_id = character(), pregame_eligible = logical(), archived_at_utc = character(),
  model_version = character(), stringsAsFactors = FALSE
)
state_reliever_items <- lapply(snapshots, function(snapshot) {
  if (!is.null(snapshot$state_reliever_predictions)) snapshot$state_reliever_predictions else data.frame()
})
state_reliever_items <- state_reliever_items[vapply(
  state_reliever_items,
  function(item) is.data.frame(item) && nrow(item) > 0L,
  logical(1)
)]
state_reliever_ledger <- if (length(state_reliever_items)) {
  do.call(rbind, state_reliever_items)
} else {
  data.frame(
    game_id = character(), pitcher_id = character(), pregame_eligible = logical(),
    archived_at_utc = character(), model_version = character(),
    appearance_probability = numeric(), stringsAsFactors = FALSE
  )
}
game_ledger$game_id <- as.character(game_ledger$game_id)
player_ledger$game_id <- as.character(player_ledger$game_id)
matchup_ledger$game_id <- as.character(matchup_ledger$game_id)
state_ledger$game_id <- as.character(state_ledger$game_id)
state_reliever_ledger$game_id <- as.character(state_reliever_ledger$game_id)
state_reliever_ledger$pitcher_id <- as.character(state_reliever_ledger$pitcher_id)

eligible_games <- game_ledger[as.logical(game_ledger$pregame_eligible), , drop = FALSE]
if (nrow(eligible_games)) {
  eligible_games <- eligible_games[order(eligible_games$archived_at_utc), , drop = FALSE]
  key <- paste(eligible_games$game_id, eligible_games$model_version, sep = "|")
  eligible_games <- eligible_games[!duplicated(key), , drop = FALSE]
}
eligible_players <- player_ledger[as.logical(player_ledger$pregame_eligible), , drop = FALSE]
if (nrow(eligible_players)) {
  eligible_players <- eligible_players[order(eligible_players$archived_at_utc), , drop = FALSE]
  key <- paste(eligible_players$game_id, eligible_players$model_version, eligible_players$metric_id, eligible_players$player_id, sep = "|")
  eligible_players <- eligible_players[!duplicated(key), , drop = FALSE]
}
eligible_matchups <- matchup_ledger[as.logical(matchup_ledger$pregame_eligible), , drop = FALSE]
if (nrow(eligible_matchups)) {
  eligible_matchups <- eligible_matchups[order(eligible_matchups$archived_at_utc), , drop = FALSE]
  key <- paste(
    eligible_matchups$game_id, eligible_matchups$batter_id,
    eligible_matchups$pitcher_id, eligible_matchups$model_version,
    sep = "|"
  )
  eligible_matchups <- eligible_matchups[!duplicated(key), , drop = FALSE]
}
eligible_state <- state_ledger[as.logical(state_ledger$pregame_eligible), , drop = FALSE]
if (nrow(eligible_state)) {
  eligible_state <- eligible_state[order(eligible_state$archived_at_utc), , drop = FALSE]
  key <- paste(eligible_state$game_id, eligible_state$model_version, sep = "|")
  eligible_state <- eligible_state[!duplicated(key), , drop = FALSE]
}
eligible_state_relievers <- state_reliever_ledger[
  as.logical(state_reliever_ledger$pregame_eligible),
  ,
  drop = FALSE
]
if (nrow(eligible_state_relievers)) {
  eligible_state_relievers <- eligible_state_relievers[
    order(eligible_state_relievers$archived_at_utc),
    ,
    drop = FALSE
  ]
  key <- paste(
    eligible_state_relievers$game_id,
    eligible_state_relievers$pitcher_id,
    eligible_state_relievers$model_version,
    sep = "|"
  )
  eligible_state_relievers <- eligible_state_relievers[!duplicated(key), , drop = FALSE]
}

season <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
pbp_path <- Sys.getenv(
  "SABRHOOD_PBP_PATH",
  unset = file.path(workspace, ".private-data", "pbp", as.character(season), "current.rds")
)
if (!file.exists(pbp_path)) stop("Private PBP cache is missing: ", pbp_path, call. = FALSE)
pbp <- readRDS(pbp_path)
num <- function(value) suppressWarnings(as.numeric(value))
max_score <- function(primary, fallback) {
  value <- c(num(primary), num(fallback)); value <- value[is.finite(value)]
  if (!length(value)) NA_real_ else max(value)
}
predicted_game_ids <- unique(c(
  eligible_games$game_id, eligible_matchups$game_id, eligible_state$game_id,
  eligible_state_relievers$game_id
))
pbp_game_ids <- intersect(predicted_game_ids, unique(as.character(pbp$game_pk)))
game_outcomes <- data.frame()
if (length(pbp_game_ids)) {
  groups <- split(seq_len(nrow(pbp))[as.character(pbp$game_pk) %in% pbp_game_ids], as.character(pbp$game_pk[as.character(pbp$game_pk) %in% pbp_game_ids]))
  game_outcomes <- do.call(rbind, lapply(names(groups), function(game_id) {
    index <- groups[[game_id]]
    data.frame(game_id = game_id,
      actual_away_runs = max_score(pbp$result.awayScore[index], pbp$details.awayScore[index]),
      actual_home_runs = max_score(pbp$result.homeScore[index], pbp$details.homeScore[index]), stringsAsFactors = FALSE)
  }))
  game_outcomes <- game_outcomes[is.finite(game_outcomes$actual_away_runs) & is.finite(game_outcomes$actual_home_runs) & game_outcomes$actual_away_runs != game_outcomes$actual_home_runs, , drop = FALSE]
  game_outcomes$actual_home_win <- as.integer(game_outcomes$actual_home_runs > game_outcomes$actual_away_runs)
}

settled_games <- if (nrow(eligible_games) && nrow(game_outcomes)) merge(eligible_games, game_outcomes, by = "game_id", all = FALSE) else data.frame()
game_metrics <- data.frame(
  settled_games = nrow(settled_games), eligible_unsettled_games = nrow(eligible_games) - nrow(settled_games),
  calibration_minimum_games = 300L, calibration_status = if (nrow(settled_games) >= 300L) "fitted" else "accumulating pregame outcomes",
  brier_score = NA_real_, log_loss = NA_real_, classification_accuracy = NA_real_, calibration_bias = NA_real_,
  away_runs_mae = NA_real_, home_runs_mae = NA_real_, total_runs_mae = NA_real_,
  model_version = if (nrow(eligible_games)) paste(unique(eligible_games$model_version), collapse = "; ") else "none eligible yet",
  stringsAsFactors = FALSE
)
game_calibration <- data.frame(
  probability_bin = NA_integer_, bin_lower = NA_real_, bin_upper = NA_real_, observations = 0L,
  mean_probability = NA_real_, observed_rate = NA_real_, calibration_gap = NA_real_,
  calibration_status = "awaiting eligible settled pregame forecasts", stringsAsFactors = FALSE
)
if (nrow(settled_games)) {
  scored <- score_projection_probabilities(settled_games$home_win_probability, settled_games$actual_home_win)
  game_metrics$brier_score <- scored$brier_score
  game_metrics$log_loss <- scored$log_loss
  game_metrics$classification_accuracy <- scored$classification_accuracy
  game_metrics$calibration_bias <- scored$calibration_bias
  game_metrics$away_runs_mae <- mean(abs(num(settled_games$away_mean_runs) - settled_games$actual_away_runs))
  game_metrics$home_runs_mae <- mean(abs(num(settled_games$home_mean_runs) - settled_games$actual_home_runs))
  game_metrics$total_runs_mae <- mean(abs(num(settled_games$mean_total_runs) - (settled_games$actual_away_runs + settled_games$actual_home_runs)))
  game_calibration <- build_probability_calibration(settled_games$home_win_probability, settled_games$actual_home_win, bins = 10L)
  game_calibration$calibration_status <- "observed"
  if (nrow(settled_games) >= 300L) {
    probability_model <- fit_probability_calibrator(settled_games$home_win_probability, settled_games$actual_home_win, min_rows = 300L)
    saveRDS(probability_model, file.path(private_model_dir, "live-game-probability-calibrator.rds"))
  }
}

settled_state <- if (nrow(eligible_state) && nrow(game_outcomes)) {
  merge(eligible_state, game_outcomes, by = "game_id", all = FALSE)
} else {
  data.frame()
}
calibration_version <- if (nrow(eligible_state)) {
  as.character(utils::tail(eligible_state$model_version, 1L))
} else {
  "none eligible yet"
}
calibration_state <- if (nrow(settled_state)) {
  settled_state[
    as.character(settled_state$model_version) == calibration_version,
    ,
    drop = FALSE
  ]
} else {
  data.frame()
}
state_metrics <- data.frame(
  eligible_game_forecasts = nrow(eligible_state),
  settled_games = nrow(settled_state),
  brier_score = NA_real_,
  away_runs_mae = NA_real_,
  home_runs_mae = NA_real_,
  total_runs_mae = NA_real_,
  mean_tie_probability = if (nrow(eligible_state)) mean(num(eligible_state$tie_probability), na.rm = TRUE) else NA_real_,
  calibration_status = if (nrow(eligible_state)) "accumulating pregame game-state outcomes" else "awaiting first pregame game-state snapshot",
  model_version = if (nrow(eligible_state)) paste(unique(eligible_state$model_version), collapse = "; ") else "none eligible yet",
  stringsAsFactors = FALSE
)
if (nrow(settled_state)) {
  state_metrics$brier_score <- mean((num(settled_state$home_win_probability) - settled_state$actual_home_win)^2)
  state_metrics$away_runs_mae <- mean(abs(num(settled_state$away_mean_runs) - settled_state$actual_away_runs))
  state_metrics$home_runs_mae <- mean(abs(num(settled_state$home_mean_runs) - settled_state$actual_home_runs))
  state_metrics$total_runs_mae <- mean(abs(
    num(settled_state$mean_total_runs) -
      (settled_state$actual_away_runs + settled_state$actual_home_runs)
  ))
  state_metrics$calibration_status <- if (nrow(settled_state) >= 300L) {
    "eligible for chronological calibration review"
  } else {
    "accumulating pregame game-state outcomes"
  }
}

state_calibration_status <- data.frame(
  settled_games = nrow(calibration_state), training_games = 0L, holdout_games = 0L,
  raw_holdout_brier = NA_real_, calibrated_holdout_brier = NA_real_,
  raw_holdout_runs_mae = NA_real_, calibrated_holdout_runs_mae = NA_real_,
  probability_improvement = NA_real_, runs_improvement = NA_real_,
  deployment_approved = FALSE,
  status = if (nrow(calibration_state) < 300L) {
    paste(
      "awaiting",
      300L - nrow(calibration_state),
      "more settled forecasts for",
      calibration_version
    )
  } else {
    "eligible for chronological fitting"
  },
  method = "chronological_70_30_state_gate_v2",
  model_version = calibration_version,
  stringsAsFactors = FALSE
)
if (nrow(calibration_state) >= 300L) {
  ordering_value <- if ("game_time_utc" %in% names(calibration_state)) {
    as.character(calibration_state$game_time_utc)
  } else {
    as.character(calibration_state$archived_at_utc)
  }
  calibration_state <- calibration_state[order(ordering_value), , drop = FALSE]
  training_rows <- floor(0.70 * nrow(calibration_state))
  train <- calibration_state[seq_len(training_rows), , drop = FALSE]
  holdout <- calibration_state[
    (training_rows + 1L):nrow(calibration_state),
    ,
    drop = FALSE
  ]

  probability_model <- fit_probability_calibrator(
    train$home_win_probability,
    train$actual_home_win,
    min_rows = 200L
  )
  calibrated_probability <- predict_calibrated_probability(
    probability_model,
    num(holdout$home_win_probability)
  )
  raw_brier <- mean((num(holdout$home_win_probability) - holdout$actual_home_win)^2)
  calibrated_brier <- mean((calibrated_probability - holdout$actual_home_win)^2)

  away_run_model <- stats::lm(actual_away_runs ~ away_mean_runs, data = train)
  home_run_model <- stats::lm(actual_home_runs ~ home_mean_runs, data = train)
  calibrated_away_runs <- pmax(
    as.numeric(stats::predict(away_run_model, newdata = holdout)),
    0
  )
  calibrated_home_runs <- pmax(
    as.numeric(stats::predict(home_run_model, newdata = holdout)),
    0
  )
  raw_runs_mae <- mean(c(
    abs(num(holdout$away_mean_runs) - holdout$actual_away_runs),
    abs(num(holdout$home_mean_runs) - holdout$actual_home_runs)
  ))
  calibrated_runs_mae <- mean(c(
    abs(calibrated_away_runs - holdout$actual_away_runs),
    abs(calibrated_home_runs - holdout$actual_home_runs)
  ))
  approved <- is.finite(calibrated_brier) && is.finite(calibrated_runs_mae) &&
    calibrated_brier < raw_brier && calibrated_runs_mae <= raw_runs_mae

  state_calibration_status$training_games <- nrow(train)
  state_calibration_status$holdout_games <- nrow(holdout)
  state_calibration_status$raw_holdout_brier <- raw_brier
  state_calibration_status$calibrated_holdout_brier <- calibrated_brier
  state_calibration_status$raw_holdout_runs_mae <- raw_runs_mae
  state_calibration_status$calibrated_holdout_runs_mae <- calibrated_runs_mae
  state_calibration_status$probability_improvement <- raw_brier - calibrated_brier
  state_calibration_status$runs_improvement <- raw_runs_mae - calibrated_runs_mae
  state_calibration_status$deployment_approved <- approved
  state_calibration_status$status <- if (approved) {
    "approved for shadow application; public promotion still withheld"
  } else {
    "chronological holdout gate failed; raw versioned state outputs retained"
  }
  if (approved) {
    saveRDS(
      list(
        probability_model = probability_model,
        away_run_model = away_run_model,
        home_run_model = home_run_model,
        fitted_through = ordering_value[[training_rows]],
        method = state_calibration_status$method,
        model_version = calibration_version
      ),
      file.path(private_model_dir, "state-engine-calibrator.rds")
    )
  }
}

state_reliever_metrics <- data.frame(
  eligible_reliever_forecasts = nrow(eligible_state_relievers),
  settled_reliever_forecasts = 0L,
  mean_appearance_probability = NA_real_,
  observed_appearance_rate = NA_real_,
  appearance_brier_score = NA_real_,
  calibration_bias = NA_real_,
  status = if (nrow(eligible_state_relievers)) {
    "awaiting completed games for state-engine reliever grading"
  } else {
    "awaiting first pregame state-engine reliever forecast"
  },
  model_version = if (nrow(eligible_state_relievers)) {
    paste(unique(eligible_state_relievers$model_version), collapse = "; ")
  } else {
    "none eligible yet"
  },
  stringsAsFactors = FALSE
)
settled_state_relievers <- data.frame()
if (nrow(eligible_state_relievers) && nrow(game_outcomes)) {
  final_ids <- unique(game_outcomes$game_id)
  candidates <- eligible_state_relievers[
    eligible_state_relievers$game_id %in% final_ids,
    ,
    drop = FALSE
  ]
  if (nrow(candidates)) {
    relevant_pitches <- build_pitch_view(
      pbp[as.character(pbp$game_pk) %in% unique(candidates$game_id), , drop = FALSE]
    )
    actual_keys <- unique(paste(
      as.character(relevant_pitches$game_pk),
      as.character(relevant_pitches$pitcher_id),
      sep = "|"
    ))
    candidates$actual_appeared <- as.integer(
      paste(candidates$game_id, candidates$pitcher_id, sep = "|") %in% actual_keys
    )
    settled_state_relievers <- candidates
    scored <- score_projection_probabilities(
      candidates$appearance_probability,
      candidates$actual_appeared
    )
    state_reliever_metrics$settled_reliever_forecasts <- nrow(candidates)
    state_reliever_metrics$mean_appearance_probability <- scored$mean_probability
    state_reliever_metrics$observed_appearance_rate <- scored$observed_rate
    state_reliever_metrics$appearance_brier_score <- scored$brier_score
    state_reliever_metrics$calibration_bias <- scored$calibration_bias
    state_reliever_metrics$status <- if (nrow(candidates) >= 1000L) {
      "eligible for manager-selector calibration review"
    } else {
      "accumulating completed reliever decisions"
    }
  }
}

settled_players <- data.frame()
player_metrics <- data.frame(
  metric_id = "awaiting_eligible_forecasts", metric_label = "Player-event calibration has not started", settled_predictions = 0L,
  mean_probability = NA_real_, observed_rate = NA_real_, brier_score = NA_real_, calibration_bias = NA_real_,
  calibration_status = "awaiting eligible settled pregame forecasts", stringsAsFactors = FALSE
)
if (nrow(eligible_players) && length(pbp_game_ids)) {
  relevant_pbp <- pbp[as.character(pbp$game_pk) %in% pbp_game_ids, , drop = FALSE]
  plate_appearances <- build_plate_appearance_view(build_pitch_view(relevant_pbp))
  batter_groups <- split(seq_len(nrow(plate_appearances)), paste(plate_appearances$game_pk, plate_appearances$batter_id, sep = "|"))
  batter_actual <- if (length(batter_groups)) do.call(rbind, lapply(names(batter_groups), function(key) {
    index <- batter_groups[[key]]; bits <- strsplit(key, "\\|", fixed = FALSE)[[1L]]
    data.frame(game_id = bits[[1L]], player_id = bits[[2L]], role = "hitter",
      actual_H = sum(plate_appearances$is_hit[index]), actual_HR = sum(plate_appearances$is_home_run[index]),
      actual_XBH = sum(plate_appearances$total_bases[index] >= 2), actual_TB = sum(plate_appearances$total_bases[index]),
      actual_K = sum(plate_appearances$is_strikeout[index]), stringsAsFactors = FALSE)
  })) else data.frame()
  pitcher_groups <- split(seq_len(nrow(plate_appearances)), paste(plate_appearances$game_pk, plate_appearances$pitcher_id, sep = "|"))
  pitcher_actual <- if (length(pitcher_groups)) do.call(rbind, lapply(names(pitcher_groups), function(key) {
    index <- pitcher_groups[[key]]; bits <- strsplit(key, "\\|", fixed = FALSE)[[1L]]
    data.frame(game_id = bits[[1L]], player_id = bits[[2L]], role = "pitcher",
      actual_H = sum(plate_appearances$is_hit[index]), actual_HR = sum(plate_appearances$is_home_run[index]),
      actual_XBH = sum(plate_appearances$total_bases[index] >= 2), actual_TB = sum(plate_appearances$total_bases[index]),
      actual_K = sum(plate_appearances$is_strikeout[index]), stringsAsFactors = FALSE)
  })) else data.frame()
  actuals <- rbind(batter_actual, pitcher_actual)
  eligible_players$player_id <- as.character(eligible_players$player_id)
  settled_players <- merge(eligible_players, actuals, by = c("game_id", "player_id", "role"), all = FALSE)
  if (nrow(settled_players)) {
    settled_players$actual_binary <- with(settled_players, ifelse(metric_id == "batter_hit_1plus", actual_H >= 1,
      ifelse(metric_id == "batter_hit_2plus", actual_H >= 2,
        ifelse(metric_id == "batter_hr_1plus", actual_HR >= 1,
          ifelse(metric_id == "batter_xbh_1plus", actual_XBH >= 1,
            ifelse(metric_id == "batter_tb_2plus", actual_TB >= 2,
              ifelse(metric_id == "batter_tb_3plus", actual_TB >= 3,
                ifelse(metric_id == "pitcher_k_5plus", actual_K >= 5,
                  ifelse(metric_id == "pitcher_k_7plus", actual_K >= 7, NA)))))))))
    settled_players$actual_binary <- as.integer(settled_players$actual_binary)
    groups <- split(seq_len(nrow(settled_players)), settled_players$metric_id)
    player_metrics <- do.call(rbind, lapply(names(groups), function(metric_id) {
      index <- groups[[metric_id]]
      scored <- score_projection_probabilities(settled_players$probability[index], settled_players$actual_binary[index])
      data.frame(metric_id = metric_id, metric_label = settled_players$metric_label[index[[1L]]],
        settled_predictions = scored$observations, mean_probability = scored$mean_probability,
        observed_rate = scored$observed_rate, brier_score = scored$brier_score, calibration_bias = scored$calibration_bias,
        calibration_status = if (scored$observations >= 300L) "eligible to fit" else "accumulating outcomes", stringsAsFactors = FALSE)
    }))
  }
}

matchup_metrics <- data.frame(
  eligible_matchups = nrow(eligible_matchups), settled_matchups = 0L,
  observed_plate_appearances = 0L, multiclass_log_loss = NA_real_,
  multiclass_brier_score = NA_real_, predicted_woba = NA_real_,
  observed_woba = NA_real_,
  calibration_status = if (nrow(eligible_matchups)) "accumulating observed matchup plate appearances" else "awaiting first pregame matchup snapshot",
  model_version = if (nrow(eligible_matchups)) paste(unique(eligible_matchups$model_version), collapse = "; ") else "none eligible yet",
  stringsAsFactors = FALSE
)
matchup_event_metrics <- data.frame(
  event = c("BB", "HBP", "K", "X1B", "X2B", "X3B", "HR", "OUT"),
  observations = 0L, mean_probability = NA_real_, observed_rate = NA_real_,
  brier_score = NA_real_, calibration_bias = NA_real_,
  calibration_status = if (nrow(eligible_matchups)) "awaiting completed matchup plate appearances" else "awaiting first pregame matchup snapshot",
  stringsAsFactors = FALSE
)
settled_matchup_pa <- data.frame()
if (nrow(eligible_matchups) && length(pbp_game_ids)) {
  relevant_pbp <- pbp[as.character(pbp$game_pk) %in% pbp_game_ids, , drop = FALSE]
  plate_appearances <- build_plate_appearance_view(build_pitch_view(relevant_pbp))
  plate_appearances$game_id <- as.character(plate_appearances$game_pk)
  plate_appearances$batter_id <- as.character(plate_appearances$batter_id)
  plate_appearances$pitcher_id <- as.character(plate_appearances$pitcher_id)
  plate_appearances$actual_event <- ifelse(
    plate_appearances$event_key %in% c("walk", "intent_walk", "intentional_walk"), "BB",
    ifelse(
      plate_appearances$event_key == "hit_by_pitch", "HBP",
      ifelse(
        plate_appearances$event_key %in% c("strikeout", "strikeout_double_play"), "K",
        ifelse(
          plate_appearances$event_key == "single", "X1B",
          ifelse(
            plate_appearances$event_key == "double", "X2B",
            ifelse(
              plate_appearances$event_key == "triple", "X3B",
              ifelse(plate_appearances$event_key == "home_run", "HR", "OUT")
            )
          )
        )
      )
    )
  )
  settled_matchup_pa <- merge(
    eligible_matchups,
    plate_appearances[, c("game_id", "batter_id", "pitcher_id", "actual_event"), drop = FALSE],
    by = c("game_id", "batter_id", "pitcher_id"),
    all = FALSE
  )
  if (nrow(settled_matchup_pa)) {
    event_columns <- c(BB = "p_BB", HBP = "p_HBP", K = "p_K", X1B = "p_1B", X2B = "p_2B", X3B = "p_3B", HR = "p_HR", OUT = "p_OUT")
    actual_probability <- vapply(seq_len(nrow(settled_matchup_pa)), function(index) {
      column <- event_columns[[settled_matchup_pa$actual_event[[index]]]]
      pmax(pmin(num(settled_matchup_pa[[column]][[index]]), 1 - 1e-12), 1e-12)
    }, numeric(1))
    one_hot_brier <- vapply(seq_len(nrow(settled_matchup_pa)), function(index) {
      probability <- num(settled_matchup_pa[index, unname(event_columns), drop = TRUE])
      actual <- as.numeric(names(event_columns) == settled_matchup_pa$actual_event[[index]])
      sum((probability - actual)^2)
    }, numeric(1))
    woba_weights <- c(BB = 0.69, HBP = 0.72, K = 0, X1B = 0.88, X2B = 1.24, X3B = 1.56, HR = 2.01, OUT = 0)
    matchup_metrics$settled_matchups <- length(unique(paste(settled_matchup_pa$game_id, settled_matchup_pa$batter_id, settled_matchup_pa$pitcher_id)))
    matchup_metrics$observed_plate_appearances <- nrow(settled_matchup_pa)
    matchup_metrics$multiclass_log_loss <- -mean(log(actual_probability))
    matchup_metrics$multiclass_brier_score <- mean(one_hot_brier)
    matchup_metrics$predicted_woba <- mean(num(settled_matchup_pa$estimated_woba))
    matchup_metrics$observed_woba <- mean(woba_weights[settled_matchup_pa$actual_event])
    matchup_metrics$calibration_status <- if (nrow(settled_matchup_pa) >= 5000L) "eligible for event calibration review" else "accumulating observed matchup plate appearances"
    matchup_event_metrics <- do.call(rbind, lapply(names(event_columns), function(event) {
      probability <- num(settled_matchup_pa[[event_columns[[event]]]])
      actual <- as.integer(settled_matchup_pa$actual_event == event)
      data.frame(
        event = event, observations = length(actual),
        mean_probability = mean(probability), observed_rate = mean(actual),
        brier_score = mean((probability - actual)^2),
        calibration_bias = mean(probability) - mean(actual),
        calibration_status = if (length(actual) >= 5000L) "eligible for calibration review" else "accumulating outcomes",
        stringsAsFactors = FALSE
      )
    }))
  }
}

ledger_status <- data.frame(
  snapshots = length(snapshot_files), archived_game_rows = nrow(game_ledger), eligible_unique_games = nrow(eligible_games), settled_games = nrow(settled_games),
  archived_player_rows = nrow(player_ledger), eligible_unique_player_rows = nrow(eligible_players), settled_player_rows = nrow(settled_players),
  archived_matchup_rows = nrow(matchup_ledger), eligible_unique_matchup_rows = nrow(eligible_matchups),
  settled_matchup_plate_appearances = nrow(settled_matchup_pa),
  archived_state_game_rows = nrow(state_ledger), eligible_unique_state_games = nrow(eligible_state),
  settled_state_games = nrow(settled_state),
  archived_state_reliever_rows = nrow(state_reliever_ledger),
  eligible_state_reliever_rows = nrow(eligible_state_relievers),
  settled_state_reliever_rows = nrow(settled_state_relievers),
  next_action = if (nrow(eligible_games) < 300L) paste0("archive ", 300L - nrow(eligible_games), " more eligible game forecasts before fitting live probability calibration") else "fit and validate live probability calibration",
  leakage_rule = "late snapshots excluded; first eligible snapshot wins; model versions evaluated separately",
  stringsAsFactors = FALSE
)

saveRDS(
  list(
    games = settled_games, players = settled_players, matchups = settled_matchup_pa,
    state_games = settled_state, state_relievers = settled_state_relievers
  ),
  file.path(ledger_dir, "settled-projection-ledger.rds")
)
utils::write.csv(game_metrics, file.path(output_dir, "projection-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(game_calibration, file.path(output_dir, "projection-feedback-calibration.csv"), row.names = FALSE, na = "")
utils::write.csv(player_metrics, file.path(output_dir, "player-projection-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(matchup_metrics, file.path(output_dir, "matchup-event-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(matchup_event_metrics, file.path(output_dir, "matchup-event-feedback-by-event.csv"), row.names = FALSE, na = "")
utils::write.csv(state_metrics, file.path(output_dir, "state-simulation-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(state_calibration_status, file.path(output_dir, "state-simulation-calibration-status.csv"), row.names = FALSE, na = "")
utils::write.csv(state_reliever_metrics, file.path(output_dir, "state-reliever-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(ledger_status, file.path(output_dir, "projection-feedback-ledger.csv"), row.names = FALSE, na = "")
cat("Settled", nrow(settled_games), "eligible games and", nrow(settled_players), "eligible player-event rows.\n")
cat(
  "Settled", matchup_metrics$observed_plate_appearances,
  "starter-matchup plate appearances from",
  matchup_metrics$settled_matchups, "eligible batter-pitcher pairs.\n"
)
