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
  ledger_status <- data.frame(
    snapshots = 0L, archived_game_rows = 0L, eligible_unique_games = 0L, settled_games = 0L,
    archived_player_rows = 0L, eligible_unique_player_rows = 0L, settled_player_rows = 0L,
    next_action = "archive the first eligible pregame forecast",
    leakage_rule = "late snapshots excluded; first eligible snapshot wins; model versions evaluated separately",
    stringsAsFactors = FALSE
  )
  utils::write.csv(game_metrics, file.path(output_dir, "projection-feedback-metrics.csv"), row.names = FALSE, na = "")
  utils::write.csv(game_calibration, file.path(output_dir, "projection-feedback-calibration.csv"), row.names = FALSE, na = "")
  utils::write.csv(player_metrics, file.path(output_dir, "player-projection-feedback-metrics.csv"), row.names = FALSE, na = "")
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
game_ledger$game_id <- as.character(game_ledger$game_id)
player_ledger$game_id <- as.character(player_ledger$game_id)

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
predicted_game_ids <- unique(eligible_games$game_id)
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

ledger_status <- data.frame(
  snapshots = length(snapshot_files), archived_game_rows = nrow(game_ledger), eligible_unique_games = nrow(eligible_games), settled_games = nrow(settled_games),
  archived_player_rows = nrow(player_ledger), eligible_unique_player_rows = nrow(eligible_players), settled_player_rows = nrow(settled_players),
  next_action = if (nrow(eligible_games) < 300L) paste0("archive ", 300L - nrow(eligible_games), " more eligible game forecasts before fitting live probability calibration") else "fit and validate live probability calibration",
  leakage_rule = "late snapshots excluded; first eligible snapshot wins; model versions evaluated separately",
  stringsAsFactors = FALSE
)

saveRDS(list(games = settled_games, players = settled_players), file.path(ledger_dir, "settled-projection-ledger.rds"))
utils::write.csv(game_metrics, file.path(output_dir, "projection-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(game_calibration, file.path(output_dir, "projection-feedback-calibration.csv"), row.names = FALSE, na = "")
utils::write.csv(player_metrics, file.path(output_dir, "player-projection-feedback-metrics.csv"), row.names = FALSE, na = "")
utils::write.csv(ledger_status, file.path(output_dir, "projection-feedback-ledger.csv"), row.names = FALSE, na = "")
cat("Settled", nrow(settled_games), "eligible games and", nrow(settled_players), "eligible player-event rows.\n")
