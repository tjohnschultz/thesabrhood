workspace <- normalizePath(".", winslash = "/", mustWork = TRUE)
output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path(workspace, "data", "derived"))
ledger_dir <- Sys.getenv("SABRHOOD_LEDGER_DIR", unset = file.path(workspace, ".private-data", "projection-ledger"))
snapshot_time <- Sys.time()
snapshot_id <- format(snapshot_time, "%Y%m%d-%H%M%S")

read_product <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing projection snapshot input: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
games <- read_product("daily-game-inputs.csv")
game_predictions <- read_product("daily-projections-live.csv")
player_predictions <- if (file.exists(file.path(output_dir, "daily-player-simulations.csv"))) {
  read_product("daily-player-simulations.csv")
} else {
  data.frame(game_id = character(), stringsAsFactors = FALSE)
}
player_model <- if (file.exists(file.path(output_dir, "daily-player-simulation-model-card.csv"))) {
  read_product("daily-player-simulation-model-card.csv")
} else {
  data.frame(publication_status = "no current player simulations", stringsAsFactors = FALSE)
}
game_readiness <- read_product("projection-publication-readiness.csv")

games$game_id <- as.character(games$game_id)
game_predictions$game_id <- as.character(game_predictions$game_id)
player_predictions$game_id <- as.character(player_predictions$game_id)
player_predictions <- player_predictions[player_predictions$game_id %in% games$game_id, , drop = FALSE]
game_index <- match(game_predictions$game_id, games$game_id)
first_pitch <- as.POSIXct(games$game_time_utc[game_index], format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
archived_at_utc <- as.POSIXct(format(snapshot_time, tz = "UTC", usetz = TRUE), tz = "UTC")
game_predictions$snapshot_id <- snapshot_id
game_predictions$archived_at_utc <- format(snapshot_time, tz = "UTC", usetz = TRUE)
game_predictions$first_pitch_utc <- format(first_pitch, tz = "UTC", usetz = TRUE)
game_predictions$pregame_eligible <- !is.na(first_pitch) & archived_at_utc <= first_pitch
game_predictions$evaluation_status <- ifelse(game_predictions$pregame_eligible, "eligible_when_final", "late_snapshot_excluded")

if (nrow(player_predictions)) {
  player_game_match <- match(player_predictions$game_id, game_predictions$game_id)
  player_predictions$snapshot_id <- snapshot_id
  player_predictions$archived_at_utc <- format(snapshot_time, tz = "UTC", usetz = TRUE)
  player_predictions$pregame_eligible <- game_predictions$pregame_eligible[player_game_match]
  player_predictions$evaluation_status <- ifelse(player_predictions$pregame_eligible, "eligible_when_final", "late_snapshot_excluded")
} else {
  player_predictions$snapshot_id <- character()
  player_predictions$archived_at_utc <- character()
  player_predictions$pregame_eligible <- logical()
  player_predictions$evaluation_status <- character()
}

snapshot_date <- if (nrow(games)) as.character(games$game_date[[1L]]) else format(snapshot_time, "%Y-%m-%d")
snapshot_dir <- file.path(ledger_dir, "snapshots", snapshot_date)
dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
snapshot <- list(
  snapshot_id = snapshot_id,
  archived_at_utc = format(snapshot_time, tz = "UTC", usetz = TRUE),
  game_date = snapshot_date,
  game_predictions = game_predictions,
  player_predictions = player_predictions,
  game_inputs = games,
  player_model = player_model,
  game_readiness = game_readiness,
  snapshot_rule = "only predictions archived at or before first pitch are eligible for calibration"
)
saveRDS(snapshot, file.path(snapshot_dir, paste0(snapshot_id, ".rds")), compress = TRUE)

snapshot_files <- list.files(file.path(ledger_dir, "snapshots"), pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
snapshots <- lapply(snapshot_files, readRDS)
all_games <- do.call(rbind, lapply(snapshots, `[[`, "game_predictions"))
player_items <- lapply(snapshots, `[[`, "player_predictions")
player_items <- player_items[vapply(player_items, nrow, integer(1)) > 0L]
all_players <- if (length(player_items)) do.call(rbind, player_items) else data.frame(pregame_eligible = logical())
status <- data.frame(
  snapshot_id = snapshot_id,
  snapshot_date = snapshot_date,
  snapshots_archived = length(snapshot_files),
  game_rows_archived = nrow(all_games),
  player_rows_archived = nrow(all_players),
  eligible_game_rows = sum(as.logical(all_games$pregame_eligible), na.rm = TRUE),
  eligible_player_rows = sum(as.logical(all_players$pregame_eligible), na.rm = TRUE),
  current_snapshot_games = nrow(game_predictions),
  current_snapshot_player_rows = nrow(player_predictions),
  current_snapshot_eligible_games = sum(game_predictions$pregame_eligible, na.rm = TRUE),
  calibration_minimum_games = 300L,
  calibration_rule = "first eligible pregame snapshot per game and model version; late builds never enter evaluation",
  stringsAsFactors = FALSE
)
utils::write.csv(status, file.path(output_dir, "projection-ledger-status.csv"), row.names = FALSE, na = "")
cat("Archived", nrow(game_predictions), "game predictions and", nrow(player_predictions), "player-event probabilities in snapshot", snapshot_id, ".\n")
cat(sum(game_predictions$pregame_eligible), "games were archived before first pitch and are eligible for future calibration.\n")
