suppressPackageStartupMessages({
  library(dplyr)
  library(sabrhoodR)
})

season <- as.integer(Sys.getenv("SABRHOOD_SEASON", unset = format(Sys.Date(), "%Y")))
pbp_path <- Sys.getenv(
  "SABRHOOD_PBP_PATH",
  unset = file.path(".private-data", "pbp", as.character(season), "current.rds")
)
output_dir <- Sys.getenv("SABRHOOD_DERIVED_DIR", unset = file.path("data", "derived"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(pbp_path)) stop("Private PBP cache is missing: ", pbp_path, call. = FALSE)

source_through <- NA_character_
generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
write_product <- function(data, name) {
  if (!is.data.frame(data)) stop("Product is not a data frame: ", name, call. = FALSE)
  if (!"source_through" %in% names(data)) data$source_through <- as.character(source_through)
  if (!"generated_at_utc" %in% names(data)) data$generated_at_utc <- generated_at_utc
  utils::write.csv(data, file.path(output_dir, name), row.names = FALSE, na = "")
  cat(sprintf("%-42s %s rows\n", name, format(nrow(data), big.mark = ",")))
  invisible(data)
}

cat("Reading private completed-game PBP cache...\n")
pbp <- readRDS(pbp_path)
date_column <- if ("game_date" %in% names(pbp)) "game_date" else "game_date_date"
source_through <- max(as.Date(pbp[[date_column]]), na.rm = TRUE)
if (identical(date_column, "game_date_date")) {
  pbp$game_date <- pbp[[date_column]]
  date_column <- "game_date"
}

# The Stats API cache is intentionally wide so the private archive remains
# useful for future research. The public refresh needs only the columns used by
# build_pitch_view(). Dropping the other source columns before filtering pitch
# rows prevents the GitHub runner from holding two copies of the full wide
# table while the canonical pitch tibble is allocated.
pitch_source_columns <- c(
  "isPitch", "is_pitch", "game_pk", "gamePk", "game_date", "gameDate",
  "about.halfInning", "half_inning", "about.isTopInning", "is_top_inning",
  "about.atBatIndex", "atBatIndex", "pitchNumber", "pitch_number", "playId", "play_id",
  "about.inning", "inning", "batting_team", "battingTeam", "fielding_team", "fieldingTeam",
  "home_team", "homeTeam", "away_team", "awayTeam",
  "matchup.pitcher.id", "pitcher_id", "matchup.pitcher.fullName", "pitcher_name",
  "matchup.pitchHand.code", "pitcher_hand", "matchup.batter.id", "batter_id",
  "matchup.batter.fullName", "batter_name", "matchup.batSide.code", "batter_side",
  "count.balls.start", "balls_before", "count.strikes.start", "strikes_before",
  "count.outs.start", "outs_before", "count.balls.end", "balls_after",
  "count.strikes.end", "strikes_after", "count.outs.end", "outs_after",
  "matchup.postOnFirst.id", "runner_on_first_id", "matchup.postOnSecond.id", "runner_on_second_id",
  "matchup.postOnThird.id", "runner_on_third_id", "result.eventType", "details.eventType", "event_type",
  "result.event", "details.event", "event", "details.description", "result.description", "description",
  "details.type.code", "pitch_type", "details.type.description", "pitch_name",
  "details.call.code", "call_code", "details.call.description", "call_description",
  "details.isInPlay", "is_in_play", "details.isStrike", "is_strike", "details.isBall", "is_ball",
  "last.pitch.of.ab", "last_pitch_of_ab", "pitchData.startSpeed", "release_speed",
  "pitchData.endSpeed", "end_speed", "pitchData.zone", "zone",
  "pitchData.strikeZoneTop", "strike_zone_top", "pitchData.strikeZoneBottom", "strike_zone_bottom",
  "pitchData.extension", "extension", "pitchData.coordinates.pX", "plate_x",
  "pitchData.coordinates.pZ", "plate_z", "pitchData.coordinates.x0", "release_x",
  "pitchData.coordinates.z0", "release_z", "pitchData.breaks.breakHorizontal", "horizontal_break",
  "pitchData.breaks.breakVerticalInduced", "induced_vertical_break", "pitchData.breaks.spinRate", "spin_rate",
  "hitData.launchSpeed", "launch_speed", "hitData.launchAngle", "launch_angle",
  "hitData.totalDistance", "hit_distance", "hitData.location", "hit_location",
  "hitData.coordinates.coordX", "hit_coord_x", "hitData.coordinates.coordY", "hit_coord_y",
  "hitData.trajectory", "trajectory", "battingOrder", "batting_order", "result.rbi", "runs_batted_in",
  "details.homeScore", "home_score_current", "details.awayScore", "away_score_current",
  "result.homeScore", "home_score", "result.awayScore", "away_score"
)
keep_columns <- names(pbp) %in% pitch_source_columns
raw_column_count <- ncol(pbp)
pbp <- pbp[, keep_columns, drop = FALSE]
invisible(gc())

pitch_flag_column <- if ("isPitch" %in% names(pbp)) "isPitch" else "is_pitch"
pitch_flag <- pbp[[pitch_flag_column]]
if (!is.logical(pitch_flag)) {
  pitch_flag <- tolower(as.character(pitch_flag)) %in% c("true", "t", "1", "yes", "y")
}
pbp <- pbp[pitch_flag %in% TRUE, , drop = FALSE]
invisible(gc())
cat(
  "Building canonical pitch and appearance views through", as.character(source_through),
  "from", format(nrow(pbp), big.mark = ","), "pitch rows and",
  ncol(pbp), "of", raw_column_count, "raw columns...\n"
)
pitch_month <- format(as.Date(pbp[[date_column]]), "%Y-%m")
chunk_rows <- split(seq_len(nrow(pbp)), pitch_month, drop = TRUE)
pitch_chunks <- lapply(names(chunk_rows), function(month) {
  rows <- chunk_rows[[month]]
  cat("  canonicalizing", month, "(", format(length(rows), big.mark = ","), "rows )\n")
  chunk <- build_pitch_view(pbp[rows, , drop = FALSE])
  chunk$source_row <- rows[chunk$source_row]
  chunk
})
rm(pbp)
invisible(gc())
pitches <- dplyr::bind_rows(pitch_chunks)
rm(pitch_chunks, chunk_rows)
invisible(gc())

pitch_type_summary <- summarize_pitch_types(pitches, minimum_pitches = 100L)
pitch_usage_changes <- build_pitch_usage_change_board(
  pitches,
  recent_games = 5L,
  minimum_recent_pitches = 15L,
  minimum_baseline_pitches = 40L
)
appearances <- build_pitcher_appearance_view(pitches)
pitcher_registry <- build_pitcher_registry(appearances)
bullpen_availability <- build_bullpen_availability(
  appearances,
  pitcher_registry,
  as_of_date = source_through
)
rm(appearances)
invisible(gc())

plate_appearances <- build_plate_appearance_view(pitches)
run_expectancy <- build_run_expectancy_table(plate_appearances)
plate_appearances <- add_run_expectancy(plate_appearances, run_expectancy)
plate_appearances <- add_game_context(plate_appearances)

hitters <- summarize_hitters(plate_appearances, minimum_pa = 100L)
pitchers <- summarize_pitchers(plate_appearances, minimum_pa = 75L)
hitter_platoon <- summarize_hitters(
  plate_appearances,
  group_by = "opponent_hand",
  minimum_pa = 40L
)
pitcher_platoon <- summarize_pitchers(
  plate_appearances,
  group_by = "opponent_hand",
  minimum_pa = 40L
)
hitter_form <- compare_recent_player_form(
  plate_appearances,
  perspective = "batter",
  recent_days = 14L,
  minimum_recent_pa = 25L,
  minimum_baseline_pa = 50L
)
pitcher_form <- compare_recent_player_form(
  plate_appearances,
  perspective = "pitcher",
  recent_days = 14L,
  minimum_recent_pa = 20L,
  minimum_baseline_pa = 40L
)

cat("Building manager hook model and validation products...\n")
manager_decisions <- build_manager_pitching_decisions(plate_appearances, pitcher_registry)
hook_model <- fit_manager_hook_model(manager_decisions)
hook_validation <- validate_manager_hook_model(
  manager_decisions,
  validation_fraction = 0.20,
  calibration_bins = 10L
)
hook_scenarios <- build_hook_scenario_board(hook_model)
hook_examples <- manager_decisions[
  manager_decisions$pitcher_pulled_after_pa %in% TRUE,
  intersect(
    c(
      "game_date", "game_pk", "inning", "half_inning", "fielding_team",
      "pitcher_id", "pitcher_name", "pitcher_hand", "pitcher_role",
      "batters_faced_in_appearance", "pitches_in_appearance",
      "times_through_order_proxy", "event_key", "fielding_score_diff",
      "next_batter_id", "next_batter_name", "next_batter_side",
      "next_pitcher_id", "next_pitcher_name"
    ),
    names(manager_decisions)
  ),
  drop = FALSE
]
hook_examples <- predict_manager_hook_probability(hook_model, hook_examples)
hook_coefficients <- as.data.frame(summary(hook_model$model)$coefficients)
hook_coefficients$term <- rownames(hook_coefficients)
rownames(hook_coefficients) <- NULL
names(hook_coefficients)[1:4] <- c("estimate", "standard_error", "z_value", "p_value")
hook_coefficients$training_rows <- hook_model$training_rows
hook_coefficients$observed_hook_rate <- hook_model$observed_hook_rate
hook_coefficients$method <- hook_model$method
manager_summary <- data.frame(
  dataset = c(
    "manager_pitching_decisions", "observed_pitcher_hooks",
    "hook_model_training_rows", "hook_model_validation_rows"
  ),
  rows = c(
    nrow(manager_decisions),
    sum(manager_decisions$pitcher_pulled_after_pa, na.rm = TRUE),
    hook_model$training_rows,
    hook_validation$metrics$validation_rows
  ),
  method = c(
    "canonical_completed_pa_v1", "next_pitcher_changed_v1", hook_model$method,
    hook_validation$metrics$validation_method
  ),
  source_through = as.character(source_through),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)
rm(manager_decisions, pitcher_registry)
invisible(gc())

cat("Building league movement, discipline and unusual-award products...\n")
league_trends <- build_rolling_league_trends(pitches, plate_appearances, rolling_days = 14L)
insane_awards <- build_insane_baseball_awards(
  hitters,
  pitchers,
  pitches,
  minimum_pa = 100L,
  minimum_split_pa = 15L
)
discipline <- pitches |>
  dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
  dplyr::group_by(.data$batter_id) |>
  dplyr::summarise(
    player_name = dplyr::last(.data$batter_name),
    team = dplyr::last(.data$batting_team),
    pitches = dplyr::n(),
    swings = sum(.data$is_swing),
    whiffs = sum(.data$is_whiff),
    out_of_zone_pitches = sum(.data$is_out_of_zone),
    chases = sum(.data$is_chase),
    zone_swings = sum(.data$is_zone_swing),
    zone_contact = sum(.data$is_zone_contact),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$pitches >= 300) |>
  dplyr::mutate(
    swing_rate = .data$swings / .data$pitches,
    whiff_rate = .data$whiffs / pmax(.data$swings, 1),
    chase_rate = .data$chases / pmax(.data$out_of_zone_pitches, 1),
    zone_contact_rate = .data$zone_contact / pmax(.data$zone_swings, 1),
    discipline_score = round(100 * (
      0.40 * (1 - dplyr::percent_rank(.data$chase_rate)) +
        0.35 * dplyr::percent_rank(.data$zone_contact_rate) +
        0.25 * (1 - dplyr::percent_rank(.data$whiff_rate))
    ), 1),
    discipline_method = "pitch_level_chase_contact_profile_v1"
  ) |>
  dplyr::arrange(dplyr::desc(.data$discipline_score))
discipline$discipline_rank <- seq_len(nrow(discipline))

spotlight_candidates <- pitch_usage_changes[
  is.finite(pitch_usage_changes$usage_delta_pp) & pitch_usage_changes$usage_delta_pp > 0 &
    is.finite(pitch_usage_changes$whiff_delta) & pitch_usage_changes$whiff_delta > 0,
  ,
  drop = FALSE
]
spotlight_candidates$spotlight_score <- spotlight_candidates$change_signal_score +
  pmin(spotlight_candidates$usage_delta_pp, 10) * 2 +
  pmin(100 * spotlight_candidates$whiff_delta, 20)
spotlight_candidates <- spotlight_candidates[
  order(-spotlight_candidates$spotlight_score),
  ,
  drop = FALSE
]
spotlight_candidates <- spotlight_candidates[!duplicated(spotlight_candidates$pitcher_id), , drop = FALSE]
spotlight_candidates <- utils::head(spotlight_candidates, 4L)
arsenal_spotlights <- do.call(rbind, lapply(seq_len(nrow(spotlight_candidates)), function(index) {
  candidate <- spotlight_candidates[index, , drop = FALSE]
  arsenal <- pitch_type_summary[
    pitch_type_summary$player_id == candidate$pitcher_id[[1L]],
    ,
    drop = FALSE
  ]
  arsenal <- arsenal[order(-arsenal$usage_rate), , drop = FALSE]
  arsenal$spotlight_rank <- index
  arsenal$featured_pitch_type <- candidate$pitch_type[[1L]]
  arsenal$featured_pitch_name <- candidate$pitch_name[[1L]]
  arsenal$featured_usage_delta_pp <- candidate$usage_delta_pp[[1L]]
  arsenal$featured_whiff_delta <- candidate$whiff_delta[[1L]]
  arsenal$spotlight_score <- candidate$spotlight_score[[1L]]
  arsenal$spotlight_method <- "usage_gain_plus_pitch_success_v1"
  arsenal
}))

pull_pool <- hitters[hitters$pa >= 100 & is.finite(hitters$pull_rate), , drop = FALSE]
pull_leader <- pull_pool[order(-pull_pool$pull_rate, -pull_pool$batted_balls), , drop = FALSE][1L, ]
pull_spray <- plate_appearances[
  plate_appearances$batter_id == pull_leader$player_id[[1L]] &
    plate_appearances$is_batted_ball %in% TRUE &
    is.finite(plate_appearances$hit_coord_x) &
    is.finite(plate_appearances$hit_coord_y),
  ,
  drop = FALSE
]
pull_spray$leader_pull_rate <- pull_leader$pull_rate[[1L]]
pull_spray$leader_batted_balls <- pull_leader$batted_balls[[1L]]
pull_spray$spray_method <- "mlbam_hit_coordinates_pull_rate_leader_v1"

write_product(pitch_type_summary, "pitch-type-summary.csv")
write_product(pitch_usage_changes, "pitch-usage-change-board.csv")
write_product(bullpen_availability, "bullpen-availability.csv")
write_product(run_expectancy, "run-expectancy-24.csv")
write_product(hitters, "hitter-performance-summary.csv")
write_product(pitchers, "pitcher-performance-summary.csv")
write_product(hitter_platoon, "hitter-platoon-summary.csv")
write_product(pitcher_platoon, "pitcher-platoon-summary.csv")
write_product(hitter_form, "hitter-recent-form.csv")
write_product(pitcher_form, "pitcher-recent-form.csv")
write_product(utils::head(hook_examples, 250L), "manager-hook-examples.csv")
write_product(hook_coefficients, "manager-hook-model.csv")
write_product(hook_validation$metrics, "manager-hook-validation-metrics.csv")
write_product(hook_validation$calibration, "manager-hook-calibration.csv")
write_product(hook_scenarios, "manager-hook-scenarios.csv")
write_product(manager_summary, "manager-data-summary.csv")
write_product(league_trends$pitch_usage, "rolling-league-pitch-usage.csv")
write_product(league_trends$production, "rolling-league-production.csv")
write_product(insane_awards, "insane-baseball-awards.csv")
write_product(discipline, "hitter-discipline-profiles.csv")
write_product(arsenal_spotlights, "arsenal-spotlights.csv")
write_product(pull_spray, "pull-rate-leader-batted-balls.csv")

cat("Completed full PBP product build through", as.character(source_through), ".\n")
