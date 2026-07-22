.column_or_default <- function(data, candidates, default = NA) {
  existing <- candidates[candidates %in% names(data)]
  if (length(existing) == 0L) return(rep(default, nrow(data)))

  output <- data[[existing[[1L]]]]
  if (length(existing) == 1L) return(output)

  for (column in existing[-1L]) {
    replacement <- data[[column]]
    missing <- is.na(output)
    if (is.character(output)) missing <- missing | output == ""
    output[missing] <- replacement[missing]
  }
  output
}

.logical_value <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

.integer_value <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

.numeric_value <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

.normalize_event_key <- function(x) {
  output <- tolower(trimws(as.character(x)))
  output <- gsub("[^a-z0-9]+", "_", output)
  gsub("(^_+|_+$)", "", output)
}

.first_non_missing <- function(x) {
  available <- x[!is.na(x) & as.character(x) != ""]
  if (length(available) == 0L) NA else available[[1L]]
}

.last_non_missing <- function(x) {
  available <- x[!is.na(x) & as.character(x) != ""]
  if (length(available) == 0L) NA else available[[length(available)]]
}

#' Build the canonical pitch-level view
#'
#' @param pbp Raw or previously enriched BaseballR MLB play-by-play data.
#'
#' @return One row per pitch with stable SABRhood column names.
#' @export
build_pitch_view <- function(pbp) {
  if (!is.data.frame(pbp) || nrow(pbp) == 0L) {
    stop("`pbp` must be a non-empty data frame.", call. = FALSE)
  }

  is_pitch <- .logical_value(.column_or_default(pbp, c("isPitch", "is_pitch"), FALSE))
  raw <- pbp[is_pitch %in% TRUE, , drop = FALSE]
  if (nrow(raw) == 0L) stop("No pitch rows were found in `pbp`.", call. = FALSE)

  half <- tolower(as.character(.column_or_default(raw, c("about.halfInning", "half_inning"))))
  is_top_raw <- .column_or_default(raw, c("about.isTopInning", "is_top_inning"), NA)
  is_top <- ifelse(is.na(is_top_raw), half == "top", .logical_value(is_top_raw))

  output <- tibble::tibble(
    source_row = which(is_pitch %in% TRUE),
    game_pk = as.character(.column_or_default(raw, c("game_pk", "gamePk"))),
    game_date = as.Date(.column_or_default(raw, c("game_date", "gameDate"))),
    at_bat_index = .integer_value(.column_or_default(raw, c("about.atBatIndex", "atBatIndex"))),
    pitch_number = .integer_value(.column_or_default(raw, c("pitchNumber", "pitch_number"))),
    play_id = as.character(.column_or_default(raw, c("playId", "play_id"))),
    inning = .integer_value(.column_or_default(raw, c("about.inning", "inning"))),
    half_inning = half,
    is_top_inning = is_top,
    batting_team = as.character(.column_or_default(raw, c("batting_team", "battingTeam"))),
    fielding_team = as.character(.column_or_default(raw, c("fielding_team", "fieldingTeam"))),
    home_team = as.character(.column_or_default(raw, c("home_team", "homeTeam"))),
    away_team = as.character(.column_or_default(raw, c("away_team", "awayTeam"))),
    pitcher_id = as.character(.column_or_default(raw, c("matchup.pitcher.id", "pitcher_id"))),
    pitcher_name = as.character(.column_or_default(raw, c("matchup.pitcher.fullName", "pitcher_name"))),
    pitcher_hand = as.character(.column_or_default(raw, c("matchup.pitchHand.code", "pitcher_hand"))),
    batter_id = as.character(.column_or_default(raw, c("matchup.batter.id", "batter_id"))),
    batter_name = as.character(.column_or_default(raw, c("matchup.batter.fullName", "batter_name"))),
    batter_side = as.character(.column_or_default(raw, c("matchup.batSide.code", "batter_side"))),
    balls_before = .integer_value(.column_or_default(raw, c("count.balls.start", "balls_before"))),
    strikes_before = .integer_value(.column_or_default(raw, c("count.strikes.start", "strikes_before"))),
    outs_before = .integer_value(.column_or_default(raw, c("count.outs.start", "outs_before"))),
    balls_after = .integer_value(.column_or_default(raw, c("count.balls.end", "balls_after"))),
    strikes_after = .integer_value(.column_or_default(raw, c("count.strikes.end", "strikes_after"))),
    outs_after = .integer_value(.column_or_default(raw, c("count.outs.end", "outs_after"))),
    runner_on_first = !is.na(.column_or_default(raw, c("matchup.postOnFirst.id", "runner_on_first_id"))),
    runner_on_second = !is.na(.column_or_default(raw, c("matchup.postOnSecond.id", "runner_on_second_id"))),
    runner_on_third = !is.na(.column_or_default(raw, c("matchup.postOnThird.id", "runner_on_third_id"))),
    event_type = as.character(.column_or_default(raw, c("result.eventType", "details.eventType", "event_type"))),
    event = as.character(.column_or_default(raw, c("result.event", "details.event", "event"))),
    description = as.character(.column_or_default(raw, c("details.description", "result.description", "description"))),
    pitch_type = as.character(.column_or_default(raw, c("details.type.code", "pitch_type"))),
    pitch_name = as.character(.column_or_default(raw, c("details.type.description", "pitch_name"))),
    call_code = as.character(.column_or_default(raw, c("details.call.code", "call_code"))),
    call_description = as.character(.column_or_default(raw, c("details.call.description", "call_description"))),
    is_in_play = .logical_value(.column_or_default(raw, c("details.isInPlay", "is_in_play"), FALSE)),
    is_strike = .logical_value(.column_or_default(raw, c("details.isStrike", "is_strike"), FALSE)),
    is_ball = .logical_value(.column_or_default(raw, c("details.isBall", "is_ball"), FALSE)),
    is_terminal_pitch = .logical_value(.column_or_default(raw, c("last.pitch.of.ab", "last_pitch_of_ab"), FALSE)),
    start_speed = .numeric_value(.column_or_default(raw, c("pitchData.startSpeed", "release_speed"))),
    end_speed = .numeric_value(.column_or_default(raw, c("pitchData.endSpeed", "end_speed"))),
    zone = .integer_value(.column_or_default(raw, c("pitchData.zone", "zone"))),
    strike_zone_top = .numeric_value(.column_or_default(raw, c("pitchData.strikeZoneTop", "strike_zone_top"))),
    strike_zone_bottom = .numeric_value(.column_or_default(raw, c("pitchData.strikeZoneBottom", "strike_zone_bottom"))),
    extension = .numeric_value(.column_or_default(raw, c("pitchData.extension", "extension"))),
    plate_x = .numeric_value(.column_or_default(raw, c("pitchData.coordinates.pX", "plate_x"))),
    plate_z = .numeric_value(.column_or_default(raw, c("pitchData.coordinates.pZ", "plate_z"))),
    release_x = .numeric_value(.column_or_default(raw, c("pitchData.coordinates.x0", "release_x"))),
    release_z = .numeric_value(.column_or_default(raw, c("pitchData.coordinates.z0", "release_z"))),
    horizontal_break = .numeric_value(.column_or_default(raw, c("pitchData.breaks.breakHorizontal", "horizontal_break"))),
    induced_vertical_break = .numeric_value(.column_or_default(raw, c("pitchData.breaks.breakVerticalInduced", "induced_vertical_break"))),
    spin_rate = .numeric_value(.column_or_default(raw, c("pitchData.breaks.spinRate", "spin_rate"))),
    launch_speed = .numeric_value(.column_or_default(raw, c("hitData.launchSpeed", "launch_speed"))),
    launch_angle = .numeric_value(.column_or_default(raw, c("hitData.launchAngle", "launch_angle"))),
    hit_distance = .numeric_value(.column_or_default(raw, c("hitData.totalDistance", "hit_distance"))),
    hit_location = .integer_value(.column_or_default(raw, c("hitData.location", "hit_location"))),
    hit_coord_x = .numeric_value(.column_or_default(raw, c("hitData.coordinates.coordX", "hit_coord_x"))),
    hit_coord_y = .numeric_value(.column_or_default(raw, c("hitData.coordinates.coordY", "hit_coord_y"))),
    trajectory = as.character(.column_or_default(raw, c("hitData.trajectory", "trajectory"))),
    batting_order = .integer_value(.column_or_default(raw, c("battingOrder", "batting_order"))),
    runs_batted_in = .integer_value(.column_or_default(raw, c("result.rbi", "runs_batted_in"), 0L)),
    home_score_current = .integer_value(.column_or_default(raw, c("details.homeScore", "home_score_current"))),
    away_score_current = .integer_value(.column_or_default(raw, c("details.awayScore", "away_score_current"))),
    home_score_after = .integer_value(.column_or_default(raw, c("result.homeScore", "details.homeScore", "home_score"))),
    away_score_after = .integer_value(.column_or_default(raw, c("result.awayScore", "details.awayScore", "away_score")))
  )

  swing_calls <- c(
    "Swinging Strike", "Foul", "In play, out(s)", "In play, run(s)",
    "In play, no out", "Foul Tip", "Swinging Strike (Blocked)",
    "Foul Bunt", "Missed Bunt", "Swinging Pitchout"
  )
  whiff_calls <- c("Swinging Strike", "Swinging Strike (Blocked)", "Missed Bunt", "Swinging Pitchout")
  output$is_swing <- output$call_description %in% swing_calls
  output$event_key <- .normalize_event_key(output$event_type)
  output$is_whiff <- output$call_description %in% whiff_calls
  output$is_contact <- output$is_swing & !output$is_whiff
  output$is_called_strike <- output$call_description == "Called Strike"
  output$is_called_ball <- output$call_description %in% c("Ball", "Ball In Dirt", "Pitchout", "Hit By Pitch")
  output$is_zone <- !is.na(output$zone) & output$zone >= 1L & output$zone <= 9L
  output$is_out_of_zone <- !is.na(output$zone) & output$zone > 9L
  output$is_heart <- !is.na(output$zone) & output$zone == 5L
  output$is_chase <- output$is_swing & output$is_out_of_zone
  output$is_zone_swing <- output$is_swing & output$is_zone
  output$is_out_of_zone_contact <- output$is_contact & output$is_out_of_zone
  output$is_zone_contact <- output$is_contact & output$is_zone
  output$count_key <- paste0(output$balls_before, "-", output$strikes_before)
  output$is_two_strike <- !is.na(output$strikes_before) & output$strikes_before == 2L
  output$is_three_ball <- !is.na(output$balls_before) & output$balls_before == 3L

  required <- c("game_pk", "at_bat_index", "pitcher_id", "batter_id")
  for (column in required) {
    if (all(is.na(output[[column]]) | output[[column]] == "")) {
      stop("Canonical pitch view could not resolve `", column, "`.", call. = FALSE)
    }
  }

  output |>
    dplyr::arrange(.data$game_date, .data$game_pk, .data$at_bat_index, .data$pitch_number, .data$source_row) |>
    dplyr::group_by(.data$game_pk, .data$at_bat_index) |>
    dplyr::mutate(pitch_in_pa = dplyr::row_number()) |>
    dplyr::ungroup()
}

#' Build the canonical plate-appearance view
#'
#' @param pbp Raw BaseballR PBP or a canonical pitch view.
#'
#' @return One terminal row per plate appearance with event flags.
#' @export
build_plate_appearance_view <- function(pbp) {
  pitches <- if (all(c("at_bat_index", "pitch_in_pa", "pitcher_id") %in% names(pbp))) pbp else build_pitch_view(pbp)

  # Canonical pitch files are large. Sorting once and using contiguous run
  # lengths avoids several full-table copies made by grouped slice/join verbs.
  order_index <- order(
    pitches$game_date,
    pitches$game_pk,
    pitches$at_bat_index,
    pitches$pitch_number,
    pitches$source_row,
    na.last = TRUE
  )
  pitches <- pitches[order_index, , drop = FALSE]
  pa_key <- paste(pitches$game_pk, pitches$at_bat_index, sep = "\034")
  pa_runs <- rle(pa_key)
  terminal <- pitches[cumsum(pa_runs$lengths), , drop = FALSE]
  terminal$pitches_in_pa <- as.integer(pa_runs$lengths)
  first_in_game <- !duplicated(terminal$game_pk)
  terminal$home_score_before <- c(0L, terminal$home_score_after[-nrow(terminal)])
  terminal$away_score_before <- c(0L, terminal$away_score_after[-nrow(terminal)])
  terminal$home_score_before[first_in_game] <- 0L
  terminal$away_score_before[first_in_game] <- 0L
  pa_columns <- c(
    "source_row", "game_pk", "game_date", "at_bat_index", "pitch_number", "play_id",
    "inning", "half_inning", "is_top_inning", "batting_team", "fielding_team",
    "home_team", "away_team", "pitcher_id", "pitcher_name", "pitcher_hand",
    "batter_id", "batter_name", "batter_side", "outs_before", "outs_after",
    "runner_on_first", "runner_on_second", "runner_on_third", "event_type", "event",
    "event_key", "description", "pitch_type", "pitch_name", "is_in_play",
    "is_terminal_pitch", "launch_speed", "launch_angle", "hit_distance", "hit_location", "hit_coord_x", "hit_coord_y",
    "trajectory", "batting_order", "runs_batted_in", "home_score_before",
    "away_score_before", "home_score_after", "away_score_after", "pitches_in_pa"
  )
  terminal <- terminal[, pa_columns, drop = FALSE]

  event_key <- .normalize_event_key(terminal$event_type)
  hit_events <- c("single", "double", "triple", "home_run")
  walk_events <- c("walk", "intent_walk", "intentional_walk")
  no_at_bat_events <- c(walk_events, "hit_by_pitch", "sac_fly", "sac_bunt", "catcher_interf", "catcher_interference")

  terminal$event_key <- event_key
  terminal$is_hit <- event_key %in% hit_events
  terminal$is_home_run <- event_key == "home_run"
  terminal$is_walk <- event_key %in% walk_events
  terminal$is_hit_by_pitch <- event_key == "hit_by_pitch"
  terminal$is_strikeout <- event_key %in% c("strikeout", "strikeout_double_play")
  terminal$is_at_bat <- !event_key %in% no_at_bat_events & !is.na(event_key) & event_key != ""
  terminal$is_out <- event_key %in% c(
    "field_out", "force_out", "grounded_into_double_play", "double_play", "triple_play",
    "strikeout", "strikeout_double_play", "sac_fly", "sac_bunt", "fielders_choice_out"
  )
  terminal$is_batted_ball <- terminal$is_in_play | !is.na(terminal$launch_speed)
  terminal$is_hard_hit <- terminal$is_batted_ball & !is.na(terminal$launch_speed) & terminal$launch_speed >= 95
  barrel_lower <- pmax(26 - (terminal$launch_speed - 98), 8)
  barrel_upper <- pmin(30 + 2 * (terminal$launch_speed - 98), 50)
  terminal$is_barrel_proxy <- terminal$is_batted_ball & !is.na(terminal$launch_speed) &
    !is.na(terminal$launch_angle) & terminal$launch_speed >= 98 &
    terminal$launch_angle >= barrel_lower & terminal$launch_angle <= barrel_upper
  terminal$is_ground_ball <- terminal$trajectory %in% c("ground_ball", "bunt_grounder")
  terminal$is_line_drive <- terminal$trajectory %in% c("line_drive", "bunt_line_drive")
  terminal$is_fly_ball <- terminal$trajectory == "fly_ball"
  terminal$is_popup <- terminal$trajectory %in% c("popup", "bunt_popup")
  terminal$batted_ball_type <- dplyr::case_when(
    terminal$is_ground_ball ~ "ground_ball",
    terminal$is_line_drive ~ "line_drive",
    terminal$is_fly_ball ~ "fly_ball",
    terminal$is_popup ~ "popup",
    TRUE ~ NA_character_
  )
  terminal$spray_direction <- dplyr::case_when(
    terminal$batter_side == "R" & terminal$hit_location %in% c(5L, 6L, 7L) ~ "pull",
    terminal$batter_side == "R" & terminal$hit_location %in% c(8L, 78L, 89L) ~ "middle",
    terminal$batter_side == "R" & terminal$hit_location %in% c(9L, 4L, 3L) ~ "opposite",
    terminal$batter_side == "L" & terminal$hit_location %in% c(9L, 4L, 3L) ~ "pull",
    terminal$batter_side == "L" & terminal$hit_location %in% c(8L, 78L, 89L) ~ "middle",
    terminal$batter_side == "L" & terminal$hit_location %in% c(5L, 6L, 7L) ~ "opposite",
    TRUE ~ NA_character_
  )
  terminal$total_bases <- dplyr::case_when(
    event_key == "single" ~ 1,
    event_key == "double" ~ 2,
    event_key == "triple" ~ 3,
    event_key == "home_run" ~ 4,
    TRUE ~ 0
  )
  terminal$outs_on_play <- pmax(terminal$outs_after - terminal$outs_before, 0, na.rm = TRUE)
  terminal$fielding_score_diff <- ifelse(
    terminal$is_top_inning,
    terminal$home_score_after - terminal$away_score_after,
    terminal$away_score_after - terminal$home_score_after
  )
  terminal$batting_score_before <- ifelse(terminal$is_top_inning, terminal$away_score_before, terminal$home_score_before)
  terminal$fielding_score_before <- ifelse(terminal$is_top_inning, terminal$home_score_before, terminal$away_score_before)
  terminal$batting_score_after <- ifelse(terminal$is_top_inning, terminal$away_score_after, terminal$home_score_after)
  terminal$fielding_score_after <- ifelse(terminal$is_top_inning, terminal$home_score_after, terminal$away_score_after)
  terminal$batting_score_diff_before <- terminal$batting_score_before - terminal$fielding_score_before
  terminal$batting_score_diff_after <- terminal$batting_score_after - terminal$fielding_score_after
  score_change <- terminal$batting_score_after - terminal$batting_score_before
  terminal$runs_scored_on_play <- ifelse(
    is.finite(score_change),
    pmax(score_change, 0),
    pmax(terminal$runs_batted_in, 0, na.rm = TRUE)
  )

  terminal$runner_on_first_after <- terminal$runner_on_first
  terminal$runner_on_second_after <- terminal$runner_on_second
  terminal$runner_on_third_after <- terminal$runner_on_third
  half_key <- paste(terminal$game_pk, terminal$inning, terminal$half_inning, sep = "\034")
  first_in_half <- !duplicated(half_key)
  lag_runner <- function(x) {
    previous <- c(FALSE, x[-length(x)])
    previous[first_in_half] <- FALSE
    previous
  }
  terminal$runner_on_first_before <- lag_runner(terminal$runner_on_first_after)
  terminal$runner_on_second_before <- lag_runner(terminal$runner_on_second_after)
  terminal$runner_on_third_before <- lag_runner(terminal$runner_on_third_after)
  terminal$base_state_before <-
    as.integer(terminal$runner_on_first_before) +
    2L * as.integer(terminal$runner_on_second_before) +
    4L * as.integer(terminal$runner_on_third_before)
  terminal$base_state_after <-
    as.integer(terminal$runner_on_first_after) +
    2L * as.integer(terminal$runner_on_second_after) +
    4L * as.integer(terminal$runner_on_third_after)
  terminal$is_risp_before <- terminal$runner_on_second_before | terminal$runner_on_third_before
  terminal
}

#' Build the canonical batted-ball view
#'
#' @param pbp Raw PBP, canonical pitches, or canonical plate appearances.
#'
#' @return One row per batted-ball plate appearance.
#' @export
build_batted_ball_view <- function(pbp) {
  plate_appearances <- if ("is_batted_ball" %in% names(pbp)) pbp else build_plate_appearance_view(pbp)
  dplyr::filter(plate_appearances, .data$is_batted_ball %in% TRUE)
}

#' Build pitcher appearances from canonical MLB PBP
#'
#' @param pbp Raw PBP, canonical pitches, or canonical plate appearances.
#'
#' @return One row per pitcher per game with starter and workload fields.
#' @export
build_pitcher_appearance_view <- function(pbp) {
  is_pa_input <- all(c("pitches_in_pa", "is_hit", "outs_on_play") %in% names(pbp))
  if (is_pa_input) {
    plate_appearances <- pbp
    pitch_counts <- plate_appearances |>
      dplyr::group_by(.data$game_pk, .data$pitcher_id) |>
      dplyr::summarise(pitches = sum(.data$pitches_in_pa, na.rm = TRUE), .groups = "drop")
  } else {
    pitches <- if (all(c("pitch_in_pa", "pitcher_id") %in% names(pbp))) pbp else build_pitch_view(pbp)
    pitch_counts <- pitches |>
      dplyr::count(.data$game_pk, .data$pitcher_id, name = "pitches")

    order_index <- order(
      pitches$game_date, pitches$game_pk, pitches$at_bat_index,
      pitches$pitch_number, pitches$source_row, na.last = TRUE
    )
    ordered <- pitches[order_index, , drop = FALSE]
    pa_key <- paste(ordered$game_pk, ordered$at_bat_index, sep = "\034")
    terminal_index <- cumsum(rle(pa_key)$lengths)
    appearance_columns <- c(
      "game_pk", "game_date", "at_bat_index", "inning", "pitcher_id",
      "pitcher_name", "pitcher_hand", "fielding_team", "home_team", "away_team",
      "event_key", "outs_before", "outs_after", "is_in_play", "launch_speed"
    )
    plate_appearances <- ordered[terminal_index, appearance_columns, drop = FALSE]
    plate_appearances$is_hit <- plate_appearances$event_key %in% c("single", "double", "triple", "home_run")
    plate_appearances$is_home_run <- plate_appearances$event_key == "home_run"
    plate_appearances$is_walk <- plate_appearances$event_key %in% c("walk", "intent_walk", "intentional_walk")
    plate_appearances$is_strikeout <- plate_appearances$event_key %in% c("strikeout", "strikeout_double_play")
    plate_appearances$is_batted_ball <- plate_appearances$is_in_play | !is.na(plate_appearances$launch_speed)
    outs_change <- plate_appearances$outs_after - plate_appearances$outs_before
    plate_appearances$outs_on_play <- ifelse(is.finite(outs_change), pmax(outs_change, 0), 0)
  }

  appearance_columns <- c(
    "game_pk", "game_date", "at_bat_index", "inning", "pitcher_id",
    "pitcher_name", "pitcher_hand", "fielding_team", "home_team", "away_team",
    "outs_on_play", "is_hit", "is_home_run", "is_walk", "is_strikeout",
    "is_batted_ball"
  )
  plate_appearances <- plate_appearances[, appearance_columns, drop = FALSE]

  appearances <- plate_appearances |>
    dplyr::group_by(.data$game_pk, .data$pitcher_id) |>
    dplyr::summarise(
      game_date = min(.data$game_date, na.rm = TRUE),
      pitcher_name = .first_non_missing(.data$pitcher_name),
      pitcher_hand = .first_non_missing(.data$pitcher_hand),
      fielding_team = .first_non_missing(.data$fielding_team),
      home_team = .first_non_missing(.data$home_team),
      away_team = .first_non_missing(.data$away_team),
      appearance_start = min(.data$at_bat_index, na.rm = TRUE),
      first_inning = min(.data$inning, na.rm = TRUE),
      last_inning = max(.data$inning, na.rm = TRUE),
      batters_faced = dplyr::n(),
      outs_recorded = sum(.data$outs_on_play, na.rm = TRUE),
      hits = sum(.data$is_hit, na.rm = TRUE),
      home_runs = sum(.data$is_home_run, na.rm = TRUE),
      walks = sum(.data$is_walk, na.rm = TRUE),
      strikeouts = sum(.data$is_strikeout, na.rm = TRUE),
      batted_balls = sum(.data$is_batted_ball, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(pitch_counts, by = c("game_pk", "pitcher_id")) |>
    dplyr::group_by(.data$game_pk, .data$fielding_team) |>
    dplyr::mutate(
      appearance_order = dplyr::dense_rank(.data$appearance_start),
      is_starter = .data$appearance_order == 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$game_date, .data$game_pk, .data$fielding_team, .data$appearance_order)

  appearances$pitches[is.na(appearances$pitches)] <- 0L
  appearances
}
