.tracking_rate <- function(numerator, denominator) {
  denominator <- suppressWarnings(as.numeric(denominator))
  numerator <- suppressWarnings(as.numeric(numerator))
  ifelse(is.finite(denominator) & denominator > 0, numerator / denominator, NA_real_)
}

.tracking_max <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (length(value)) max(value) else NA_real_
}

.tracking_mean <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (length(value)) mean(value) else NA_real_
}

#' Build velocity-event leaderboards from canonical pitch data
#'
#' Creates compact player and team totals for pitches at least 100 mph and
#' tracked batted balls at least 100 mph. Both counts and opportunity rates are
#' retained so the public boards can show volume without hiding playing time.
#'
#' @param pitches Canonical pitch view from [build_pitch_view()].
#' @param plate_appearances Canonical plate-appearance view.
#' @return A list containing hitter, pitcher, and team tracking tables.
#' @export
build_tracking_event_leaderboards <- function(pitches, plate_appearances) {
  pitch_required <- c(
    "pitcher_id", "pitcher_name", "fielding_team", "start_speed",
    "batter_id", "batter_name", "batting_team"
  )
  pa_required <- c(
    "batter_id", "batter_name", "batting_team", "pitcher_id",
    "pitcher_name", "fielding_team", "is_batted_ball", "launch_speed"
  )
  pitch_missing <- setdiff(pitch_required, names(pitches))
  pa_missing <- setdiff(pa_required, names(plate_appearances))
  if (length(pitch_missing)) {
    stop("Pitches are missing: ", paste(pitch_missing, collapse = ", "), call. = FALSE)
  }
  if (length(pa_missing)) {
    stop("Plate appearances are missing: ", paste(pa_missing, collapse = ", "), call. = FALSE)
  }

  pitcher_velocity <- pitches |>
    dplyr::filter(!is.na(.data$pitcher_id), .data$pitcher_id != "") |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(
      player_name = .last_non_missing(.data$pitcher_name),
      team = .last_non_missing(.data$fielding_team),
      total_pitches = dplyr::n(),
      tracked_pitches = sum(is.finite(.data$start_speed)),
      pitches_100_plus = sum(is.finite(.data$start_speed) & .data$start_speed >= 100),
      average_velocity = .tracking_mean(.data$start_speed),
      max_velocity = .tracking_max(.data$start_speed),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pitches_100_plus_rate = .tracking_rate(.data$pitches_100_plus, .data$tracked_pitches),
      perspective = "pitcher",
      tracking_method = "mlbam_release_speed_100_mph_threshold_v1"
    ) |>
    dplyr::arrange(dplyr::desc(.data$pitches_100_plus), dplyr::desc(.data$max_velocity))

  contact <- plate_appearances |>
    dplyr::filter(.data$is_batted_ball %in% TRUE, is.finite(.data$launch_speed))

  hitter_contact <- contact |>
    dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
    dplyr::group_by(.data$batter_id) |>
    dplyr::summarise(
      player_name = .last_non_missing(.data$batter_name),
      team = .last_non_missing(.data$batting_team),
      tracked_batted_balls = dplyr::n(),
      batted_balls_100_plus = sum(.data$launch_speed >= 100),
      average_exit_velocity = .tracking_mean(.data$launch_speed),
      max_exit_velocity = .tracking_max(.data$launch_speed),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      batted_balls_100_plus_rate = .tracking_rate(.data$batted_balls_100_plus, .data$tracked_batted_balls),
      perspective = "batter",
      tracking_method = "mlbam_launch_speed_100_mph_threshold_v1"
    ) |>
    dplyr::arrange(dplyr::desc(.data$batted_balls_100_plus), dplyr::desc(.data$max_exit_velocity))

  pitcher_contact <- contact |>
    dplyr::filter(!is.na(.data$pitcher_id), .data$pitcher_id != "") |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(
      player_name = .last_non_missing(.data$pitcher_name),
      team = .last_non_missing(.data$fielding_team),
      tracked_batted_balls_allowed = dplyr::n(),
      batted_balls_100_plus_allowed = sum(.data$launch_speed >= 100),
      average_exit_velocity_allowed = .tracking_mean(.data$launch_speed),
      max_exit_velocity_allowed = .tracking_max(.data$launch_speed),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      batted_balls_100_plus_allowed_rate = .tracking_rate(
        .data$batted_balls_100_plus_allowed,
        .data$tracked_batted_balls_allowed
      )
    )

  pitcher_totals <- dplyr::left_join(
    pitcher_velocity,
    pitcher_contact,
    by = c("pitcher_id", "player_name", "team")
  )

  team_pitch_velocity <- pitcher_velocity |>
    dplyr::group_by(.data$team) |>
    dplyr::summarise(
      total_pitches = sum(.data$total_pitches, na.rm = TRUE),
      tracked_pitches = sum(.data$tracked_pitches, na.rm = TRUE),
      pitches_100_plus = sum(.data$pitches_100_plus, na.rm = TRUE),
      max_velocity = .tracking_max(.data$max_velocity),
      .groups = "drop"
    )
  team_contact_for <- hitter_contact |>
    dplyr::group_by(.data$team) |>
    dplyr::summarise(
      tracked_batted_balls = sum(.data$tracked_batted_balls, na.rm = TRUE),
      batted_balls_100_plus = sum(.data$batted_balls_100_plus, na.rm = TRUE),
      max_exit_velocity = .tracking_max(.data$max_exit_velocity),
      .groups = "drop"
    )
  team_contact_against <- pitcher_contact |>
    dplyr::group_by(.data$team) |>
    dplyr::summarise(
      tracked_batted_balls_allowed = sum(.data$tracked_batted_balls_allowed, na.rm = TRUE),
      batted_balls_100_plus_allowed = sum(.data$batted_balls_100_plus_allowed, na.rm = TRUE),
      .groups = "drop"
    )
  teams <- dplyr::full_join(team_contact_for, team_pitch_velocity, by = "team") |>
    dplyr::full_join(team_contact_against, by = "team") |>
    dplyr::mutate(
      batted_balls_100_plus_rate = .tracking_rate(.data$batted_balls_100_plus, .data$tracked_batted_balls),
      pitches_100_plus_rate = .tracking_rate(.data$pitches_100_plus, .data$tracked_pitches),
      batted_balls_100_plus_allowed_rate = .tracking_rate(
        .data$batted_balls_100_plus_allowed,
        .data$tracked_batted_balls_allowed
      ),
      tracking_method = "mlbam_team_velocity_events_v1"
    ) |>
    dplyr::arrange(dplyr::desc(.data$batted_balls_100_plus))

  list(
    hitters = tibble::as_tibble(hitter_contact),
    pitchers = tibble::as_tibble(pitcher_totals),
    teams = tibble::as_tibble(teams)
  )
}
