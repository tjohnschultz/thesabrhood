#' Find the largest recent pitch-usage and shape changes
#'
#' @param pitches Canonical pitch view from [build_pitch_view()].
#' @param recent_games Number of each pitcher's latest games in the recent window.
#' @param minimum_recent_pitches Minimum pitch-type sample in the recent window.
#' @param minimum_baseline_pitches Minimum pitch-type sample before the recent window.
#'
#' @return One row per qualified pitcher and pitch type, ranked by contextual change.
#' @export
build_pitch_usage_change_board <- function(
    pitches,
    recent_games = 5L,
    minimum_recent_pitches = 15L,
    minimum_baseline_pitches = 40L) {
  required <- c(
    "game_pk", "game_date", "pitcher_id", "pitcher_name", "fielding_team",
    "pitch_type", "pitch_name", "start_speed", "horizontal_break",
    "induced_vertical_break", "is_whiff", "is_swing", "is_chase"
  )
  missing <- setdiff(required, names(pitches))
  if (length(missing)) stop("Pitch data are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  recent_games <- .integer_value(recent_games)[[1L]]
  if (is.na(recent_games) || recent_games < 1L) stop("`recent_games` must be positive.", call. = FALSE)

  data <- pitches |>
    dplyr::filter(
      !is.na(.data$pitcher_id), .data$pitcher_id != "",
      !is.na(.data$pitch_type), .data$pitch_type != ""
    ) |>
    dplyr::mutate(game_date = as.Date(.data$game_date))
  games <- data |>
    dplyr::distinct(.data$pitcher_id, .data$game_pk, .data$game_date) |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::arrange(.data$game_date, .data$game_pk, .by_group = TRUE) |>
    dplyr::mutate(
      game_number = dplyr::row_number(),
      games_played = dplyr::n(),
      window = ifelse(.data$game_number > .data$games_played - recent_games, "recent", "baseline")
    ) |>
    dplyr::ungroup()
  data <- dplyr::left_join(data, games, by = c("pitcher_id", "game_pk", "game_date")) |>
    dplyr::filter(.data$games_played > recent_games)
  if (nrow(data) == 0L) return(tibble::tibble())

  totals <- data |>
    dplyr::count(.data$pitcher_id, .data$window, name = "window_pitches")
  summaries <- data |>
    dplyr::group_by(.data$pitcher_id, .data$window, .data$pitch_type) |>
    dplyr::summarise(
      pitcher_name = .last_non_missing(.data$pitcher_name),
      team = .last_non_missing(.data$fielding_team),
      pitch_name = .last_non_missing(.data$pitch_name),
      pitches = dplyr::n(),
      average_velocity = mean(.numeric_value(.data$start_speed), na.rm = TRUE),
      average_horizontal_break = mean(.numeric_value(.data$horizontal_break), na.rm = TRUE),
      average_induced_vertical_break = mean(.numeric_value(.data$induced_vertical_break), na.rm = TRUE),
      swings = sum(.data$is_swing %in% TRUE, na.rm = TRUE),
      whiffs = sum(.data$is_whiff %in% TRUE, na.rm = TRUE),
      chases = sum(.data$is_chase %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(totals, by = c("pitcher_id", "window")) |>
    dplyr::mutate(
      usage = .data$pitches / .data$window_pitches,
      whiff_rate = ifelse(.data$swings > 0, .data$whiffs / .data$swings, NA_real_)
    )

  recent <- summaries |>
    dplyr::filter(.data$window == "recent") |>
    dplyr::select(-.data$window) |>
    dplyr::rename_with(~ paste0("recent_", .x), -c("pitcher_id", "pitch_type"))
  baseline <- summaries |>
    dplyr::filter(.data$window == "baseline") |>
    dplyr::select(-.data$window) |>
    dplyr::rename_with(~ paste0("baseline_", .x), -c("pitcher_id", "pitch_type"))
  output <- dplyr::inner_join(recent, baseline, by = c("pitcher_id", "pitch_type")) |>
    dplyr::filter(
      .data$recent_pitches >= minimum_recent_pitches,
      .data$baseline_pitches >= minimum_baseline_pitches
    ) |>
    dplyr::mutate(
      pitcher_name = .data$recent_pitcher_name,
      team = .data$recent_team,
      pitch_name = .data$recent_pitch_name,
      usage_delta = .data$recent_usage - .data$baseline_usage,
      usage_delta_pp = 100 * .data$usage_delta,
      velocity_delta = .data$recent_average_velocity - .data$baseline_average_velocity,
      horizontal_break_delta = .data$recent_average_horizontal_break - .data$baseline_average_horizontal_break,
      ivb_delta = .data$recent_average_induced_vertical_break - .data$baseline_average_induced_vertical_break,
      whiff_delta = .data$recent_whiff_rate - .data$baseline_whiff_rate,
      sample_reliability = pmin(.data$recent_pitches / 50, 1) * pmin(.data$baseline_pitches / 150, 1)
    ) |>
    dplyr::group_by(.data$pitch_type) |>
    dplyr::mutate(
      usage_change_z = ifelse(
        stats::sd(.data$usage_delta, na.rm = TRUE) > 0,
        (.data$usage_delta - mean(.data$usage_delta, na.rm = TRUE)) / stats::sd(.data$usage_delta, na.rm = TRUE),
        0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      contextual_change = abs(.data$usage_change_z) * (0.50 + 0.50 * .data$sample_reliability),
      change_signal_score = round(100 * dplyr::percent_rank(.data$contextual_change), 1),
      direction = dplyr::case_when(
        .data$usage_delta_pp >= 2 ~ "usage up",
        .data$usage_delta_pp <= -2 ~ "usage down",
        TRUE ~ "stable usage"
      ),
      trend_headline = paste0(
        .data$pitcher_name, " ", .data$pitch_name, ": ",
        ifelse(.data$usage_delta_pp >= 0, "+", ""), round(.data$usage_delta_pp, 1),
        " points of usage over the last ", recent_games, " games"
      ),
      trend_method = "recent_games_vs_prior_season_pitch_mix_z_v1",
      interpretation_warning = "Descriptive usage change; role, opponent mix, health, and pitch classification can contribute."
    ) |>
    dplyr::arrange(dplyr::desc(.data$change_signal_score), dplyr::desc(abs(.data$usage_delta_pp)))
  output$change_rank <- seq_len(nrow(output))
  output
}

