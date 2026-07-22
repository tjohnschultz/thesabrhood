.rolling_window_indices <- function(dates, target, days) {
  dates >= target - (days - 1L) & dates <= target
}

#' Build rolling MLB-wide production and pitch-usage trends
#'
#' Aggregates canonical pitch and plate-appearance views into calendar-day
#' rolling windows. The products are designed for league context graphics, not
#' player evaluation.
#'
#' @param pitches Canonical pitch view from [build_pitch_view()].
#' @param plate_appearances Canonical plate-appearance view from
#'   [build_plate_appearance_view()].
#' @param rolling_days Positive rolling calendar window.
#' @return A list containing `pitch_usage` and `production` data frames.
#' @export
build_rolling_league_trends <- function(pitches, plate_appearances, rolling_days = 14L) {
  stopifnot(is.data.frame(pitches), is.data.frame(plate_appearances))
  rolling_days <- as.integer(rolling_days[[1L]])
  if (is.na(rolling_days) || rolling_days < 1L) stop("`rolling_days` must be positive.", call. = FALSE)
  pitch_required <- c("game_date", "pitch_type", "pitch_name")
  pa_required <- c("game_date", "is_at_bat", "is_hit", "is_home_run", "is_walk", "is_strikeout", "total_bases", "is_batted_ball", "is_hard_hit")
  if (length(setdiff(pitch_required, names(pitches)))) stop("`pitches` is missing league-trend fields.", call. = FALSE)
  if (length(setdiff(pa_required, names(plate_appearances)))) stop("`plate_appearances` is missing league-trend fields.", call. = FALSE)

  pitch_data <- pitches[!is.na(pitches$pitch_type) & pitches$pitch_type != "" & !is.na(pitches$game_date), , drop = FALSE]
  pitch_data$game_date <- as.Date(pitch_data$game_date)
  pitch_daily <- pitch_data |>
    dplyr::count(.data$game_date, .data$pitch_type, .data$pitch_name, name = "pitches")
  pitch_dates <- sort(unique(pitch_daily$game_date))
  pitch_rows <- lapply(pitch_dates, function(target) {
    window <- pitch_daily[.rolling_window_indices(pitch_daily$game_date, target, rolling_days), , drop = FALSE]
    summarized <- window |>
      dplyr::group_by(.data$pitch_type) |>
      dplyr::summarise(
        pitch_name = names(sort(table(.data$pitch_name), decreasing = TRUE))[[1L]],
        pitches_rolling = sum(.data$pitches),
        .groups = "drop"
      )
    summarized$date <- target
    summarized$all_pitches_rolling <- sum(summarized$pitches_rolling)
    summarized$usage_rate_rolling <- summarized$pitches_rolling / summarized$all_pitches_rolling
    summarized
  })
  pitch_output <- do.call(rbind, pitch_rows)
  season_counts <- stats::aggregate(pitches_rolling ~ pitch_type, pitch_output[pitch_output$date == max(pitch_output$date), ], identity)
  full_counts <- table(pitch_data$pitch_type)
  pitch_output$season_usage_rate <- as.numeric(full_counts[pitch_output$pitch_type]) / sum(full_counts)
  pitch_output$rolling_days <- rolling_days
  pitch_output$trend_method <- "calendar_day_rolling_pitch_usage_v1"
  pitch_output <- pitch_output[order(pitch_output$date, -pitch_output$usage_rate_rolling), , drop = FALSE]

  pa_data <- plate_appearances[!is.na(plate_appearances$game_date), , drop = FALSE]
  pa_data$game_date <- as.Date(pa_data$game_date)
  pa_daily <- pa_data |>
    dplyr::group_by(.data$game_date) |>
    dplyr::summarise(
      pa = dplyr::n(), ab = sum(.data$is_at_bat, na.rm = TRUE), hits = sum(.data$is_hit, na.rm = TRUE),
      home_runs = sum(.data$is_home_run, na.rm = TRUE), walks = sum(.data$is_walk, na.rm = TRUE),
      strikeouts = sum(.data$is_strikeout, na.rm = TRUE), total_bases = sum(.data$total_bases, na.rm = TRUE),
      batted_balls = sum(.data$is_batted_ball, na.rm = TRUE), hard_hit = sum(.data$is_hard_hit, na.rm = TRUE),
      .groups = "drop"
    )
  pa_dates <- sort(unique(pa_daily$game_date))
  production_rows <- lapply(pa_dates, function(target) {
    window <- pa_daily[.rolling_window_indices(pa_daily$game_date, target, rolling_days), , drop = FALSE]
    totals <- colSums(window[, setdiff(names(window), "game_date"), drop = FALSE], na.rm = TRUE)
    data.frame(date = target, as.list(totals), stringsAsFactors = FALSE)
  })
  production <- do.call(rbind, production_rows)
  production$batting_average <- .safe_rate(production$hits, production$ab)
  production$slugging_percentage <- .safe_rate(production$total_bases, production$ab)
  production$home_run_rate <- .safe_rate(production$home_runs, production$pa)
  production$strikeout_rate <- .safe_rate(production$strikeouts, production$pa)
  production$walk_rate <- .safe_rate(production$walks, production$pa)
  production$hard_hit_rate <- .safe_rate(production$hard_hit, production$batted_balls)
  production$rolling_days <- rolling_days
  production$trend_method <- "calendar_day_rolling_league_production_v1"
  rownames(pitch_output) <- NULL
  rownames(production) <- NULL
  list(pitch_usage = pitch_output, production = production)
}
