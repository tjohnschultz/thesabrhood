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
#' @return A list containing pitch usage, production, pitch quality,
#'   batted-ball shape, and starter/bullpen workload data frames.
#' @export
build_rolling_league_trends <- function(pitches, plate_appearances, rolling_days = 14L) {
  stopifnot(is.data.frame(pitches), is.data.frame(plate_appearances))
  rolling_days <- as.integer(rolling_days[[1L]])
  if (is.na(rolling_days) || rolling_days < 1L) stop("`rolling_days` must be positive.", call. = FALSE)
  pitch_required <- c("game_date", "pitch_type", "pitch_name")
  pa_required <- c("game_date", "is_at_bat", "is_hit", "is_home_run", "is_walk", "is_strikeout", "total_bases", "is_batted_ball", "is_hard_hit")
  if (length(setdiff(pitch_required, names(pitches)))) stop("`pitches` is missing league-trend fields.", call. = FALSE)
  if (length(setdiff(pa_required, names(plate_appearances)))) stop("`plate_appearances` is missing league-trend fields.", call. = FALSE)
  for (column in c("start_speed", "horizontal_break", "induced_vertical_break")) {
    if (!column %in% names(pitches)) pitches[[column]] <- NA_real_
  }
  for (column in c("launch_speed", "launch_angle")) {
    if (!column %in% names(plate_appearances)) plate_appearances[[column]] <- NA_real_
  }
  for (column in c("is_ground_ball", "is_fly_ball")) {
    if (!column %in% names(plate_appearances)) plate_appearances[[column]] <- FALSE
  }

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

  quality_source <- pitch_data |>
    dplyr::group_by(.data$game_date, .data$pitch_type, .data$pitch_name) |>
    dplyr::summarise(
      pitches = dplyr::n(),
      speed_sum = sum(.data$start_speed, na.rm = TRUE),
      speed_n = sum(is.finite(.data$start_speed)),
      hbreak_sum = sum(.data$horizontal_break, na.rm = TRUE),
      hbreak_n = sum(is.finite(.data$horizontal_break)),
      ivb_sum = sum(.data$induced_vertical_break, na.rm = TRUE),
      ivb_n = sum(is.finite(.data$induced_vertical_break)),
      .groups = "drop"
    )
  quality_rows <- lapply(pitch_dates, function(target) {
    window <- quality_source[.rolling_window_indices(quality_source$game_date, target, rolling_days), , drop = FALSE]
    output <- window |>
      dplyr::group_by(.data$pitch_type) |>
      dplyr::summarise(
        pitch_name = names(sort(table(.data$pitch_name), decreasing = TRUE))[[1L]],
        pitches = sum(.data$pitches),
        average_velocity = sum(.data$speed_sum) / pmax(sum(.data$speed_n), 1),
        average_horizontal_break = sum(.data$hbreak_sum) / pmax(sum(.data$hbreak_n), 1),
        average_induced_vertical_break = sum(.data$ivb_sum) / pmax(sum(.data$ivb_n), 1),
        .groups = "drop"
      )
    output$date <- target
    output
  })
  pitch_quality <- dplyr::bind_rows(quality_rows)
  pitch_quality$rolling_days <- rolling_days
  pitch_quality$trend_method <- "calendar_day_rolling_pitch_quality_v1"

  batted_daily <- pa_data |>
    dplyr::group_by(.data$game_date) |>
    dplyr::summarise(
      batted_balls = sum(.data$is_batted_ball, na.rm = TRUE),
      hard_hit = sum(.data$is_hard_hit, na.rm = TRUE),
      ground_balls = sum(.data$is_ground_ball, na.rm = TRUE),
      fly_balls = sum(.data$is_fly_ball, na.rm = TRUE),
      launch_speed_sum = sum(.data$launch_speed, na.rm = TRUE),
      launch_speed_n = sum(is.finite(.data$launch_speed)),
      launch_angle_sum = sum(.data$launch_angle, na.rm = TRUE),
      launch_angle_n = sum(is.finite(.data$launch_angle)),
      .groups = "drop"
    )
  batted_rows <- lapply(pa_dates, function(target) {
    window <- batted_daily[.rolling_window_indices(batted_daily$game_date, target, rolling_days), , drop = FALSE]
    totals <- colSums(window[, setdiff(names(window), "game_date"), drop = FALSE], na.rm = TRUE)
    data.frame(date = target, as.list(totals), stringsAsFactors = FALSE)
  })
  batted_ball <- dplyr::bind_rows(batted_rows)
  batted_ball$average_exit_velocity <- batted_ball$launch_speed_sum / pmax(batted_ball$launch_speed_n, 1)
  batted_ball$average_launch_angle <- batted_ball$launch_angle_sum / pmax(batted_ball$launch_angle_n, 1)
  batted_ball$hard_hit_rate <- .safe_rate(batted_ball$hard_hit, batted_ball$batted_balls)
  batted_ball$ground_ball_rate <- .safe_rate(batted_ball$ground_balls, batted_ball$batted_balls)
  batted_ball$fly_ball_rate <- .safe_rate(batted_ball$fly_balls, batted_ball$batted_balls)
  batted_ball$rolling_days <- rolling_days
  batted_ball$trend_method <- "calendar_day_rolling_batted_ball_shape_v1"

  workload_required <- c("game_pk", "fielding_team", "pitcher_id", "at_bat_index")
  if (all(workload_required %in% names(plate_appearances))) {
    pa_order <- order(plate_appearances$game_date, plate_appearances$game_pk, plate_appearances$at_bat_index)
    workload_pa <- plate_appearances[pa_order, , drop = FALSE]
    staff_key <- paste(workload_pa$game_pk, workload_pa$fielding_team, sep = "\034")
    starter_id <- ave(as.character(workload_pa$pitcher_id), staff_key, FUN = function(value) value[[1L]])
    workload_pa$is_starter_pa <- as.character(workload_pa$pitcher_id) == starter_id
    workload_daily <- workload_pa |>
      dplyr::group_by(.data$game_date) |>
      dplyr::summarise(
        plate_appearances = dplyr::n(),
        starter_plate_appearances = sum(.data$is_starter_pa, na.rm = TRUE),
        .groups = "drop"
      )
    workload_rows <- lapply(pa_dates, function(target) {
      window <- workload_daily[.rolling_window_indices(workload_daily$game_date, target, rolling_days), , drop = FALSE]
      data.frame(
        date = target,
        plate_appearances = sum(window$plate_appearances),
        starter_plate_appearances = sum(window$starter_plate_appearances),
        stringsAsFactors = FALSE
      )
    })
    workload <- dplyr::bind_rows(workload_rows)
    workload$starter_pa_share <- .safe_rate(workload$starter_plate_appearances, workload$plate_appearances)
    workload$bullpen_pa_share <- 1 - workload$starter_pa_share
    workload$rolling_days <- rolling_days
    workload$trend_method <- "calendar_day_rolling_starter_bullpen_workload_v1"
  } else {
    workload <- tibble::tibble()
  }

  rownames(pitch_output) <- NULL
  rownames(production) <- NULL
  list(
    pitch_usage = pitch_output,
    production = production,
    pitch_quality = pitch_quality,
    batted_ball = batted_ball,
    workload = workload
  )
}
