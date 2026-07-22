.safe_rate <- function(numerator, denominator) {
  ifelse(!is.na(denominator) & denominator > 0, numerator / denominator, NA_real_)
}

.mean_or_na <- function(x) {
  available <- x[is.finite(x)]
  if (length(available) == 0L) NA_real_ else mean(available)
}

.prepare_player_summary_data <- function(plate_appearances, perspective) {
  perspective <- match.arg(perspective, c("batter", "pitcher"))
  required <- c(
    "event_key", "is_at_bat", "is_hit", "is_home_run", "is_walk",
    "is_hit_by_pitch", "is_strikeout", "total_bases", "is_batted_ball",
    "game_date", "launch_speed", "launch_angle", "is_hard_hit",
    "is_barrel_proxy", "is_ground_ball", "is_line_drive", "is_fly_ball",
    "is_popup", "spray_direction"
  )
  missing <- setdiff(required, names(plate_appearances))
  if (length(missing) > 0L) stop("Plate appearances are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  if (perspective == "batter") {
    player_id <- plate_appearances$batter_id
    player_name <- plate_appearances$batter_name
    team <- plate_appearances$batting_team
    hand <- plate_appearances$batter_side
    opponent_hand <- plate_appearances$pitcher_hand
  } else {
    player_id <- plate_appearances$pitcher_id
    player_name <- plate_appearances$pitcher_name
    team <- plate_appearances$fielding_team
    hand <- plate_appearances$pitcher_hand
    opponent_hand <- plate_appearances$batter_side
  }
  run_value <- if ("run_value" %in% names(plate_appearances)) plate_appearances$run_value else rep(NA_real_, nrow(plate_appearances))
  tibble::tibble(
    player_id = player_id,
    player_name = player_name,
    team = team,
    hand = hand,
    opponent_hand = opponent_hand,
    game_date = plate_appearances$game_date,
    event_key = plate_appearances$event_key,
    is_at_bat = plate_appearances$is_at_bat,
    is_hit = plate_appearances$is_hit,
    is_home_run = plate_appearances$is_home_run,
    is_walk = plate_appearances$is_walk,
    is_hit_by_pitch = plate_appearances$is_hit_by_pitch,
    is_strikeout = plate_appearances$is_strikeout,
    total_bases = plate_appearances$total_bases,
    is_batted_ball = plate_appearances$is_batted_ball,
    launch_speed = plate_appearances$launch_speed,
    launch_angle = plate_appearances$launch_angle,
    is_hard_hit = plate_appearances$is_hard_hit,
    is_barrel_proxy = plate_appearances$is_barrel_proxy,
    is_ground_ball = plate_appearances$is_ground_ball,
    is_line_drive = plate_appearances$is_line_drive,
    is_fly_ball = plate_appearances$is_fly_ball,
    is_popup = plate_appearances$is_popup,
    spray_direction = plate_appearances$spray_direction,
    run_value = run_value
  )
}

#' Summarize canonical batter or pitcher performance
#'
#' @param plate_appearances Canonical, preferably run-expectancy-enriched plate appearances.
#' @param perspective `batter` for offensive results or `pitcher` for results allowed.
#' @param group_by Optional canonical columns for splits, such as `opponent_hand`.
#' @param minimum_pa Minimum plate appearances retained.
#' @param as_of_date Optional inclusive information cutoff.
#'
#' @return One row per player and requested split with outcomes, contact quality,
#'   run value, and sample-reliability fields.
#' @export
summarize_player_performance <- function(
    plate_appearances,
    perspective = c("batter", "pitcher"),
    group_by = NULL,
    minimum_pa = 1L,
    as_of_date = NULL) {
  perspective <- match.arg(perspective)
  data <- .prepare_player_summary_data(plate_appearances, perspective)

  group_by <- unique(as.character(group_by))
  group_by <- group_by[!is.na(group_by) & nzchar(group_by)]
  missing_groups <- setdiff(group_by, names(plate_appearances))
  missing_groups <- setdiff(missing_groups, names(data))
  if (length(missing_groups) > 0L) stop("Unknown summary groups: ", paste(missing_groups, collapse = ", "), call. = FALSE)
  for (column in setdiff(group_by, names(data))) data[[column]] <- plate_appearances[[column]]
  if (!is.null(as_of_date)) data <- data[as.Date(data$game_date) <= as.Date(as_of_date), , drop = FALSE]

  minimum_pa <- .integer_value(minimum_pa)[[1L]]
  if (is.na(minimum_pa) || minimum_pa < 1L) stop("`minimum_pa` must be positive.", call. = FALSE)
  grouping <- c("player_id", group_by)

  output <- data |>
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::summarise(
      player_name = .last_non_missing(.data$player_name),
      team = .last_non_missing(.data$team),
      hand = .last_non_missing(.data$hand),
      first_game = min(as.Date(.data$game_date), na.rm = TRUE),
      last_game = max(as.Date(.data$game_date), na.rm = TRUE),
      pa = dplyr::n(),
      ab = sum(.data$is_at_bat, na.rm = TRUE),
      hits = sum(.data$is_hit, na.rm = TRUE),
      singles = sum(.data$event_key == "single", na.rm = TRUE),
      doubles = sum(.data$event_key == "double", na.rm = TRUE),
      triples = sum(.data$event_key == "triple", na.rm = TRUE),
      home_runs = sum(.data$is_home_run, na.rm = TRUE),
      walks = sum(.data$is_walk, na.rm = TRUE),
      intentional_walks = sum(.data$event_key %in% c("intent_walk", "intentional_walk"), na.rm = TRUE),
      hit_by_pitch = sum(.data$is_hit_by_pitch, na.rm = TRUE),
      strikeouts = sum(.data$is_strikeout, na.rm = TRUE),
      sacrifice_flies = sum(.data$event_key %in% c("sac_fly", "sac_fly_double_play"), na.rm = TRUE),
      total_bases = sum(.data$total_bases, na.rm = TRUE),
      batted_balls = sum(.data$is_batted_ball, na.rm = TRUE),
      hard_hit = sum(.data$is_hard_hit, na.rm = TRUE),
      barrel_proxy = sum(.data$is_barrel_proxy, na.rm = TRUE),
      ground_balls = sum(.data$is_ground_ball, na.rm = TRUE),
      line_drives = sum(.data$is_line_drive, na.rm = TRUE),
      fly_balls = sum(.data$is_fly_ball, na.rm = TRUE),
      popups = sum(.data$is_popup, na.rm = TRUE),
      pulled_batted_balls = sum(.data$spray_direction == "pull", na.rm = TRUE),
      average_exit_velocity = .mean_or_na(.data$launch_speed[.data$is_batted_ball]),
      average_launch_angle = .mean_or_na(.data$launch_angle[.data$is_batted_ball]),
      run_value_total = sum(.data$run_value, na.rm = TRUE),
      run_value_opportunities = sum(is.finite(.data$run_value)),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$pa >= minimum_pa)

  obp_denominator <- output$ab + output$walks + output$hit_by_pitch + output$sacrifice_flies
  woba_denominator <- output$ab + output$walks - output$intentional_walks +
    output$hit_by_pitch + output$sacrifice_flies
  output$batting_average <- .safe_rate(output$hits, output$ab)
  output$on_base_percentage <- .safe_rate(output$hits + output$walks + output$hit_by_pitch, obp_denominator)
  output$slugging_percentage <- .safe_rate(output$total_bases, output$ab)
  output$ops <- output$on_base_percentage + output$slugging_percentage
  output$isolated_power <- output$slugging_percentage - output$batting_average
  output$strikeout_rate <- .safe_rate(output$strikeouts, output$pa)
  output$walk_rate <- .safe_rate(output$walks, output$pa)
  output$hard_hit_rate <- .safe_rate(output$hard_hit, output$batted_balls)
  output$barrel_proxy_rate <- .safe_rate(output$barrel_proxy, output$batted_balls)
  output$ground_ball_rate <- .safe_rate(output$ground_balls, output$batted_balls)
  output$line_drive_rate <- .safe_rate(output$line_drives, output$batted_balls)
  output$fly_ball_rate <- .safe_rate(output$fly_balls, output$batted_balls)
  output$pull_rate <- .safe_rate(output$pulled_batted_balls, output$batted_balls)
  output$woba_estimate <- .safe_rate(
    0.69 * (output$walks - output$intentional_walks) +
      0.72 * output$hit_by_pitch + 0.88 * output$singles +
      1.24 * output$doubles + 1.56 * output$triples + 2.01 * output$home_runs,
    woba_denominator
  )
  output$run_value_per_pa <- ifelse(
    output$run_value_opportunities > 0,
    output$run_value_total / output$run_value_opportunities,
    NA_real_
  )
  output$pa_reliability <- round(output$pa / (output$pa + 100), 3)
  output$bbe_reliability <- round(output$batted_balls / (output$batted_balls + 50), 3)
  output$sample_label <- dplyr::case_when(
    output$pa >= 250 ~ "strong",
    output$pa >= 100 ~ "moderate",
    output$pa >= 40 ~ "developing",
    TRUE ~ "small"
  )
  output$perspective <- perspective
  output$woba_method <- "fixed_linear_weights_v1"
  output$summary_method <- "sabrhood_player_summary_v1"
  dplyr::arrange(output, dplyr::desc(.data$pa), .data$player_name)
}

#' Summarize hitter performance
#' @inheritParams summarize_player_performance
#' @export
summarize_hitters <- function(plate_appearances, group_by = NULL, minimum_pa = 1L, as_of_date = NULL) {
  summarize_player_performance(plate_appearances, "batter", group_by, minimum_pa, as_of_date)
}

#' Summarize pitcher performance allowed
#' @inheritParams summarize_player_performance
#' @export
summarize_pitchers <- function(plate_appearances, group_by = NULL, minimum_pa = 1L, as_of_date = NULL) {
  summarize_player_performance(plate_appearances, "pitcher", group_by, minimum_pa, as_of_date)
}

#' Summarize pitch characteristics and results by pitch type
#'
#' @param pitches Canonical pitch view, optionally sequence-enriched.
#' @param perspective `pitcher` for arsenals or `batter` for pitches seen.
#' @param group_by Optional additional split columns.
#' @param minimum_pitches Minimum pitches retained.
#'
#' @return One row per player, split, and pitch type.
#' @export
summarize_pitch_types <- function(
    pitches,
    perspective = c("pitcher", "batter"),
    group_by = NULL,
    minimum_pitches = 10L) {
  perspective <- match.arg(perspective)
  required <- c(
    "game_pk", "game_date", "at_bat_index", "pitch_in_pa", "pitch_type", "pitch_name",
    "pitcher_id", "pitcher_name", "pitcher_hand", "batter_id", "batter_name",
    "batter_side", "fielding_team", "batting_team", "is_swing", "is_whiff",
    "is_contact", "is_zone", "is_out_of_zone", "is_chase", "is_in_play",
    "launch_speed", "is_terminal_pitch", "event_key", "is_two_strike",
    "start_speed", "spin_rate", "horizontal_break", "induced_vertical_break",
    "extension", "plate_x", "plate_z"
  )
  missing <- setdiff(required, names(pitches))
  if (length(missing) > 0L) stop("Pitches are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  group_by <- unique(as.character(group_by))
  group_by <- group_by[!is.na(group_by) & nzchar(group_by)]
  missing_groups <- setdiff(group_by, names(pitches))
  if (length(missing_groups) > 0L) stop("Unknown pitch summary groups: ", paste(missing_groups, collapse = ", "), call. = FALSE)

  order_index <- order(
    pitches$game_date, pitches$game_pk, pitches$at_bat_index,
    pitches$pitch_in_pa, na.last = TRUE
  )
  group_key <- paste(pitches$game_pk[order_index], pitches$at_bat_index[order_index], sep = "\034")
  first_in_group <- !duplicated(group_key)
  current_type <- pitches$pitch_type[order_index]
  previous_type <- c(NA_character_, current_type[-length(current_type)])
  previous_type[first_in_group] <- NA_character_
  comparable <- !is.na(current_type) & !is.na(previous_type)
  pitch_type_changed <- ifelse(comparable, current_type != previous_type, NA)
  plate_x <- pitches$plate_x[order_index]
  plate_z <- pitches$plate_z[order_index]
  previous_x <- c(NA_real_, plate_x[-length(plate_x)])
  previous_z <- c(NA_real_, plate_z[-length(plate_z)])
  previous_x[first_in_group] <- NA_real_
  previous_z[first_in_group] <- NA_real_
  location_separation <- sqrt((plate_x - previous_x)^2 + (plate_z - previous_z)^2)

  if (perspective == "pitcher") {
    player_id <- pitches$pitcher_id[order_index]
    player_name <- pitches$pitcher_name[order_index]
    team <- pitches$fielding_team[order_index]
    hand <- pitches$pitcher_hand[order_index]
    opponent_hand <- pitches$batter_side[order_index]
  } else {
    player_id <- pitches$batter_id[order_index]
    player_name <- pitches$batter_name[order_index]
    team <- pitches$batting_team[order_index]
    hand <- pitches$batter_side[order_index]
    opponent_hand <- pitches$pitcher_hand[order_index]
  }

  data <- tibble::tibble(
    player_id = player_id,
    player_name = player_name,
    team = team,
    hand = hand,
    opponent_hand = opponent_hand,
    pitch_type = current_type,
    pitch_name = pitches$pitch_name[order_index],
    is_swing = pitches$is_swing[order_index],
    is_whiff = pitches$is_whiff[order_index],
    is_contact = pitches$is_contact[order_index],
    is_zone = pitches$is_zone[order_index],
    is_out_of_zone = pitches$is_out_of_zone[order_index],
    is_chase = pitches$is_chase[order_index],
    is_in_play = pitches$is_in_play[order_index],
    launch_speed = pitches$launch_speed[order_index],
    is_terminal_pitch = pitches$is_terminal_pitch[order_index],
    event_key = pitches$event_key[order_index],
    is_two_strike = pitches$is_two_strike[order_index],
    start_speed = pitches$start_speed[order_index],
    spin_rate = pitches$spin_rate[order_index],
    horizontal_break = pitches$horizontal_break[order_index],
    induced_vertical_break = pitches$induced_vertical_break[order_index],
    extension = pitches$extension[order_index],
    pitch_type_changed = pitch_type_changed,
    location_separation = location_separation
  )
  for (column in group_by) data[[column]] <- pitches[[column]][order_index]
  minimum_pitches <- .integer_value(minimum_pitches)[[1L]]
  if (is.na(minimum_pitches) || minimum_pitches < 1L) stop("`minimum_pitches` must be positive.", call. = FALSE)

  grouping <- c("player_id", group_by, "pitch_type", "pitch_name")
  output <- data |>
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "", !is.na(.data$pitch_type), .data$pitch_type != "") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::summarise(
      player_name = .last_non_missing(.data$player_name),
      team = .last_non_missing(.data$team),
      hand = .last_non_missing(.data$hand),
      pitches = dplyr::n(),
      swings = sum(.data$is_swing, na.rm = TRUE),
      whiffs = sum(.data$is_whiff, na.rm = TRUE),
      contact = sum(.data$is_contact, na.rm = TRUE),
      zone_pitches = sum(.data$is_zone, na.rm = TRUE),
      out_of_zone_pitches = sum(.data$is_out_of_zone, na.rm = TRUE),
      chases = sum(.data$is_chase, na.rm = TRUE),
      batted_balls = sum(.data$is_in_play | !is.na(.data$launch_speed), na.rm = TRUE),
      hard_hit = sum((.data$is_in_play | !is.na(.data$launch_speed)) & .data$launch_speed >= 95, na.rm = TRUE),
      strikeouts = sum(.data$is_terminal_pitch & .data$event_key %in% c("strikeout", "strikeout_double_play"), na.rm = TRUE),
      two_strike_pitches = sum(.data$is_two_strike, na.rm = TRUE),
      average_velocity = .mean_or_na(.data$start_speed),
      average_spin_rate = .mean_or_na(.data$spin_rate),
      average_horizontal_break = .mean_or_na(.data$horizontal_break),
      average_induced_vertical_break = .mean_or_na(.data$induced_vertical_break),
      average_extension = .mean_or_na(.data$extension),
      average_exit_velocity = .mean_or_na(.data$launch_speed),
      pitch_changes = sum(.data$pitch_type_changed %in% TRUE, na.rm = TRUE),
      sequence_opportunities = sum(!is.na(.data$pitch_type_changed)),
      average_location_separation = .mean_or_na(.data$location_separation),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$pitches >= minimum_pitches)

  usage_groups <- c("player_id", group_by)
  output <- output |>
    dplyr::group_by(dplyr::across(dplyr::all_of(usage_groups))) |>
    dplyr::mutate(usage_rate = .data$pitches / sum(.data$pitches)) |>
    dplyr::ungroup()
  output$swing_rate <- .safe_rate(output$swings, output$pitches)
  output$whiff_rate <- .safe_rate(output$whiffs, output$swings)
  output$contact_rate <- .safe_rate(output$contact, output$swings)
  output$zone_rate <- .safe_rate(output$zone_pitches, output$pitches)
  output$chase_rate <- .safe_rate(output$chases, output$out_of_zone_pitches)
  output$hard_hit_rate <- .safe_rate(output$hard_hit, output$batted_balls)
  output$putaway_rate <- .safe_rate(output$strikeouts, output$two_strike_pitches)
  output$pitch_change_rate <- .safe_rate(output$pitch_changes, output$sequence_opportunities)
  output$pitch_reliability <- round(output$pitches / (output$pitches + 200), 3)
  output$perspective <- perspective
  output$summary_method <- "sabrhood_pitch_type_summary_v1"
  dplyr::arrange(output, .data$player_name, dplyr::desc(.data$usage_rate))
}

.standard_score <- function(x) {
  center <- mean(x, na.rm = TRUE)
  spread <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(spread) || spread == 0) return(rep(0, length(x)))
  output <- (x - center) / spread
  output[!is.finite(output)] <- 0
  output
}

#' Compare recent player form with the prior-season baseline
#'
#' @param plate_appearances Canonical, run-expectancy-enriched plate appearances.
#' @param perspective `batter` or `pitcher`.
#' @param recent_days Inclusive recent window length.
#' @param minimum_recent_pa Minimum recent plate appearances.
#' @param minimum_baseline_pa Minimum earlier baseline plate appearances.
#' @param as_of_date Inclusive cutoff date.
#'
#' @return One row per player with recent, baseline, delta, reliability, and
#'   zero-to-100 form-score fields.
#' @export
compare_recent_player_form <- function(
    plate_appearances,
    perspective = c("batter", "pitcher"),
    recent_days = 14L,
    minimum_recent_pa = 15L,
    minimum_baseline_pa = 30L,
    as_of_date = NULL) {
  perspective <- match.arg(perspective)
  recent_days <- .integer_value(recent_days)[[1L]]
  minimum_recent_pa <- .integer_value(minimum_recent_pa)[[1L]]
  minimum_baseline_pa <- .integer_value(minimum_baseline_pa)[[1L]]
  if (is.na(recent_days) || recent_days < 1L) stop("`recent_days` must be positive.", call. = FALSE)
  if (is.na(minimum_recent_pa) || minimum_recent_pa < 1L) stop("`minimum_recent_pa` must be positive.", call. = FALSE)
  if (is.na(minimum_baseline_pa) || minimum_baseline_pa < 1L) stop("`minimum_baseline_pa` must be positive.", call. = FALSE)

  dates <- as.Date(plate_appearances$game_date)
  if (is.null(as_of_date)) as_of_date <- max(dates, na.rm = TRUE)
  as_of_date <- as.Date(as_of_date)
  recent_start <- as_of_date - recent_days + 1L
  recent_data <- plate_appearances[dates >= recent_start & dates <= as_of_date, , drop = FALSE]
  baseline_data <- plate_appearances[dates < recent_start & dates <= as_of_date, , drop = FALSE]

  recent <- summarize_player_performance(
    recent_data, perspective, minimum_pa = minimum_recent_pa, as_of_date = as_of_date
  )
  baseline <- summarize_player_performance(
    baseline_data, perspective, minimum_pa = minimum_baseline_pa, as_of_date = recent_start - 1L
  )
  metrics <- c(
    "pa", "ops", "woba_estimate", "strikeout_rate", "walk_rate",
    "hard_hit_rate", "run_value_per_pa", "pa_reliability"
  )
  recent_keep <- recent[, c("player_id", "player_name", "team", "hand", metrics), drop = FALSE]
  baseline_keep <- baseline[, c("player_id", metrics), drop = FALSE]
  names(recent_keep)[match(metrics, names(recent_keep))] <- paste0("recent_", metrics)
  names(baseline_keep)[match(metrics, names(baseline_keep))] <- paste0("baseline_", metrics)
  output <- dplyr::inner_join(recent_keep, baseline_keep, by = "player_id")

  for (metric in setdiff(metrics, c("pa", "pa_reliability"))) {
    output[[paste0(metric, "_delta")]] <-
      output[[paste0("recent_", metric)]] - output[[paste0("baseline_", metric)]]
  }
  z_ops <- .standard_score(output$ops_delta)
  z_woba <- .standard_score(output$woba_estimate_delta)
  z_k <- .standard_score(output$strikeout_rate_delta)
  z_bb <- .standard_score(output$walk_rate_delta)
  z_hard <- .standard_score(output$hard_hit_rate_delta)
  z_run <- .standard_score(output$run_value_per_pa_delta)
  raw_form <- if (perspective == "batter") {
    z_ops + z_woba + z_run + 0.50 * z_hard - 0.50 * z_k + 0.30 * z_bb
  } else {
    -z_ops - z_woba - z_run - 0.50 * z_hard + 0.50 * z_k - 0.30 * z_bb
  }
  raw_form <- raw_form / 4.30
  output$form_score <- round(pmax(pmin(50 + 15 * raw_form, 100), 0), 1)
  output$form_score_confidence <- output$recent_pa_reliability
  output$form_label <- dplyr::case_when(
    output$form_score >= 65 ~ "surging",
    output$form_score >= 55 ~ "improving",
    output$form_score > 45 ~ "stable",
    output$form_score > 35 ~ "cooling",
    TRUE ~ "slumping"
  )
  output$recent_start <- recent_start
  output$as_of_date <- as_of_date
  output$perspective <- perspective
  output$form_method <- "sabrhood_recent_vs_prior_zscore_v1"
  dplyr::arrange(output, dplyr::desc(.data$form_score), dplyr::desc(.data$recent_pa))
}
