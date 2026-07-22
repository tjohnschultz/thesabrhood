.safe_rate <- function(numerator, denominator) {
  numerator <- .numeric_value(numerator)
  denominator <- .numeric_value(denominator)
  ifelse(is.finite(denominator) & denominator > 0, numerator / denominator, NA_real_)
}

.percentile_score <- function(value, higher_is_better = TRUE) {
  value <- .numeric_value(value)
  score <- dplyr::percent_rank(value)
  if (!isTRUE(higher_is_better)) score <- 1 - score
  score
}

#' Build a compact Triple-A performance watch
#'
#' @param hitting,pitching BaseballR `mlb_stats()` season results for sport ID 11.
#' @param minimum_pa Minimum hitter plate appearances.
#' @param minimum_ip Minimum pitcher innings.
#' @param prospect_age Maximum age used for the young-player lens.
#'
#' @return A list with ranked hitter and pitcher watch tables.
#' @export
build_aaa_performance_watch <- function(
    hitting,
    pitching,
    minimum_pa = 100L,
    minimum_ip = 25,
    prospect_age = 24L) {
  if (!is.data.frame(hitting) || !is.data.frame(pitching)) {
    stop("`hitting` and `pitching` must be data frames.", call. = FALSE)
  }
  normalize_name <- function(data) as.character(.column_or_default(
    data, c("player_full_name", "player.fullName", "player_name", "fullName")
  ))
  common <- function(data) {
    tibble::tibble(
      player_id = as.character(.column_or_default(data, c("player_id", "person_id", "id"))),
      player_name = normalize_name(data),
      age = .integer_value(.column_or_default(data, c("age", "current_age"))),
      team = as.character(.column_or_default(data, c("team_name", "team.name"))),
      position = as.character(.column_or_default(data, c("position_abbreviation", "position.abbreviation")))
    )
  }
  hitter_watch <- dplyr::bind_cols(
    common(hitting),
    tibble::tibble(
      games = .integer_value(.column_or_default(hitting, c("games_played", "games"))),
      pa = .integer_value(.column_or_default(hitting, c("plate_appearances", "plateAppearances"))),
      home_runs = .integer_value(.column_or_default(hitting, c("home_runs", "homeRuns"))),
      stolen_bases = .integer_value(.column_or_default(hitting, c("stolen_bases", "stolenBases"))),
      strikeouts = .integer_value(.column_or_default(hitting, c("strike_outs", "strikeouts"))),
      walks = .integer_value(.column_or_default(hitting, c("base_on_balls", "baseOnBalls"))),
      avg = .numeric_value(.column_or_default(hitting, c("avg", "batting_average"))),
      obp = .numeric_value(.column_or_default(hitting, c("obp", "on_base_percentage"))),
      slg = .numeric_value(.column_or_default(hitting, c("slg", "slugging_percentage"))),
      ops = .numeric_value(.column_or_default(hitting, c("ops", "on_base_plus_slugging")))
    )
  ) |>
    dplyr::filter(.data$pa >= minimum_pa, !is.na(.data$player_id), .data$player_id != "") |>
    dplyr::mutate(
      strikeout_rate = .safe_rate(.data$strikeouts, .data$pa),
      walk_rate = .safe_rate(.data$walks, .data$pa),
      home_run_rate = .safe_rate(.data$home_runs, .data$pa),
      stolen_base_rate = .safe_rate(.data$stolen_bases, .data$pa),
      age_lens = ifelse(!is.na(.data$age) & .data$age <= prospect_age, "age-qualified watch", "performance standout"),
      performance_score = round(100 * (
        0.45 * .percentile_score(.data$ops) +
          0.20 * .percentile_score(.data$walk_rate - .data$strikeout_rate) +
          0.20 * .percentile_score(.data$home_run_rate) +
          0.15 * .percentile_score(.data$stolen_base_rate)
      ), 1),
      watch_method = "aaa_age_and_performance_lens_v1"
    ) |>
    dplyr::arrange(dplyr::desc(.data$performance_score), .data$age)
  hitter_watch$watch_rank <- seq_len(nrow(hitter_watch))

  pitcher_watch <- dplyr::bind_cols(
    common(pitching),
    tibble::tibble(
      games = .integer_value(.column_or_default(pitching, c("games_played", "games"))),
      innings = .numeric_value(.column_or_default(pitching, c("innings_pitched", "inningsPitched"))),
      era = .numeric_value(.column_or_default(pitching, c("era"))),
      whip = .numeric_value(.column_or_default(pitching, c("whip"))),
      strikeouts = .integer_value(.column_or_default(pitching, c("strike_outs", "strikeouts"))),
      walks = .integer_value(.column_or_default(pitching, c("base_on_balls", "baseOnBalls"))),
      batters_faced = .integer_value(.column_or_default(pitching, c("batters_faced", "battersFaced"))),
      home_runs = .integer_value(.column_or_default(pitching, c("home_runs", "homeRuns")))
    )
  ) |>
    dplyr::filter(.data$innings >= minimum_ip, !is.na(.data$player_id), .data$player_id != "") |>
    dplyr::mutate(
      estimated_bf = ifelse(is.finite(.data$batters_faced) & .data$batters_faced > 0, .data$batters_faced, .data$innings * 4.3),
      strikeout_rate = .safe_rate(.data$strikeouts, .data$estimated_bf),
      walk_rate = .safe_rate(.data$walks, .data$estimated_bf),
      k_minus_bb_rate = .data$strikeout_rate - .data$walk_rate,
      age_lens = ifelse(!is.na(.data$age) & .data$age <= prospect_age, "age-qualified watch", "performance standout"),
      performance_score = round(100 * (
        0.35 * .percentile_score(.data$era, FALSE) +
          0.25 * .percentile_score(.data$whip, FALSE) +
          0.40 * .percentile_score(.data$k_minus_bb_rate)
      ), 1),
      watch_method = "aaa_age_and_performance_lens_v1"
    ) |>
    dplyr::arrange(dplyr::desc(.data$performance_score), .data$age)
  pitcher_watch$watch_rank <- seq_len(nrow(pitcher_watch))
  list(hitters = hitter_watch, pitchers = pitcher_watch)
}
