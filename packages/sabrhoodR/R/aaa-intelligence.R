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
#' @param fielding,catching Optional Triple-A fielding and catching season
#'   results. When supplied, their components share the same estimated-runs
#'   scale as batting, baserunning, and pitching before conversion to sWAR.
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
    prospect_age = 24L,
    fielding = NULL,
    catching = NULL) {
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
      team_id = as.character(.column_or_default(data, c("team_id", "team.id"))),
      team = as.character(.column_or_default(data, c("team_name", "team.name"))),
      position = as.character(.column_or_default(data, c("position_abbreviation", "position.abbreviation")))
    )
  }
  fielding_components <- if (is.data.frame(fielding) && nrow(fielding)) {
    tibble::tibble(
      player_id = as.character(.column_or_default(fielding, c("player_id", "person_id", "id"))),
      position = as.character(.column_or_default(fielding, c("position_abbreviation", "position.abbreviation"))),
      chances = .numeric_value(.column_or_default(fielding, c("chances"))),
      fielding_rate = .numeric_value(.column_or_default(fielding, c("fielding", "fielding_percentage"))),
      double_plays = .numeric_value(.column_or_default(fielding, c("double_plays", "doublePlays")))
    ) |>
      dplyr::filter(
        !is.na(.data$player_id), .data$player_id != "",
        is.finite(.data$chances), .data$chances > 0,
        is.finite(.data$fielding_rate)
      ) |>
      dplyr::group_by(.data$position) |>
      dplyr::mutate(
        position_fielding_rate = stats::median(.data$fielding_rate, na.rm = TRUE),
        fielding_reliability = .data$chances / (.data$chances + 80),
        estimated_fielding_runs = (
          (.data$fielding_rate - .data$position_fielding_rate) * .data$chances * 0.75 +
            pmax(dplyr::coalesce(.data$double_plays, 0), 0) * 0.015
        ) * .data$fielding_reliability
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$player_id) |>
      dplyr::summarise(
        estimated_fielding_runs = sum(.data$estimated_fielding_runs, na.rm = TRUE),
        fielding_chances = sum(.data$chances, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      player_id = character(),
      estimated_fielding_runs = numeric(),
      fielding_chances = numeric()
    )
  }
  catching_components <- if (is.data.frame(catching) && nrow(catching)) {
    catching_rows <- tibble::tibble(
      player_id = as.character(.column_or_default(catching, c("player_id", "person_id", "id"))),
      caught_stealing = .numeric_value(.column_or_default(catching, c("caught_stealing", "caughtStealing"))),
      stolen_bases = .numeric_value(.column_or_default(catching, c("stolen_bases", "stolenBases"))),
      passed_balls = .numeric_value(.column_or_default(catching, c("passed_ball", "passedBalls"))),
      pickoffs = .numeric_value(.column_or_default(catching, c("pickoffs"))),
      catcher_interference = .numeric_value(.column_or_default(catching, c("catchers_interference", "catchersInterference")))
    ) |>
      dplyr::filter(!is.na(.data$player_id), .data$player_id != "") |>
      dplyr::mutate(
        caught_stealing = dplyr::coalesce(.data$caught_stealing, 0),
        stolen_bases = dplyr::coalesce(.data$stolen_bases, 0),
        passed_balls = dplyr::coalesce(.data$passed_balls, 0),
        pickoffs = dplyr::coalesce(.data$pickoffs, 0),
        catcher_interference = dplyr::coalesce(.data$catcher_interference, 0),
        steal_attempts = .data$caught_stealing + .data$stolen_bases
      )
    league_cs_rate <- with(
      catching_rows,
      if (sum(steal_attempts, na.rm = TRUE) > 0) {
        sum(caught_stealing, na.rm = TRUE) / sum(steal_attempts, na.rm = TRUE)
      } else {
        0.25
      }
    )
    catching_rows |>
      dplyr::mutate(
        estimated_catching_runs =
          0.45 * (.data$caught_stealing - league_cs_rate * .data$steal_attempts) +
          0.25 * .data$pickoffs -
          0.25 * .data$passed_balls -
          0.30 * .data$catcher_interference
      ) |>
      dplyr::group_by(.data$player_id) |>
      dplyr::summarise(
        estimated_catching_runs = sum(.data$estimated_catching_runs, na.rm = TRUE),
        catcher_steal_attempts = sum(.data$steal_attempts, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      player_id = character(),
      estimated_catching_runs = numeric(),
      catcher_steal_attempts = numeric()
    )
  }
  hitter_watch <- dplyr::bind_cols(
    common(hitting),
    tibble::tibble(
      games = .integer_value(.column_or_default(hitting, c("games_played", "games"))),
      pa = .integer_value(.column_or_default(hitting, c("plate_appearances", "plateAppearances"))),
      home_runs = .integer_value(.column_or_default(hitting, c("home_runs", "homeRuns"))),
      stolen_bases = .integer_value(.column_or_default(hitting, c("stolen_bases", "stolenBases"))),
      caught_stealing = .integer_value(.column_or_default(hitting, c("caught_stealing", "caughtStealing"))),
      strikeouts = .integer_value(.column_or_default(hitting, c("strike_outs", "strikeouts"))),
      walks = .integer_value(.column_or_default(hitting, c("base_on_balls", "baseOnBalls"))),
      avg = .numeric_value(.column_or_default(hitting, c("avg", "batting_average"))),
      obp = .numeric_value(.column_or_default(hitting, c("obp", "on_base_percentage"))),
      slg = .numeric_value(.column_or_default(hitting, c("slg", "slugging_percentage"))),
      ops = .numeric_value(.column_or_default(hitting, c("ops", "on_base_plus_slugging")))
    )
  ) |>
    dplyr::left_join(fielding_components, by = "player_id") |>
    dplyr::left_join(catching_components, by = "player_id") |>
    dplyr::filter(.data$pa >= minimum_pa, !is.na(.data$player_id), .data$player_id != "") |>
    dplyr::mutate(
      caught_stealing = dplyr::coalesce(.data$caught_stealing, 0L),
      estimated_fielding_runs = dplyr::coalesce(.data$estimated_fielding_runs, 0),
      estimated_catching_runs = dplyr::coalesce(.data$estimated_catching_runs, 0),
      fielding_chances = dplyr::coalesce(.data$fielding_chances, 0),
      catcher_steal_attempts = dplyr::coalesce(.data$catcher_steal_attempts, 0),
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
      estimated_batting_runs = (.data$ops - stats::median(.data$ops, na.rm = TRUE)) * .data$pa * 0.16,
      estimated_running_runs = .data$stolen_bases * 0.18 - .data$caught_stealing * 0.42,
      sabrhood_war = round((
        .data$estimated_batting_runs + .data$estimated_running_runs +
          .data$estimated_fielding_runs + .data$estimated_catching_runs
      ) / 10, 1),
      swar_component_status = "batting, baserunning, fielding, and catching estimates on a 10-runs-per-win scale",
      watch_method = "aaa_age_performance_and_total_runs_lens_v2"
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
      starts = .integer_value(.column_or_default(pitching, c("games_started", "gamesStarted", "starts"))),
      strikeouts = .integer_value(.column_or_default(pitching, c("strike_outs", "strikeouts"))),
      walks = .integer_value(.column_or_default(pitching, c("base_on_balls", "baseOnBalls"))),
      batters_faced = .integer_value(.column_or_default(pitching, c("batters_faced", "battersFaced"))),
      home_runs = .integer_value(.column_or_default(pitching, c("home_runs", "homeRuns"))),
      stolen_bases_allowed = .integer_value(.column_or_default(pitching, c("stolen_bases", "stolenBases"))),
      caught_stealing = .integer_value(.column_or_default(pitching, c("caught_stealing", "caughtStealing"))),
      pickoffs = .integer_value(.column_or_default(pitching, c("pickoffs"))),
      balks = .integer_value(.column_or_default(pitching, c("balks"))),
      wild_pitches = .integer_value(.column_or_default(pitching, c("wild_pitches", "wildPitches")))
    )
  ) |>
    dplyr::left_join(fielding_components, by = "player_id") |>
    dplyr::filter(.data$innings >= minimum_ip, !is.na(.data$player_id), .data$player_id != "") |>
    dplyr::mutate(
      estimated_fielding_runs = dplyr::coalesce(.data$estimated_fielding_runs, 0),
      fielding_chances = dplyr::coalesce(.data$fielding_chances, 0),
      stolen_bases_allowed = dplyr::coalesce(.data$stolen_bases_allowed, 0L),
      caught_stealing = dplyr::coalesce(.data$caught_stealing, 0L),
      pickoffs = dplyr::coalesce(.data$pickoffs, 0L),
      balks = dplyr::coalesce(.data$balks, 0L),
      wild_pitches = dplyr::coalesce(.data$wild_pitches, 0L),
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
      estimated_pitching_runs = (stats::median(.data$era, na.rm = TRUE) - .data$era) * .data$innings / 9,
      estimated_running_runs =
        0.20 * (.data$caught_stealing + .data$pickoffs) -
        0.20 * .data$stolen_bases_allowed -
        0.25 * .data$balks -
        0.05 * .data$wild_pitches,
      estimated_catching_runs = 0,
      sabrhood_war = round((
        .data$estimated_pitching_runs + .data$estimated_running_runs +
          .data$estimated_fielding_runs
      ) / 10, 1),
      swar_component_status = "pitching, pitcher fielding, and running-game estimates on a 10-runs-per-win scale",
      watch_method = "aaa_age_performance_and_total_runs_lens_v2"
    ) |>
    dplyr::arrange(dplyr::desc(.data$performance_score), .data$age)
  pitcher_watch$watch_rank <- seq_len(nrow(pitcher_watch))
  list(hitters = hitter_watch, pitchers = pitcher_watch)
}

#' Build a transparent Triple-A call-up readiness radar
#'
#' This is a descriptive readiness score, not a calibrated probability. It
#' combines current performance, age relative to the level, Triple-A
#' experience, and the parent club's positional need.
#'
#' @param hitters,pitchers Outputs from [build_aaa_performance_watch()].
#' @param affiliates A data frame mapping `aaa_team` to `mlb_team`.
#' @param positional_war Team-position output from [build_team_positional_war()].
#' @param maximum_age Maximum age eligible for the public radar.
#' @return One ranked data frame containing hitter and pitcher candidates.
#' @export
build_aaa_callup_radar <- function(
    hitters,
    pitchers,
    affiliates,
    positional_war,
    maximum_age = 28L) {
  required_affiliates <- c("aaa_team", "mlb_team")
  required_needs <- c("team", "position", "position_label", "percentile", "mlb_rank")
  if (!all(required_affiliates %in% names(affiliates))) {
    stop("`affiliates` must map aaa_team to mlb_team.", call. = FALSE)
  }
  if (!all(required_needs %in% names(positional_war))) {
    stop("`positional_war` is missing team-need fields.", call. = FALSE)
  }

  position_group <- function(value) {
    value <- toupper(trimws(as.character(value)))
    ifelse(value %in% c("LF", "CF", "RF", "OF"), "OF",
      ifelse(value %in% c("P", "SP", "RP"), value, value))
  }
  normalize_team <- function(value) {
    tolower(gsub("[^a-z0-9]", "", as.character(value)))
  }
  affiliate_key <- normalize_team(affiliates$aaa_team)
  mlb_team_abbr <- c(
    "Arizona Diamondbacks" = "ARI", "Athletics" = "ATH",
    "Atlanta Braves" = "ATL", "Baltimore Orioles" = "BAL",
    "Boston Red Sox" = "BOS", "Chicago Cubs" = "CHC",
    "Chicago White Sox" = "CWS", "Cincinnati Reds" = "CIN",
    "Cleveland Guardians" = "CLE", "Colorado Rockies" = "COL",
    "Detroit Tigers" = "DET", "Houston Astros" = "HOU",
    "Kansas City Royals" = "KCR", "Los Angeles Angels" = "LAA",
    "Los Angeles Dodgers" = "LAD", "Miami Marlins" = "MIA",
    "Milwaukee Brewers" = "MIL", "Minnesota Twins" = "MIN",
    "New York Mets" = "NYM", "New York Yankees" = "NYY",
    "Philadelphia Phillies" = "PHI", "Pittsburgh Pirates" = "PIT",
    "San Diego Padres" = "SDP", "San Francisco Giants" = "SFG",
    "Seattle Mariners" = "SEA", "St. Louis Cardinals" = "STL",
    "Tampa Bay Rays" = "TBR", "Texas Rangers" = "TEX",
    "Toronto Blue Jays" = "TOR", "Washington Nationals" = "WSN"
  )

  prepare <- function(data, role) {
    if (!nrow(data)) return(tibble::tibble())
    output <- data[is.finite(.numeric_value(data$age)) & .numeric_value(data$age) <= maximum_age, , drop = FALSE]
    if (!nrow(output)) return(tibble::tibble())
    output$role <- role
    if (identical(role, "Pitcher")) {
      start_share <- .safe_rate(output$starts, output$games)
      output$need_position <- ifelse(is.finite(start_share) & start_share >= 0.45, "SP", "RP")
      output$experience_value <- output$innings
    } else {
      output$need_position <- position_group(output$position)
      output$need_position[!output$need_position %in% c("C", "1B", "2B", "3B", "SS", "OF", "DH")] <- "DH"
      output$experience_value <- output$games
    }
    affiliate_match <- match(normalize_team(output$team), affiliate_key)
    output$mlb_team <- affiliates$mlb_team[affiliate_match]
    output$age_score <- round(100 * pmax(0, pmin(1, (maximum_age - .numeric_value(output$age)) / 9)), 1)
    output$experience_score <- round(100 * .percentile_score(output$experience_value), 1)
    output
  }

  combined <- dplyr::bind_rows(
    prepare(hitters, "Hitter"),
    prepare(pitchers, "Pitcher")
  )
  if (!nrow(combined)) return(tibble::tibble())

  combined$mlb_team_abbr <- unname(mlb_team_abbr[combined$mlb_team])
  need_key <- paste(positional_war$team, positional_war$position, sep = "\034")
  candidate_key <- paste(combined$mlb_team_abbr, combined$need_position, sep = "\034")
  need_match <- match(candidate_key, need_key)
  combined$mlb_need_rank <- .integer_value(positional_war$mlb_rank[need_match])
  combined$mlb_need_percentile <- round(100 - .numeric_value(positional_war$percentile[need_match]), 1)
  combined$mlb_need_label <- as.character(positional_war$position_label[need_match])
  combined$mlb_need_percentile[!is.finite(combined$mlb_need_percentile)] <- 50
  combined$mlb_need_label[is.na(combined$mlb_need_label) | combined$mlb_need_label == ""] <- combined$need_position[
    is.na(combined$mlb_need_label) | combined$mlb_need_label == ""
  ]
  combined$callup_score <- round(
    0.35 * .numeric_value(combined$performance_score) +
      0.30 * combined$age_score +
      0.15 * combined$experience_score +
      0.20 * combined$mlb_need_percentile,
    1
  )
  combined$callup_reason <- paste0(
    combined$player_name, " pairs a ", format(combined$performance_score, nsmall = 1),
    " performance score with a ", format(combined$mlb_need_percentile, nsmall = 1),
    " parent-club need score at ", combined$mlb_need_label, "."
  )
  combined$callup_method <- "aaa_performance_age_experience_parent_need_v1_not_probability"
  combined <- combined[order(-combined$callup_score, combined$age, -combined$performance_score), , drop = FALSE]
  combined$callup_rank <- seq_len(nrow(combined))
  tibble::as_tibble(combined)
}
