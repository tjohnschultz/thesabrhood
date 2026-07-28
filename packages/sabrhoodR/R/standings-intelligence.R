.standings_number <- function(data, candidates, default = NA_real_) {
  value <- .column_or_default(data, candidates, default = default)
  suppressWarnings(as.numeric(as.character(value)))
}

.standings_text <- function(data, candidates, default = NA_character_) {
  as.character(.column_or_default(data, candidates, default = default))
}

.standings_percentile <- function(value, higher_is_better = TRUE) {
  value <- suppressWarnings(as.numeric(value))
  output <- dplyr::percent_rank(value)
  if (!isTRUE(higher_is_better)) output <- 1 - output
  output[!is.finite(output)] <- 0.5
  output
}

#' Standardize a BaseballR standings response
#'
#' @param standings Output from `baseballr::mlb_standings()`.
#' @param snapshot_date Date represented by the standings snapshot.
#' @param level Either `"MLB"` or `"AAA"`.
#' @return One row per club with stable standings fields.
#' @export
standardize_standings_snapshot <- function(standings, snapshot_date, level = c("MLB", "AAA")) {
  level <- match.arg(level)
  if (!is.data.frame(standings)) stop("`standings` must be a data frame.", call. = FALSE)
  output <- tibble::tibble(
    snapshot_date = as.Date(snapshot_date),
    level = level,
    team_id = as.character(.column_or_default(standings, c("team_records_team_id", "team_id"))),
    team = .standings_text(standings, c("team_records_team_name", "team_name", "team")),
    league_id = .standings_number(standings, c("league_id")),
    division_id = .standings_number(standings, c("division_id")),
    wins = .standings_number(standings, c("team_records_wins", "wins")),
    losses = .standings_number(standings, c("team_records_losses", "losses")),
    games_played = .standings_number(standings, c("team_records_games_played", "games_played")),
    winning_percentage = .standings_number(
      standings,
      c("team_records_winning_percentage", "winning_percentage")
    ),
    runs_scored = .standings_number(standings, c("team_records_runs_scored", "runs_scored")),
    runs_allowed = .standings_number(standings, c("team_records_runs_allowed", "runs_allowed")),
    run_differential = .standings_number(
      standings,
      c("team_records_run_differential", "run_differential")
    ),
    games_back = .standings_number(standings, c("team_records_games_back", "games_back")),
    wild_card_games_back = .standings_number(
      standings,
      c("team_records_wild_card_games_back", "wild_card_games_back")
    ),
    division_rank = .standings_number(
      standings,
      c("team_records_division_rank", "division_rank")
    ),
    league_rank = .standings_number(
      standings,
      c("team_records_league_rank", "league_rank")
    ),
    level_rank = .standings_number(
      standings,
      c("team_records_sport_rank", "sport_rank", "level_rank")
    ),
    streak_code = .standings_text(
      standings,
      c("team_records_streak_streak_code", "streak_code")
    )
  )
  output$winning_percentage[!is.finite(output$winning_percentage)] <-
    output$wins[!is.finite(output$winning_percentage)] /
    pmax(output$wins[!is.finite(output$winning_percentage)] + output$losses[!is.finite(output$winning_percentage)], 1)
  output$run_differential[!is.finite(output$run_differential)] <-
    output$runs_scored[!is.finite(output$run_differential)] -
    output$runs_allowed[!is.finite(output$run_differential)]
  output$games_back[!is.finite(output$games_back) & output$division_rank == 1] <- 0
  output$wild_card_games_back[
    !is.finite(output$wild_card_games_back) & output$league_rank <= 6
  ] <- 0
  output$league_label <- ifelse(
    level == "MLB",
    ifelse(output$league_id == 103, "American League", "National League"),
    ifelse(output$league_id == 117, "International League", "Pacific Coast League")
  )
  mlb_divisions <- c(
    `200` = "AL West", `201` = "AL East", `202` = "AL Central",
    `203` = "NL West", `204` = "NL East", `205` = "NL Central"
  )
  output$division_label <- if (level == "MLB") {
    unname(mlb_divisions[as.character(output$division_id)])
  } else {
    output$league_label
  }
  if (level == "MLB") {
    mlb_team_names <- c(
      `108` = "Los Angeles Angels", `109` = "Arizona Diamondbacks",
      `110` = "Baltimore Orioles", `111` = "Boston Red Sox",
      `112` = "Chicago Cubs", `113` = "Cincinnati Reds",
      `114` = "Cleveland Guardians", `115` = "Colorado Rockies",
      `116` = "Detroit Tigers", `117` = "Houston Astros",
      `118` = "Kansas City Royals", `119` = "Los Angeles Dodgers",
      `120` = "Washington Nationals", `121` = "New York Mets",
      `133` = "Athletics", `134` = "Pittsburgh Pirates",
      `135` = "San Diego Padres", `136` = "Seattle Mariners",
      `137` = "San Francisco Giants", `138` = "St. Louis Cardinals",
      `139` = "Tampa Bay Rays", `140` = "Texas Rangers",
      `141` = "Toronto Blue Jays", `142` = "Minnesota Twins",
      `143` = "Philadelphia Phillies", `144` = "Atlanta Braves",
      `145` = "Chicago White Sox", `146` = "Miami Marlins",
      `147` = "New York Yankees", `158` = "Milwaukee Brewers"
    )
    full_names <- unname(mlb_team_names[output$team_id])
    output$team[!is.na(full_names)] <- full_names[!is.na(full_names)]
  }
  output$standings_method <- "baseballr_mlb_standings_snapshot_v1"
  dplyr::arrange(output, .data$league_id, .data$division_id, .data$division_rank, dplyr::desc(.data$winning_percentage))
}

#' Compare two standings snapshots
#'
#' Positive rank and games-back changes mean the club moved closer to first.
#'
#' @param current,prior Standardized standings snapshots.
#' @return Current standings with seven-day movement fields.
#' @export
build_standings_movement <- function(current, prior) {
  required <- c(
    "team_id", "team", "winning_percentage", "run_differential",
    "games_back", "division_rank", "league_rank", "snapshot_date"
  )
  if (!all(required %in% names(current)) || !all(required %in% names(prior))) {
    stop("Standings snapshots are missing required movement fields.", call. = FALSE)
  }
  prior_fields <- prior[, c(
    "team_id", "team", "snapshot_date", "winning_percentage",
    "run_differential", "games_back", "division_rank", "league_rank"
  ), drop = FALSE]
  names(prior_fields) <- c(
    "team_id", "prior_team", "prior_snapshot_date", "prior_winning_percentage",
    "prior_run_differential", "prior_games_back", "prior_division_rank", "prior_league_rank"
  )
  output <- dplyr::left_join(current, prior_fields, by = "team_id")
  output$division_rank_change <- output$prior_division_rank - output$division_rank
  output$league_rank_change <- output$prior_league_rank - output$league_rank
  output$games_back_change <- output$prior_games_back - output$games_back
  output$winning_percentage_change <- output$winning_percentage - output$prior_winning_percentage
  output$run_differential_change <- output$run_differential - output$prior_run_differential
  movement_score <- 30 * output$winning_percentage_change +
    0.12 * output$run_differential_change +
    0.75 * output$division_rank_change +
    0.35 * output$games_back_change
  movement_score[!is.finite(movement_score)] <- 0
  output$movement_score <- round(movement_score, 2)
  output$movement_label <- dplyr::case_when(
    output$movement_score >= 1.5 ~ "surging",
    output$movement_score <= -1.5 ~ "slipping",
    TRUE ~ "steady"
  )
  output$movement_method <- "seven_day_rank_gap_pct_run_differential_v1"
  dplyr::arrange(output, dplyr::desc(.data$movement_score), .data$division_rank)
}

.aaa_match_full_team <- function(short_name, full_names) {
  normalize <- function(value) tolower(gsub("[^a-z0-9]", "", as.character(value)))
  short <- normalize(short_name)
  full <- normalize(full_names)
  matches <- which(endsWith(full, short) | endsWith(short, full))
  if (length(matches) == 1L) full_names[[matches]] else NA_character_
}

#' Build a Triple-A team strength ranking
#'
#' @param standings Current standardized Triple-A standings.
#' @param movement Triple-A standings movement.
#' @param hitters,pitchers Triple-A performance-watch outputs.
#' @param callups Individual call-up radar.
#' @return One row per Triple-A club with record, movement, talent, and score.
#' @export
build_aaa_team_rankings <- function(standings, movement, hitters, pitchers, callups = NULL) {
  if (!nrow(standings)) return(tibble::tibble())
  full_teams <- sort(unique(c(as.character(hitters$team), as.character(pitchers$team))))
  standings$full_team <- vapply(standings$team, .aaa_match_full_team, character(1), full_names = full_teams)
  standings$full_team[is.na(standings$full_team)] <- standings$team[is.na(standings$full_team)]

  aggregate_watch <- function(data, prefix) {
    if (!nrow(data)) return(tibble::tibble(full_team = character()))
    output <- data |>
      dplyr::group_by(full_team = .data$team) |>
      dplyr::summarise(
        talent_score = mean(utils::head(sort(.data$performance_score, decreasing = TRUE), 5L), na.rm = TRUE),
        qualified_players = dplyr::n(),
        young_standouts = sum(.data$age <= 24 & .data$performance_score >= 65, na.rm = TRUE),
        .groups = "drop"
      )
    names(output)[names(output) != "full_team"] <- paste0(prefix, "_", names(output)[names(output) != "full_team"])
    output
  }
  hitter_team <- aggregate_watch(hitters, "hitter")
  pitcher_team <- aggregate_watch(pitchers, "pitcher")
  output <- dplyr::left_join(standings, hitter_team, by = "full_team") |>
    dplyr::left_join(pitcher_team, by = "full_team")
  if (nrow(movement)) {
    movement_fields <- movement[, intersect(
      c("team_id", "movement_score", "movement_label", "division_rank_change", "run_differential_change"),
      names(movement)
    ), drop = FALSE]
    output <- dplyr::left_join(output, movement_fields, by = "team_id")
  }
  callup_counts <- if (!is.null(callups) && nrow(callups)) {
    callups |>
      dplyr::filter(.data$callup_score >= 65) |>
      dplyr::count(full_team = .data$team, name = "readiness_candidates")
  } else tibble::tibble(full_team = character(), readiness_candidates = integer())
  output <- dplyr::left_join(output, callup_counts, by = "full_team")
  numeric_defaults <- c(
    "hitter_talent_score", "pitcher_talent_score", "hitter_qualified_players",
    "pitcher_qualified_players", "hitter_young_standouts", "pitcher_young_standouts",
    "readiness_candidates", "movement_score"
  )
  for (column in intersect(numeric_defaults, names(output))) {
    output[[column]][!is.finite(output[[column]])] <- 0
  }
  output$record_score <- 100 * .standings_percentile(output$winning_percentage)
  output$run_differential_score <- 100 * .standings_percentile(output$run_differential)
  output$hitter_talent_score[output$hitter_talent_score == 0] <- 50
  output$pitcher_talent_score[output$pitcher_talent_score == 0] <- 50
  output$pipeline_score <- 100 * .standings_percentile(
    output$hitter_young_standouts + output$pitcher_young_standouts + output$readiness_candidates
  )
  output$team_strength_score <- round(
    0.35 * output$record_score +
      0.20 * output$run_differential_score +
      0.20 * output$hitter_talent_score +
      0.20 * output$pitcher_talent_score +
      0.05 * output$pipeline_score,
    1
  )
  output$team_ranking_method <- "aaa_record_rd_top5_talent_pipeline_v1"
  output <- output[order(-output$team_strength_score, -output$winning_percentage), , drop = FALSE]
  output$team_rank <- seq_len(nrow(output))
  tibble::as_tibble(output)
}
