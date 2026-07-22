fg_number <- function(data, name, default = NA_real_) {
  if (!name %in% names(data)) return(rep(default, nrow(data)))
  suppressWarnings(as.numeric(data[[name]]))
}

fg_character <- function(data, name, default = NA_character_) {
  if (!name %in% names(data)) return(rep(default, nrow(data)))
  as.character(data[[name]])
}

fg_decode_name <- function(value) {
  value <- as.character(value)
  replacements <- c("C)" = "\u00e9", "C!" = "\u00e1", "C-" = "\u00ed", "C1" = "\u00f1", "C3" = "\u00f3", "C:" = "\u00fa")
  for (token in names(replacements)) value <- gsub(token, replacements[[token]], value, fixed = TRUE)
  value
}

fg_league <- function(team) {
  american <- c("BAL", "BOS", "NYY", "TBR", "TOR", "CHW", "CLE", "DET", "KCR", "MIN", "HOU", "LAA", "ATH", "OAK", "SEA", "TEX")
  national <- c("ATL", "MIA", "NYM", "PHI", "WSN", "WSH", "CHC", "CIN", "MIL", "PIT", "STL", "ARI", "COL", "LAD", "SDP", "SFG")
  ifelse(team %in% american, "AL", ifelse(team %in% national, "NL", NA_character_))
}

fg_percentile <- function(value, higher_is_better = TRUE) {
  value <- suppressWarnings(as.numeric(value))
  output <- rep(NA_real_, length(value))
  keep <- is.finite(value)
  if (!any(keep)) return(output)
  ranked <- rank(value[keep], ties.method = "average", na.last = "keep")
  pct <- if (sum(keep) == 1L) 0.5 else (ranked - 1) / (sum(keep) - 1)
  if (!higher_is_better) pct <- 1 - pct
  output[keep] <- pct
  output
}

fg_innings_outs <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  whole <- floor(value)
  partial <- round((value - whole) * 10)
  whole * 3 + pmin(pmax(partial, 0), 2)
}

#' Standardize FanGraphs season leaderboards
#'
#' Builds compact hitter and pitcher season tables from BaseballR's FanGraphs
#' leaderboard outputs. The compact tables are intended for derived public
#' products; the wide source tables should remain outside the public site.
#'
#' @param hitters Output from `baseballr::fg_batter_leaders()`.
#' @param pitchers Output from `baseballr::fg_pitcher_leaders()`.
#' @param season Season year.
#' @return A list with `hitters` and `pitchers` data frames.
#' @export
standardize_fangraphs_season <- function(hitters, pitchers, season) {
  stopifnot(is.data.frame(hitters), is.data.frame(pitchers), length(season) == 1L)
  hitter_team <- fg_character(hitters, "team_name")
  pitcher_team <- fg_character(pitchers, "team_name")
  hitter_output <- data.frame(
    season = as.integer(season), player_id = fg_character(hitters, "xMLBAMID"),
    player_name = fg_decode_name(fg_character(hitters, "PlayerName")), team = hitter_team,
    league = fg_league(hitter_team), age = fg_number(hitters, "Age"), position = fg_character(hitters, "position"),
    games = fg_number(hitters, "G"), pa = fg_number(hitters, "PA"), ab = fg_number(hitters, "AB"),
    hits = fg_number(hitters, "H"), singles = fg_number(hitters, "1B"), doubles = fg_number(hitters, "2B"),
    triples = fg_number(hitters, "3B"), home_runs = fg_number(hitters, "HR"), runs = fg_number(hitters, "R"),
    rbi = fg_number(hitters, "RBI"), walks = fg_number(hitters, "BB"), strikeouts = fg_number(hitters, "SO"),
    stolen_bases = fg_number(hitters, "SB"), avg = fg_number(hitters, "AVG"), obp = fg_number(hitters, "OBP"),
    slg = fg_number(hitters, "SLG"), ops = fg_number(hitters, "OPS"), woba = fg_number(hitters, "wOBA"),
    wrc_plus = fg_number(hitters, "wRC_plus"), offense = fg_number(hitters, "Offense"),
    defense = fg_number(hitters, "Defense"), baserunning = fg_number(hitters, "BaseRunning"),
    war = fg_number(hitters, "WAR"), wpa = fg_number(hitters, "WPA"),
    source_method = "baseballr_fangraphs_batter_leaders_v1", stringsAsFactors = FALSE
  )
  innings_raw <- fg_number(pitchers, "IP")
  pitcher_output <- data.frame(
    season = as.integer(season), player_id = fg_character(pitchers, "xMLBAMID"),
    player_name = fg_decode_name(fg_character(pitchers, "PlayerName")), team = pitcher_team,
    league = fg_league(pitcher_team), age = fg_number(pitchers, "Age"), throws = fg_character(pitchers, "Throws"),
    games = fg_number(pitchers, "G"), starts = fg_number(pitchers, "GS"), wins = fg_number(pitchers, "W"),
    losses = fg_number(pitchers, "L"), saves = fg_number(pitchers, "SV"), innings_display = innings_raw,
    innings_outs = fg_innings_outs(innings_raw), batters_faced = fg_number(pitchers, "TBF"),
    era = fg_number(pitchers, "ERA"), whip = fg_number(pitchers, "WHIP"), fip = fg_number(pitchers, "FIP"),
    xfip = fg_number(pitchers, "xFIP"), strikeouts = fg_number(pitchers, "SO"), walks = fg_number(pitchers, "BB"),
    strikeout_rate = fg_number(pitchers, "K_pct"), walk_rate = fg_number(pitchers, "BB_pct"),
    k_minus_bb_rate = fg_number(pitchers, "K-BB_pct"), war = fg_number(pitchers, "WAR"),
    wpa = fg_number(pitchers, "WPA"), source_method = "baseballr_fangraphs_pitcher_leaders_v1",
    stringsAsFactors = FALSE
  )
  hitter_output <- hitter_output[!is.na(hitter_output$league) & hitter_output$player_id != "", , drop = FALSE]
  pitcher_output <- pitcher_output[!is.na(pitcher_output$league) & pitcher_output$player_id != "", , drop = FALSE]
  list(hitters = hitter_output, pitchers = pitcher_output)
}

#' Build transparent MLB award race boards
#'
#' Creates league-level MVP and Cy Young performance scores plus a provisional
#' young-player watch. The scores describe season performance and are not a
#' ballot forecast. Rookie eligibility remains provisional until prior service
#' time is joined.
#'
#' @param hitters Compact hitter table from `standardize_fangraphs_season()`.
#' @param pitchers Compact pitcher table from `standardize_fangraphs_season()`.
#' @param prior_hitters Optional compact prior-career hitter table.
#' @param prior_pitchers Optional compact prior-career pitcher table.
#' @param minimum_pa Minimum PA for MVP scoring.
#' @param minimum_outs Minimum recorded outs for Cy Young scoring.
#' @param young_age Maximum age for the provisional young-player watch.
#' @return A combined award-race data frame.
#' @export
build_award_race_boards <- function(hitters, pitchers, prior_hitters = NULL, prior_pitchers = NULL, minimum_pa = 200L, minimum_outs = 150L, young_age = 26L) {
  stopifnot(is.data.frame(hitters), is.data.frame(pitchers))
  pitching_war_by_player <- stats::setNames(pitchers$war, as.character(pitchers$player_id))
  boards <- list()
  for (league_name in c("AL", "NL")) {
    bat <- hitters[hitters$league == league_name & hitters$pa >= minimum_pa, , drop = FALSE]
    if (nrow(bat)) {
      bat$pitching_war <- unname(pitching_war_by_player[as.character(bat$player_id)])
      bat$pitching_war[!is.finite(bat$pitching_war)] <- 0
      bat$combined_war <- bat$war + bat$pitching_war
      bat$award_score <- 100 * (
        0.30 * fg_percentile(bat$combined_war) + 0.18 * fg_percentile(bat$wrc_plus) +
          0.12 * fg_percentile(bat$home_runs) + 0.08 * fg_percentile(bat$rbi) +
          0.07 * fg_percentile(bat$runs) + 0.10 * fg_percentile(bat$offense) +
          0.05 * fg_percentile(bat$defense) + 0.05 * fg_percentile(bat$pa) +
          0.05 * fg_percentile(bat$wpa)
      )
      bat <- bat[order(-bat$award_score, -bat$combined_war), , drop = FALSE]
      boards[[paste0(league_name, "_mvp")]] <- data.frame(
        award = "MVP", league = league_name, rank = seq_len(nrow(bat)), player_id = bat$player_id,
        player_name = bat$player_name, team = bat$team, role = "hitter", age = bat$age,
        award_score = round(bat$award_score, 1), war = bat$combined_war, primary_value = bat$wrc_plus,
        primary_label = "wRC+", volume_value = bat$pa, volume_label = "PA",
        evidence = paste0(format(round(bat$combined_war, 1), nsmall = 1), " combined WAR | ", round(bat$home_runs),
          " HR | ", round(bat$rbi), " RBI | ", round(bat$wrc_plus), " wRC+"),
        eligibility_status = "qualified performance pool", score_method = "fangraphs_mvp_total_value_score_v2",
        stringsAsFactors = FALSE
      )
    }
    pit <- pitchers[pitchers$league == league_name & pitchers$innings_outs >= minimum_outs, , drop = FALSE]
    if (nrow(pit)) {
      pit$award_score <- 100 * (
        0.25 * fg_percentile(pit$war) + 0.20 * fg_percentile(pit$era, FALSE) +
          0.15 * fg_percentile(pit$fip, FALSE) + 0.15 * fg_percentile(pit$k_minus_bb_rate) +
          0.15 * fg_percentile(pit$innings_outs) + 0.10 * fg_percentile(pit$whip, FALSE)
      )
      pit <- pit[order(-pit$award_score, -pit$war), , drop = FALSE]
      boards[[paste0(league_name, "_cy")]] <- data.frame(
        award = "Cy Young", league = league_name, rank = seq_len(nrow(pit)), player_id = pit$player_id,
        player_name = pit$player_name, team = pit$team, role = "pitcher", age = pit$age,
        award_score = round(pit$award_score, 1), war = pit$war, primary_value = pit$era,
        primary_label = "ERA", volume_value = pit$innings_display, volume_label = "IP",
        evidence = paste0(format(round(pit$war, 1), nsmall = 1), " WAR | ", format(round(pit$era, 2), nsmall = 2), " ERA | ", format(round(pit$innings_display, 1), nsmall = 1), " IP"),
        eligibility_status = "qualified performance pool", score_method = "fangraphs_cy_young_performance_score_v1",
        stringsAsFactors = FALSE
      )

      reliever_minimum_outs <- max(75L, min(as.integer(minimum_outs), 90L))
      rel <- pitchers[
        pitchers$league == league_name & pitchers$innings_outs >= reliever_minimum_outs &
          pitchers$games >= 20 & pitchers$starts <= 5 & pitchers$starts / pmax(pitchers$games, 1) <= 0.25,
        , drop = FALSE
      ]
      if (nrow(rel)) {
        rel$award_score <- 100 * (
          0.25 * fg_percentile(rel$war) + 0.20 * fg_percentile(rel$era, FALSE) +
            0.15 * fg_percentile(rel$fip, FALSE) + 0.15 * fg_percentile(rel$k_minus_bb_rate) +
            0.10 * fg_percentile(rel$wpa) + 0.10 * fg_percentile(rel$saves) +
            0.05 * fg_percentile(rel$innings_outs)
        )
        rel <- rel[order(-rel$award_score, -rel$war, -rel$saves), , drop = FALSE]
        boards[[paste0(league_name, "_reliever")]] <- data.frame(
          award = "Reliever of the Year", league = league_name, rank = seq_len(nrow(rel)), player_id = rel$player_id,
          player_name = rel$player_name, team = rel$team, role = "reliever", age = rel$age,
          award_score = round(rel$award_score, 1), war = rel$war, primary_value = rel$era,
          primary_label = "ERA", volume_value = rel$innings_display, volume_label = "IP",
          evidence = paste0(format(round(rel$war, 1), nsmall = 1), " WAR | ", format(round(rel$era, 2), nsmall = 2),
            " ERA | ", round(rel$saves), " SV | ", format(round(rel$innings_display, 1), nsmall = 1), " IP"),
          eligibility_status = "20+ games; no more than five starts and 25% start share",
          score_method = "fangraphs_reliever_profile_score_v1", stringsAsFactors = FALSE
        )
      }
    }
    if (!is.null(prior_hitters)) {
      prior_ab <- stats::setNames(prior_hitters$ab, prior_hitters$player_id)
      bat$prior_ab <- unname(prior_ab[as.character(bat$player_id)])
      bat$prior_ab[is.na(bat$prior_ab)] <- 0
      young_bat <- bat[bat$age <= young_age & bat$prior_ab <= 130, , drop = FALSE]
    } else {
      young_bat <- bat[bat$age <= young_age, , drop = FALSE]
    }
    if (!is.null(prior_pitchers)) {
      prior_outs <- stats::setNames(prior_pitchers$innings_outs, prior_pitchers$player_id)
      pit$prior_outs <- unname(prior_outs[as.character(pit$player_id)])
      pit$prior_outs[is.na(pit$prior_outs)] <- 0
      young_pit <- pit[pit$age <= young_age & pit$prior_outs <= 150, , drop = FALSE]
    } else {
      young_pit <- pit[pit$age <= young_age, , drop = FALSE]
    }
    young_rows <- list()
    if (nrow(young_bat)) {
      young_bat$rookie_score <- 100 * (
        0.40 * fg_percentile(young_bat$war) + 0.25 * fg_percentile(young_bat$wrc_plus) +
          0.15 * fg_percentile(young_bat$offense) + 0.10 * fg_percentile(young_bat$pa) +
          0.05 * fg_percentile(young_bat$defense) + 0.05 * fg_percentile(young_bat$baserunning)
      )
      young_rows[["bat"]] <- data.frame(
      player_id = young_bat$player_id, player_name = young_bat$player_name, team = young_bat$team,
      role = "hitter", age = young_bat$age, young_score = young_bat$rookie_score,
        war = young_bat$combined_war, primary_value = young_bat$wrc_plus, primary_label = "wRC+",
      volume_value = young_bat$pa, volume_label = "PA",
      evidence = paste0(format(round(young_bat$combined_war, 1), nsmall = 1), " combined WAR | ", round(young_bat$wrc_plus), " wRC+ | age ", round(young_bat$age)),
      stringsAsFactors = FALSE
    )
    }
    if (nrow(young_pit)) {
      young_pit$rookie_score <- 100 * (
        0.40 * fg_percentile(young_pit$war) + 0.20 * fg_percentile(young_pit$era, FALSE) +
          0.15 * fg_percentile(young_pit$fip, FALSE) + 0.15 * fg_percentile(young_pit$k_minus_bb_rate) +
          0.10 * fg_percentile(young_pit$innings_outs)
      )
      young_rows[["pit"]] <- data.frame(
      player_id = young_pit$player_id, player_name = young_pit$player_name, team = young_pit$team,
      role = "pitcher", age = young_pit$age, young_score = young_pit$rookie_score,
      war = young_pit$war, primary_value = young_pit$era, primary_label = "ERA",
      volume_value = young_pit$innings_display, volume_label = "IP",
      evidence = paste0(format(round(young_pit$war, 1), nsmall = 1), " WAR | ", format(round(young_pit$era, 2), nsmall = 2), " ERA | age ", round(young_pit$age)),
      stringsAsFactors = FALSE
    )
    }
    if (length(young_rows)) {
      young <- do.call(rbind, young_rows)
      young$award_score <- young$young_score
      young <- young[order(-young$award_score, -young$war), , drop = FALSE]
      boards[[paste0(league_name, "_young")]] <- data.frame(
        award = "ROTY watch", league = league_name, rank = seq_len(nrow(young)), player_id = young$player_id,
        player_name = young$player_name, team = young$team, role = young$role, age = young$age,
        award_score = round(young$award_score, 1), war = young$war, primary_value = young$primary_value,
        primary_label = young$primary_label, volume_value = young$volume_value, volume_label = young$volume_label,
        evidence = young$evidence,
        eligibility_status = if (is.null(prior_hitters) || is.null(prior_pitchers)) "provisional age screen; MLB rookie service not verified" else "prior AB/IP screen passed; MLB service days not verified",
        score_method = "fangraphs_roty_role_balanced_score_v3", stringsAsFactors = FALSE
      )
    }
  }
  output <- do.call(rbind, boards)
  rownames(output) <- NULL
  output
}
