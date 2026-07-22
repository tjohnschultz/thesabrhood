probability_rank <- function(data) {
  groups <- split(seq_len(nrow(data)), data$metric_id)
  data$metric_rank <- NA_integer_
  for (indices in groups) {
    ordered <- indices[order(-data$probability[indices], -data$sample_size[indices])]
    data$metric_rank[ordered] <- seq_along(ordered)
  }
  data[order(match(data$metric_id, unique(data$metric_id)), data$metric_rank), , drop = FALSE]
}

#' Build daily player performance probability leaderboards
#'
#' Converts season event rates into simple pregame opportunity baselines for
#' players in posted batting orders and probable starters. These probabilities
#' deliberately exclude opponent, park, weather, and batting-order effects and
#' therefore remain development baselines rather than public betting forecasts.
#'
#' @param hitters Compact FanGraphs hitter season table.
#' @param pitchers Compact FanGraphs pitcher season table.
#' @param lineups Standardized daily batting orders.
#' @param probables Standardized daily probable starters.
#' @param expected_pa Expected plate appearances for each listed hitter.
#' @return Long-form probability leaderboard.
#' @export
build_player_probability_board <- function(hitters, pitchers, lineups, probables, expected_pa = 4.2) {
  stopifnot(is.data.frame(hitters), is.data.frame(pitchers), is.data.frame(lineups), is.data.frame(probables))
  lineup_ids <- unique(as.character(lineups$player_id))
  bat <- hitters[as.character(hitters$player_id) %in% lineup_ids & hitters$pa >= 75, , drop = FALSE]
  lineup_match <- match(as.character(bat$player_id), as.character(lineups$player_id))
  bat$game_id <- as.character(lineups$game_id[lineup_match])
  bat$batting_order <- suppressWarnings(as.integer(lineups$batting_order[lineup_match]))
  singles <- if ("singles" %in% names(bat)) bat$singles else pmax(0, bat$hits - bat$doubles - bat$triples - bat$home_runs)
  hit_rate <- pmin(pmax(bat$hits / bat$pa, 0), 1)
  hr_rate <- pmin(pmax(bat$home_runs / bat$pa, 0), 1)
  single_rate <- pmin(pmax(singles / bat$pa, 0), 1)
  extra_base_rate <- pmin(pmax((bat$doubles + bat$triples + bat$home_runs) / bat$pa, 0), 1)
  batter_rows <- rbind(
    data.frame(metric_id = "batter_hr_1plus", metric_label = "1+ home run", player_id = bat$player_id, player_name = bat$player_name, team = bat$team, role = "hitter", game_id = bat$game_id, probability = 1 - exp(-expected_pa * hr_rate), expected_opportunities = expected_pa, sample_size = bat$pa, evidence = paste0(bat$home_runs, " HR in ", round(bat$pa), " PA"), stringsAsFactors = FALSE),
    data.frame(metric_id = "batter_hit_1plus", metric_label = "1+ hit", player_id = bat$player_id, player_name = bat$player_name, team = bat$team, role = "hitter", game_id = bat$game_id, probability = 1 - exp(-expected_pa * hit_rate), expected_opportunities = expected_pa, sample_size = bat$pa, evidence = paste0(bat$hits, " H in ", round(bat$pa), " PA"), stringsAsFactors = FALSE),
    data.frame(metric_id = "batter_tb_2plus", metric_label = "2+ total bases", player_id = bat$player_id, player_name = bat$player_name, team = bat$team, role = "hitter", game_id = bat$game_id, probability = 1 - exp(-expected_pa * (single_rate + extra_base_rate)) * (1 + expected_pa * single_rate), expected_opportunities = expected_pa, sample_size = bat$pa, evidence = paste0("Season event mix across ", round(bat$pa), " PA"), stringsAsFactors = FALSE)
  )

  probable_ids <- unique(as.character(probables$starter_id))
  pit <- pitchers[as.character(pitchers$player_id) %in% probable_ids & pitchers$starts >= 3 & pitchers$batters_faced > 0, , drop = FALSE]
  probable_match <- match(as.character(pit$player_id), as.character(probables$starter_id))
  pit$game_id <- as.character(probables$game_id[probable_match])
  expected_bf <- pmin(pmax(pit$batters_faced / pmax(pit$starts, 1), 12), 30)
  trials <- round(expected_bf)
  k_rate <- pmin(pmax(pit$strikeout_rate, 0), 1)
  pitcher_rows <- rbind(
    data.frame(metric_id = "pitcher_k_5plus", metric_label = "5+ strikeouts", player_id = pit$player_id, player_name = pit$player_name, team = pit$team, role = "pitcher", game_id = pit$game_id, probability = stats::pbinom(4, trials, k_rate, lower.tail = FALSE), expected_opportunities = expected_bf, sample_size = pit$innings_outs / 3, evidence = paste0(round(100 * k_rate, 1), "% K across ", format(round(pit$innings_display, 1), nsmall = 1), " IP"), stringsAsFactors = FALSE),
    data.frame(metric_id = "pitcher_k_7plus", metric_label = "7+ strikeouts", player_id = pit$player_id, player_name = pit$player_name, team = pit$team, role = "pitcher", game_id = pit$game_id, probability = stats::pbinom(6, trials, k_rate, lower.tail = FALSE), expected_opportunities = expected_bf, sample_size = pit$innings_outs / 3, evidence = paste0(round(100 * k_rate, 1), "% K across ", format(round(pit$innings_display, 1), nsmall = 1), " IP"), stringsAsFactors = FALSE)
  )
  output <- rbind(batter_rows, pitcher_rows)
  output$probability <- pmin(pmax(output$probability, 0), 1)
  output$model_status <- "development season-rate baseline"
  output$probability_method <- "poisson_event_and_binomial_strikeout_baseline_v1"
  probability_rank(output)
}
