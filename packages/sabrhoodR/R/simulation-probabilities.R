#' Convert a matchup-adjusted lineup to plate-appearance event probabilities
#'
#' @param lineup_df A validated hitter lineup.
#' @param player_col Column containing player names.
#' @param team_col Column containing team identifiers.
#' @param triple_share Share of non-home-run extra-base hits assigned as triples.
#' @param use_features Whether to apply optional engineered feature scores.
#'
#' @return A tibble with mutually exclusive event probabilities for each hitter.
#' @export
prepare_lineup_event_probs <- function(
    lineup_df,
    player_col = "player",
    team_col = "team",
    triple_share = 0.08,
    use_features = TRUE) {
  validate_lineup_df(lineup_df, player_col = player_col, team_col = team_col)

  if (!is.numeric(triple_share) || length(triple_share) != 1L ||
      is.na(triple_share) || triple_share < 0 || triple_share > 1) {
    stop("`triple_share` must be one number between zero and one.", call. = FALSE)
  }

  choose_column <- function(preferred, fallback) {
    if (preferred %in% names(lineup_df)) lineup_df[[preferred]] else lineup_df[[fallback]]
  }
  score_or_zero <- function(column) {
    if (column %in% names(lineup_df)) .safe_numeric(lineup_df[[column]]) else rep(0, nrow(lineup_df))
  }

  bb_input <- choose_column("shrunk_BB_pct", "BB_pct")
  k_input <- choose_column("shrunk_K_pct", "K_pct")
  avg_input <- choose_column("shrunk_AVG", "AVG")
  slg_input <- choose_column("shrunk_SLG", "SLG")

  if (isTRUE(use_features)) {
    bb_multiplier <- .clip_probability(exp(0.04 * score_or_zero("discipline_score")), 0.85, 1.15)
    k_multiplier <- .clip_probability(exp(0.05 * score_or_zero("k_risk_score")), 0.85, 1.20)
    hit_multiplier <- .clip_probability(exp(0.03 * score_or_zero("contact_floor_score")), 0.90, 1.12)
    xbh_multiplier <- .clip_probability(exp(0.05 * score_or_zero("power_quality_score")), 0.85, 1.20)
    hr_multiplier <- .clip_probability(
      exp(0.06 * score_or_zero("power_quality_score") + 0.05 * score_or_zero("hr_shape_score")),
      0.75,
      1.30
    )
  } else {
    bb_multiplier <- k_multiplier <- hit_multiplier <- xbh_multiplier <- hr_multiplier <- rep(1, nrow(lineup_df))
  }

  output <- tibble::as_tibble(lineup_df)
  output$lineup_spot <- seq_len(nrow(output))
  output$player <- as.character(output[[player_col]])
  output$team <- as.character(output[[team_col]])
  output$PA <- dplyr::coalesce(.safe_numeric(output$PA), 0)
  output$H <- dplyr::coalesce(.safe_numeric(output$H), 0)
  output$HR <- dplyr::coalesce(.safe_numeric(output$HR), 0)
  output$TB <- dplyr::coalesce(.safe_numeric(output$TB), 0)

  avg_value <- dplyr::coalesce(.safe_numeric(avg_input), 0)
  slg_value <- dplyr::coalesce(.safe_numeric(slg_input), 0)
  bb_value <- dplyr::coalesce(.as_decimal_rate(.safe_numeric(bb_input)), 0)
  k_value <- dplyr::coalesce(.as_decimal_rate(.safe_numeric(k_input)), 0)

  output$p_BB <- .clip_probability(bb_value * bb_multiplier)
  output$p_K <- .clip_probability(k_value * k_multiplier)
  hit_probability <- .clip_probability(avg_value * (1 - output$p_BB) * hit_multiplier)
  total_base_rate <- pmax(slg_value * (1 - output$p_BB), 0)
  output$p_HR <- pmin(.clip_probability(.safe_rate(output$HR, output$PA) * hr_multiplier), hit_probability)

  non_hr_extra_bases <- pmax(total_base_rate - hit_probability - 3 * output$p_HR, 0)
  non_hr_xbh <- (non_hr_extra_bases / (1 + triple_share)) * xbh_multiplier
  non_hr_xbh <- pmin(non_hr_xbh, pmax(hit_probability - output$p_HR, 0))
  output$p_3B <- non_hr_xbh * triple_share
  output$p_2B <- non_hr_xbh - output$p_3B
  output$p_1B <- pmax(hit_probability - output$p_HR - output$p_2B - output$p_3B, 0)

  allocated <- output$p_BB + output$p_K + output$p_1B + output$p_2B + output$p_3B + output$p_HR
  output$p_OUT <- pmax(1 - allocated, 0)
  total <- allocated + output$p_OUT

  probability_columns <- c("p_BB", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  output[probability_columns] <- lapply(output[probability_columns], function(x) x / total)
  output$raw_prob_sum <- allocated
  output$final_prob_sum <- rowSums(output[probability_columns])

  metadata_columns <- intersect(
    c("game_pk", "batting_side", "opposing_pitcher", "opposing_pitcher_hand", "opposing_pitcher_side"),
    names(output)
  )
  feature_columns <- intersect(
    c("shrunk_BB_pct", "shrunk_K_pct", "shrunk_AVG", "shrunk_SLG", "power_quality_score",
      "discipline_score", "k_risk_score", "hr_shape_score", "contact_floor_score"),
    names(output)
  )
  keep <- c(metadata_columns, "lineup_spot", "player", "team", "PA", "AVG", "SLG", "BB_pct", "K_pct",
            "H", "HR", "TB", feature_columns, probability_columns, "raw_prob_sum", "final_prob_sum")
  output <- output[unique(keep)]
  .validate_event_probabilities(output)
  output
}
