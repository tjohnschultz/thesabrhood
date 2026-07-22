#' Build representative pitcher-hook scenarios
#'
#' @param model Output from [fit_manager_hook_model()].
#' @param scenarios Optional completed plate-appearance contexts. When omitted,
#'   a representative starter and reliever scenario ladder is used.
#'
#' @return Scenario contexts with modeled hook probabilities and ranks.
#' @export
build_hook_scenario_board <- function(model, scenarios = NULL) {
  if (!inherits(model, "sabrhood_hook_model")) {
    stop("`model` must come from fit_manager_hook_model().", call. = FALSE)
  }
  if (is.null(scenarios)) {
    scenarios <- tibble::tibble(
      scenario_id = c("starter_early", "starter_middle", "starter_third_time", "starter_deep", "reliever_first", "reliever_extended"),
      scenario_label = c(
        "Starter, early workload", "Starter, middle innings", "Starter, third time through",
        "Starter, deep workload", "Reliever, first pocket", "Reliever, extended outing"
      ),
      pitcher_role = c("starter", "starter", "starter", "starter", "reliever", "reliever"),
      pitches_in_appearance = c(45, 72, 88, 105, 12, 30),
      batters_faced_in_appearance = c(12, 19, 24, 28, 3, 8),
      times_through_order_proxy = c(2, 2, 3, 3, 1, 1),
      inning = c(3, 5, 6, 7, 7, 8),
      fielding_score_diff = c(0, 0, 0, 0, 0, 0),
      event_key = c("field_out", "field_out", "field_out", "field_out", "field_out", "field_out")
    )
  }
  output <- predict_manager_hook_probability(model, scenarios)
  output$hook_probability_rank <- rank(-output$hook_probability, ties.method = "min")
  relative <- .percentile(output$hook_probability)
  output$relative_likelihood <- ifelse(relative >= 0.67, "higher", ifelse(relative >= 0.34, "middle", "lower"))
  output$interpretation <- paste0(
    output$scenario_label, " carries a ", round(100 * output$hook_probability, 1),
    "% pooled-model hook probability in this fixed, tied-game scenario."
  )
  output$scenario_method <- "representative_tied_game_contexts_v1"
  output$causal_warning <- "Descriptive pooled model; feature coefficients are not causal effects."
  dplyr::arrange(output, .data$hook_probability_rank)
}

#' Build team bullpen matchup boards
#'
#' @param availability Bullpen availability output.
#' @param pitcher_summary Current pitcher performance summary.
#' @param leverage_index Leverage index supplied to the reliever selector.
#' @param top_n Number of options retained per team and batter side.
#'
#' @return Ranked reliever options for left- and right-handed upcoming batters.
#' @export
build_bullpen_matchup_board <- function(availability, pitcher_summary, leverage_index = 2, top_n = 3L) {
  availability_required <- c("pitcher_id", "pitcher_name", "team", "throws", "availability_score")
  summary_required <- c("player_id", "woba_estimate", "ops", "strikeout_rate", "hard_hit_rate", "pa_reliability")
  if (!is.data.frame(availability) || !all(availability_required %in% names(availability))) {
    stop("`availability` is missing required bullpen fields.", call. = FALSE)
  }
  if (!is.data.frame(pitcher_summary) || !all(summary_required %in% names(pitcher_summary))) {
    stop("`pitcher_summary` is missing required performance fields.", call. = FALSE)
  }
  top_n <- as.integer(top_n)
  if (is.na(top_n) || top_n < 1L) stop("`top_n` must be a positive integer.", call. = FALSE)

  performance <- data.frame(
    pitcher_id = as.character(pitcher_summary$player_id),
    performance_score =
      0.30 * .percentile(pitcher_summary$woba_estimate, FALSE) +
      0.25 * .percentile(pitcher_summary$ops, FALSE) +
      0.25 * .percentile(pitcher_summary$strikeout_rate) +
      0.20 * .percentile(pitcher_summary$hard_hit_rate, FALSE),
    performance_reliability = pmax(pmin(.safe_numeric(pitcher_summary$pa_reliability), 1), 0),
    stringsAsFactors = FALSE
  )
  performance$performance_score <- performance$performance_score * (0.65 + 0.35 * performance$performance_reliability)
  candidates <- availability
  candidates$pitcher_id <- as.character(candidates$pitcher_id)
  candidates <- merge(candidates, performance, by = "pitcher_id", all.x = TRUE, sort = FALSE)
  candidates$performance_score[!is.finite(candidates$performance_score)] <- 0.50
  candidates$performance_reliability[!is.finite(candidates$performance_reliability)] <- 0

  output <- list()
  output_index <- 1L
  teams <- sort(unique(as.character(candidates$team)))
  teams <- teams[!is.na(teams) & nzchar(teams)]
  for (team in teams) {
    team_rows <- candidates[candidates$team == team, , drop = FALSE]
    for (side in c("L", "R")) {
      ranked <- tryCatch(
        rank_reliever_options(team_rows, upcoming_batter_side = side, leverage_index = leverage_index),
        error = function(error) NULL
      )
      if (is.null(ranked) || !nrow(ranked)) next
      ranked <- ranked[seq_len(min(top_n, nrow(ranked))), , drop = FALSE]
      ranked$matchup_board_method <- "availability_role_matchup_performance_v1"
      ranked$identity_match_method <- "exact_mlbam_id"
      output[[output_index]] <- ranked
      output_index <- output_index + 1L
    }
  }
  if (!length(output)) return(tibble::tibble())
  result <- dplyr::bind_rows(output)
  dplyr::arrange(result, .data$team, .data$upcoming_batter_side, .data$selection_rank)
}
