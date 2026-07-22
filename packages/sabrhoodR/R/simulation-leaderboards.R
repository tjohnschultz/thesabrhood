#' Build hitter projection leaderboards
#' @param batter_df Derived hitter simulation summaries.
#' @return A named list of sorted tibbles.
#' @export
make_hitter_leaderboards <- function(batter_df) {
  list(
    top_total_bases = dplyr::arrange(batter_df, dplyr::desc(.data$avg_TB)),
    top_hr_probability = dplyr::arrange(batter_df, dplyr::desc(.data$p_1_plus_HR)),
    top_xbh_probability = dplyr::arrange(batter_df, dplyr::desc(.data$p_1_plus_XBH)),
    top_hit_probability = dplyr::arrange(batter_df, dplyr::desc(.data$p_1_plus_H)),
    top_overall_score = batter_df |>
      dplyr::mutate(
        hitter_score = .data$avg_TB + 1.25 * .data$avg_HR +
          0.75 * .data$avg_XBH + 0.35 * .data$avg_BB - 0.20 * .data$avg_K
      ) |>
      dplyr::arrange(dplyr::desc(.data$hitter_score))
  )
}

#' Build pitcher projection leaderboards
#' @param pitcher_df Derived pitcher simulation summaries.
#' @return A named list of sorted tibbles.
#' @export
make_pitcher_leaderboards <- function(pitcher_df) {
  list(
    top_strikeout_projection = dplyr::arrange(pitcher_df, dplyr::desc(.data$avg_K)),
    safest_pitcher_lines = dplyr::arrange(pitcher_df, .data$avg_est_runs_simple),
    most_dangerous_matchups = dplyr::arrange(pitcher_df, dplyr::desc(.data$avg_est_runs_simple)),
    lowest_hr_risk = dplyr::arrange(pitcher_df, .data$avg_HR),
    highest_hr_risk = dplyr::arrange(pitcher_df, dplyr::desc(.data$avg_HR))
  )
}
