player_projection_outcome_rules <- function() {
  data.frame(
    metric_id = c(
      "batter_hit_1plus",
      "batter_hit_2plus",
      "batter_hr_1plus",
      "batter_hr_2plus",
      "batter_xbh_1plus",
      "batter_tb_2plus",
      "batter_tb_3plus",
      "pitcher_k_5plus",
      "pitcher_k_7plus",
      "pitcher_k_10plus"
    ),
    actual_column = c(
      "actual_H",
      "actual_H",
      "actual_HR",
      "actual_HR",
      "actual_XBH",
      "actual_TB",
      "actual_TB",
      "actual_K",
      "actual_K",
      "actual_K"
    ),
    threshold = c(1L, 2L, 1L, 2L, 1L, 2L, 3L, 5L, 7L, 10L),
    stringsAsFactors = FALSE
  )
}

settle_player_projection_outcomes <- function(rows) {
  if (!is.data.frame(rows)) {
    stop("Player projection settlement requires a data frame.", call. = FALSE)
  }

  required <- c("metric_id", unique(player_projection_outcome_rules()$actual_column))
  missing_columns <- setdiff(required, names(rows))
  if (length(missing_columns)) {
    stop(
      "Player projection settlement is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  rules <- player_projection_outcome_rules()
  unknown_metrics <- setdiff(unique(as.character(rows$metric_id)), rules$metric_id)
  if (length(unknown_metrics)) {
    stop(
      "No settlement rule exists for player projection metrics: ",
      paste(unknown_metrics, collapse = ", "),
      call. = FALSE
    )
  }

  actual <- rep(NA_integer_, nrow(rows))
  for (index in seq_len(nrow(rules))) {
    rule <- rules[index, , drop = FALSE]
    matching_rows <- as.character(rows$metric_id) == rule$metric_id
    observed <- suppressWarnings(as.numeric(rows[[rule$actual_column]]))
    actual[matching_rows] <- as.integer(
      observed[matching_rows] >= rule$threshold
    )
  }
  actual
}
