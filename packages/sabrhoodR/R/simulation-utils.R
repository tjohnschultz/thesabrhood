# Internal probability clipping helper.
.clip_probability <- function(x, lower = 0, upper = 1) {
  pmin(pmax(x, lower), upper)
}

# Internal safe rate helper.
.safe_rate <- function(numerator, denominator) {
  invalid <- is.na(numerator) | is.na(denominator) | denominator <= 0
  ifelse(invalid, 0, numerator / denominator)
}

# Internal numeric conversion helper.
.safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Internal rate normalization helper.
.as_decimal_rate <- function(x) {
  ifelse(!is.na(x) & x > 1, x / 100, x)
}

#' Validate a simulation lineup
#'
#' @param lineup_df A data frame with one row per hitter.
#' @param player_col Column containing player names.
#' @param team_col Column containing team identifiers.
#'
#' @return `TRUE`, invisibly.
#' @export
validate_lineup_df <- function(lineup_df, player_col = "player", team_col = "team") {
  if (!is.data.frame(lineup_df)) {
    stop("`lineup_df` must be a data frame.", call. = FALSE)
  }

  required <- c(
    player_col, team_col, "PA", "H", "HR", "TB", "AVG", "SLG",
    "BB_pct", "K_pct"
  )
  missing <- setdiff(required, names(lineup_df))

  if (length(missing) > 0L) {
    stop(
      "Lineup is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(lineup_df) == 0L) {
    stop("Lineup has no hitters.", call. = FALSE)
  }
  if (anyNA(lineup_df[[player_col]]) || any(lineup_df[[player_col]] == "")) {
    stop("Every lineup row must have a player name.", call. = FALSE)
  }
  if (anyDuplicated(lineup_df[[player_col]])) {
    stop("Lineup player names must be unique.", call. = FALSE)
  }
  if (nrow(lineup_df) < 8L) {
    warning("Lineup has fewer than eight hitters.", call. = FALSE)
  }

  invisible(TRUE)
}

.validate_event_probabilities <- function(event_probs) {
  probability_columns <- c("p_BB", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  required <- c("lineup_spot", "player", "team", probability_columns)
  missing <- setdiff(required, names(event_probs))

  if (length(missing) > 0L) {
    stop(
      "Event probabilities are missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  probabilities <- as.matrix(event_probs[probability_columns])
  if (any(!is.finite(probabilities))) {
    stop("Event probabilities must all be finite.", call. = FALSE)
  }
  if (any(probabilities < 0 | probabilities > 1)) {
    stop("Event probabilities must be between zero and one.", call. = FALSE)
  }
  totals <- rowSums(probabilities)
  if (any(abs(totals - 1) > 1e-8)) {
    stop("Event probabilities must sum to one for every hitter.", call. = FALSE)
  }

  invisible(TRUE)
}
