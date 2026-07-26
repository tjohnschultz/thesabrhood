.matchup_event_names <- c("BB", "HBP", "K", "X1B", "X2B", "X3B", "HR", "OUT")

.matchup_event_counts <- function(row) {
  if (is.null(row) || !is.data.frame(row) || nrow(row) == 0L) {
    return(stats::setNames(rep(0, length(.matchup_event_names)), .matchup_event_names))
  }
  value <- function(column) {
    if (!column %in% names(row)) return(0)
    result <- .safe_numeric(row[[column]][[1L]])
    if (length(result) == 0L || !is.finite(result)) 0 else pmax(result, 0)
  }
  pa <- value("pa")
  counts <- c(
    BB = value("walks"),
    HBP = value("hit_by_pitch"),
    K = value("strikeouts"),
    X1B = value("singles"),
    X2B = value("doubles"),
    X3B = value("triples"),
    HR = value("home_runs")
  )
  allocated <- sum(counts)
  if (pa > 0 && allocated > pa) counts <- counts * pa / allocated
  c(counts, OUT = pmax(pa - sum(counts), 0))
}

.normalize_matchup_rates <- function(value, fallback = NULL) {
  value <- .safe_numeric(value)
  value[!is.finite(value) | value < 0] <- 0
  total <- sum(value)
  if (total <= 0) {
    if (is.null(fallback)) stop("Event rates must contain positive mass.", call. = FALSE)
    value <- .safe_numeric(fallback)
    value[!is.finite(value) | value < 0] <- 0
    total <- sum(value)
  }
  stats::setNames(value / total, .matchup_event_names)
}

.matchup_league_rates <- function(hitters) {
  if (!is.data.frame(hitters) || nrow(hitters) == 0L) {
    stop("`hitters` must contain at least one player summary.", call. = FALSE)
  }
  counts <- Reduce(
    `+`,
    lapply(seq_len(nrow(hitters)), function(index) {
      .matchup_event_counts(hitters[index, , drop = FALSE])
    })
  )
  .normalize_matchup_rates(counts)
}

.matchup_shrink_rates <- function(row, prior_rates, prior_pa) {
  counts <- .matchup_event_counts(row)
  exposure <- sum(counts)
  if (!is.finite(prior_pa) || prior_pa < 0) stop("Prior plate appearances must be non-negative.", call. = FALSE)
  .normalize_matchup_rates(counts + prior_pa * prior_rates, fallback = prior_rates)
}

.matchup_form_adjustment <- function(rates, form_row, strength = 1) {
  if (is.null(form_row) || !is.data.frame(form_row) || nrow(form_row) == 0L) return(rates)
  confidence <- if ("form_score_confidence" %in% names(form_row)) {
    .safe_numeric(form_row$form_score_confidence[[1L]])
  } else {
    0
  }
  if (!is.finite(confidence) || confidence <= 0) return(rates)
  confidence <- pmax(pmin(confidence, 1), 0) * strength
  ratio <- function(recent, baseline) {
    if (!all(c(recent, baseline) %in% names(form_row))) return(1)
    numerator <- .safe_numeric(form_row[[recent]][[1L]])
    denominator <- .safe_numeric(form_row[[baseline]][[1L]])
    if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) return(1)
    pmax(pmin(numerator / denominator, 1.60), 0.60)
  }
  multiplier <- stats::setNames(rep(1, length(.matchup_event_names)), .matchup_event_names)
  multiplier[["K"]] <- exp(0.25 * confidence * log(ratio("recent_strikeout_rate", "baseline_strikeout_rate")))
  multiplier[["BB"]] <- exp(0.22 * confidence * log(ratio("recent_walk_rate", "baseline_walk_rate")))
  contact_ratio <- ratio("recent_woba_estimate", "baseline_woba_estimate")
  multiplier[c("X1B", "X2B", "X3B")] <- exp(0.12 * confidence * log(contact_ratio))
  multiplier[["HR"]] <- exp(0.18 * confidence * log(contact_ratio))
  .normalize_matchup_rates(rates * multiplier, fallback = rates)
}

.matchup_combine_rates <- function(hitter_rates, pitcher_rates, league_rates) {
  epsilon <- 1e-8
  score <- (pmax(hitter_rates, epsilon) * pmax(pitcher_rates, epsilon)) /
    pmax(league_rates, epsilon)
  .normalize_matchup_rates(score, fallback = league_rates)
}

.matchup_find_player <- function(data, player_id) {
  if (!is.data.frame(data) || nrow(data) == 0L || !"player_id" %in% names(data)) return(data.frame())
  result <- data[as.character(data$player_id) == as.character(player_id), , drop = FALSE]
  if (nrow(result)) result[1L, , drop = FALSE] else data.frame()
}

.matchup_find_split <- function(data, player_id, opponent_hand) {
  if (!is.data.frame(data) || nrow(data) == 0L ||
      !all(c("player_id", "opponent_hand") %in% names(data))) return(data.frame())
  result <- data[
    as.character(data$player_id) == as.character(player_id) &
      toupper(as.character(data$opponent_hand)) == toupper(as.character(opponent_hand)),
    ,
    drop = FALSE
  ]
  if (nrow(result)) result[1L, , drop = FALSE] else data.frame()
}

.matchup_actual_batter_hand <- function(listed_hand, pitcher_hand) {
  listed_hand <- toupper(as.character(listed_hand)[[1L]])
  pitcher_hand <- toupper(as.character(pitcher_hand)[[1L]])
  if (listed_hand == "S" && pitcher_hand == "L") return("R")
  if (listed_hand == "S" && pitcher_hand == "R") return("L")
  if (listed_hand %in% c("L", "R")) listed_hand else "U"
}

#' Build batter-versus-pitcher event probabilities
#'
#' Combines hitter and pitcher event distributions with a multinomial log5
#' calculation. Overall player rates are shrunk toward the league, handedness
#' splits are shrunk toward each player's overall distribution, and recent form
#' is applied as a deliberately restrained optional adjustment. The resulting
#' plate-appearance events are mutually exclusive and sum to one.
#'
#' @param matchups One row per batter-starter matchup. Required identifiers are
#'   `batter_id` and `pitcher_id`; game, player, team, and batting-order columns
#'   are retained when supplied.
#' @param hitters,pitchers Overall PBP performance summaries.
#' @param hitter_platoon,pitcher_platoon Optional summaries by opponent hand.
#' @param hitter_form,pitcher_form Optional non-overlapping recent-form tables.
#' @param hitter_prior_pa,pitcher_prior_pa League-prior sample sizes for overall
#'   hitter and pitcher rates.
#' @param split_prior_pa Player-overall prior size used for handedness splits.
#' @param form_strength Global multiplier for the restrained form adjustment.
#'
#' @return A tibble with one mutually exclusive event distribution per matchup.
#'   League event rates are attached as the `league_event_rates` attribute.
#' @export
build_matchup_event_probabilities <- function(
    matchups,
    hitters,
    pitchers,
    hitter_platoon = NULL,
    pitcher_platoon = NULL,
    hitter_form = NULL,
    pitcher_form = NULL,
    hitter_prior_pa = 200,
    pitcher_prior_pa = 250,
    split_prior_pa = 125,
    form_strength = 1) {
  if (!is.data.frame(matchups) || nrow(matchups) == 0L) {
    stop("`matchups` must be a non-empty data frame.", call. = FALSE)
  }
  required <- c("batter_id", "pitcher_id")
  missing <- setdiff(required, names(matchups))
  if (length(missing)) stop("Matchups are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!is.data.frame(hitters) || !is.data.frame(pitchers)) {
    stop("Overall hitter and pitcher summaries must be data frames.", call. = FALSE)
  }
  priors <- c(hitter_prior_pa, pitcher_prior_pa, split_prior_pa)
  if (any(!is.finite(priors)) || any(priors < 0)) {
    stop("All prior sample sizes must be finite and non-negative.", call. = FALSE)
  }
  if (!is.numeric(form_strength) || length(form_strength) != 1L ||
      !is.finite(form_strength) || form_strength < 0 || form_strength > 2) {
    stop("`form_strength` must be one number between zero and two.", call. = FALSE)
  }

  league_rates <- .matchup_league_rates(hitters)
  rows <- lapply(seq_len(nrow(matchups)), function(index) {
    matchup <- matchups[index, , drop = FALSE]
    batter <- .matchup_find_player(hitters, matchup$batter_id[[1L]])
    pitcher <- .matchup_find_player(pitchers, matchup$pitcher_id[[1L]])
    batter_hand_listed <- if (nrow(batter) && "hand" %in% names(batter)) as.character(batter$hand[[1L]]) else "U"
    pitcher_hand <- if (nrow(pitcher) && "hand" %in% names(pitcher)) as.character(pitcher$hand[[1L]]) else "U"
    batter_hand <- .matchup_actual_batter_hand(batter_hand_listed, pitcher_hand)

    batter_overall <- .matchup_shrink_rates(batter, league_rates, hitter_prior_pa)
    pitcher_overall <- .matchup_shrink_rates(pitcher, league_rates, pitcher_prior_pa)
    batter_split <- .matchup_find_split(hitter_platoon, matchup$batter_id[[1L]], pitcher_hand)
    pitcher_split <- .matchup_find_split(pitcher_platoon, matchup$pitcher_id[[1L]], batter_hand)
    batter_rates <- if (nrow(batter_split)) {
      .matchup_shrink_rates(batter_split, batter_overall, split_prior_pa)
    } else {
      batter_overall
    }
    pitcher_rates <- if (nrow(pitcher_split)) {
      .matchup_shrink_rates(pitcher_split, pitcher_overall, split_prior_pa)
    } else {
      pitcher_overall
    }
    batter_form_row <- .matchup_find_player(hitter_form, matchup$batter_id[[1L]])
    pitcher_form_row <- .matchup_find_player(pitcher_form, matchup$pitcher_id[[1L]])
    batter_rates <- .matchup_form_adjustment(batter_rates, batter_form_row, form_strength)
    pitcher_rates <- .matchup_form_adjustment(pitcher_rates, pitcher_form_row, form_strength)
    probability <- .matchup_combine_rates(batter_rates, pitcher_rates, league_rates)

    batter_pa <- if (nrow(batter)) sum(.matchup_event_counts(batter)) else 0
    pitcher_pa <- if (nrow(pitcher)) sum(.matchup_event_counts(pitcher)) else 0
    batter_split_pa <- if (nrow(batter_split)) sum(.matchup_event_counts(batter_split)) else 0
    pitcher_split_pa <- if (nrow(pitcher_split)) sum(.matchup_event_counts(pitcher_split)) else 0
    reliability <- 100 * (
      0.35 * batter_pa / (batter_pa + hitter_prior_pa) +
        0.35 * pitcher_pa / (pitcher_pa + pitcher_prior_pa) +
        0.15 * batter_split_pa / (batter_split_pa + split_prior_pa) +
        0.15 * pitcher_split_pa / (pitcher_split_pa + split_prior_pa)
    )
    passthrough <- intersect(
      c("game_id", "game_date", "team_side", "batting_order", "batter_id", "batter_name",
        "batter_team", "pitcher_id", "pitcher_name", "pitcher_team"),
      names(matchup)
    )
    output <- tibble::as_tibble(matchup[passthrough])
    output$batter_hand <- batter_hand
    output$pitcher_hand <- toupper(pitcher_hand)
    output$batter_pa <- batter_pa
    output$pitcher_bf <- pitcher_pa
    output$batter_split_pa <- batter_split_pa
    output$pitcher_split_bf <- pitcher_split_pa
    output$batter_form_confidence <- if (nrow(batter_form_row)) .safe_numeric(batter_form_row$form_score_confidence[[1L]]) else 0
    output$pitcher_form_confidence <- if (nrow(pitcher_form_row)) .safe_numeric(pitcher_form_row$form_score_confidence[[1L]]) else 0
    output$p_BB <- probability[["BB"]]
    output$p_HBP <- probability[["HBP"]]
    output$p_K <- probability[["K"]]
    output$p_1B <- probability[["X1B"]]
    output$p_2B <- probability[["X2B"]]
    output$p_3B <- probability[["X3B"]]
    output$p_HR <- probability[["HR"]]
    output$p_OUT <- probability[["OUT"]]
    output$p_hit <- sum(probability[c("X1B", "X2B", "X3B", "HR")])
    output$p_xbh <- sum(probability[c("X2B", "X3B", "HR")])
    output$p_on_base <- sum(probability[c("BB", "HBP", "X1B", "X2B", "X3B", "HR")])
    output$estimated_woba <- 0.69 * probability[["BB"]] + 0.72 * probability[["HBP"]] +
      0.88 * probability[["X1B"]] + 1.24 * probability[["X2B"]] +
      1.56 * probability[["X3B"]] + 2.01 * probability[["HR"]]
    output$probability_sum <- sum(probability)
    output$matchup_reliability <- reliability
    output$input_status <- if (!nrow(batter) && !nrow(pitcher)) {
      "league_fallback"
    } else if (!nrow(batter)) {
      "batter_league_fallback"
    } else if (!nrow(pitcher)) {
      "pitcher_league_fallback"
    } else if (!nrow(batter_split) || !nrow(pitcher_split)) {
      "overall_rates_with_partial_split"
    } else {
      "overall_and_platoon_complete"
    }
    output$model_version <- "multinomial_log5_platoon_form_v1"
    output
  })
  result <- dplyr::bind_rows(rows)
  attr(result, "league_event_rates") <- league_rates
  result
}
