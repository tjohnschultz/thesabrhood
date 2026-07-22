.context_percentile <- function(x, higher_is_better = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  output <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  count <- sum(keep)
  if (!count) return(output)
  if (count == 1L) {
    output[keep] <- 50
    return(output)
  }
  scored <- if (isTRUE(higher_is_better)) x[keep] else -x[keep]
  output[keep] <- round(100 * (rank(scored, ties.method = "average") - 1) / (count - 1))
  output
}

.context_ordinal <- function(value) {
  value <- round(suppressWarnings(as.numeric(value)))
  remainder_100 <- value %% 100
  suffix <- ifelse(remainder_100 %in% 11:13, "th", ifelse(value %% 10 == 1, "st", ifelse(value %% 10 == 2, "nd", ifelse(value %% 10 == 3, "rd", "th"))))
  paste0(value, suffix)
}

.player_context_definitions <- function(perspective) {
  if (perspective == "batter") {
    directions <- c(ops = 1, woba_estimate = 1, strikeout_rate = -1,
                    walk_rate = 1, hard_hit_rate = 1, run_value_per_pa = 1)
  } else {
    directions <- c(ops = -1, woba_estimate = -1, strikeout_rate = 1,
                    walk_rate = -1, hard_hit_rate = -1, run_value_per_pa = -1)
  }
  labels <- c(
    ops = if (perspective == "batter") "OPS" else "OPS allowed",
    woba_estimate = if (perspective == "batter") "estimated wOBA" else "estimated wOBA allowed",
    strikeout_rate = "strikeout rate",
    walk_rate = if (perspective == "batter") "walk rate" else "walk rate allowed",
    hard_hit_rate = if (perspective == "batter") "hard-hit rate" else "hard-hit rate allowed",
    run_value_per_pa = if (perspective == "batter") "run value per PA" else "run value allowed per PA"
  )
  list(directions = directions, labels = labels)
}

#' Build league-context player change profiles
#'
#' Converts recent-versus-prior player form into comparable change z-scores and
#' joins season-level league percentiles. Positive change z-scores always mean
#' better baseball performance from the named player's perspective, including
#' results allowed for pitchers.
#'
#' @param recent_form Output from [compare_recent_player_form()] for one
#'   perspective.
#' @param season_summary Output from [summarize_player_performance()] for the
#'   same perspective and season.
#'
#' @return One row per exact MLBAM player ID with raw recent and baseline
#'   metrics, six direction-aware change z-scores, six season percentiles, and
#'   a selected dominant-change interpretation.
#' @export
build_player_change_profiles <- function(recent_form, season_summary) {
  form_required <- c(
    "player_id", "player_name", "team", "hand", "perspective", "recent_pa",
    "baseline_pa", "form_score", "form_score_confidence", "form_label",
    "recent_start", "as_of_date", "ops_delta", "woba_estimate_delta",
    "strikeout_rate_delta", "walk_rate_delta", "hard_hit_rate_delta",
    "run_value_per_pa_delta"
  )
  season_required <- c(
    "player_id", "perspective", "pa", "ops", "woba_estimate",
    "strikeout_rate", "walk_rate", "hard_hit_rate", "run_value_per_pa",
    "pa_reliability", "sample_label"
  )
  missing_form <- setdiff(form_required, names(recent_form))
  missing_season <- setdiff(season_required, names(season_summary))
  if (length(missing_form)) {
    stop("`recent_form` is missing: ", paste(missing_form, collapse = ", "), call. = FALSE)
  }
  if (length(missing_season)) {
    stop("`season_summary` is missing: ", paste(missing_season, collapse = ", "), call. = FALSE)
  }
  perspectives <- unique(as.character(recent_form$perspective))
  perspectives <- perspectives[!is.na(perspectives) & nzchar(perspectives)]
  if (length(perspectives) != 1L || !perspectives %in% c("batter", "pitcher")) {
    stop("`recent_form` must contain exactly one valid perspective.", call. = FALSE)
  }
  perspective <- perspectives[[1L]]
  season_subset <- season_summary[as.character(season_summary$perspective) == perspective, season_required, drop = FALSE]
  season_subset <- season_subset[!duplicated(season_subset$player_id), , drop = FALSE]
  season_names <- setdiff(names(season_subset), c("player_id", "perspective"))
  names(season_subset)[match(season_names, names(season_subset))] <- paste0("season_", season_names)
  output <- dplyr::inner_join(recent_form, season_subset, by = c("player_id", "perspective"))
  if (!nrow(output)) return(tibble::as_tibble(output))

  definitions <- .player_context_definitions(perspective)
  metrics <- names(definitions$directions)
  for (metric in metrics) {
    oriented_delta <- suppressWarnings(as.numeric(output[[paste0(metric, "_delta")]])) * definitions$directions[[metric]]
    output[[paste0(metric, "_change_z")]] <- round(.standard_score(oriented_delta), 3)
    output[[paste0("season_", metric, "_percentile")]] <- .context_percentile(
      output[[paste0("season_", metric)]], definitions$directions[[metric]] > 0
    )
  }

  z_columns <- paste0(metrics, "_change_z")
  z_matrix <- as.matrix(output[, z_columns, drop = FALSE])
  z_matrix[!is.finite(z_matrix)] <- 0
  dominant_index <- max.col(abs(z_matrix), ties.method = "first")
  row_index <- seq_len(nrow(output))
  output$dominant_change_stat <- metrics[dominant_index]
  output$dominant_change_label <- unname(definitions$labels[output$dominant_change_stat])
  output$dominant_change_z <- round(z_matrix[cbind(row_index, dominant_index)], 2)
  output$dominant_change_abs_z <- abs(output$dominant_change_z)
  output$dominant_change_direction <- dplyr::case_when(
    output$dominant_change_z >= 0.75 ~ "improving",
    output$dominant_change_z <= -0.75 ~ "declining",
    TRUE ~ "stable"
  )
  output$dominant_change_strength <- dplyr::case_when(
    output$dominant_change_abs_z >= 2.5 ~ "extreme",
    output$dominant_change_abs_z >= 2.0 ~ "strong",
    output$dominant_change_abs_z >= 1.25 ~ "notable",
    TRUE ~ "mild"
  )
  output$dominant_recent_value <- vapply(row_index, function(i) {
    output[[paste0("recent_", output$dominant_change_stat[[i]])]][[i]]
  }, numeric(1))
  output$dominant_baseline_value <- vapply(row_index, function(i) {
    output[[paste0("baseline_", output$dominant_change_stat[[i]])]][[i]]
  }, numeric(1))
  output$dominant_season_percentile <- vapply(row_index, function(i) {
    output[[paste0("season_", output$dominant_change_stat[[i]], "_percentile")]][[i]]
  }, numeric(1))
  sentence_label <- paste0(toupper(substr(output$dominant_change_label, 1L, 1L)), substr(output$dominant_change_label, 2L, nchar(output$dominant_change_label)))
  output$change_context <- paste0(
    sentence_label, " is the largest recent shift: ",
    output$dominant_change_direction, " by ",
    format(round(output$dominant_change_abs_z, 1), nsmall = 1),
    " standard deviations versus the MLB change pattern. Season level: ",
    .context_ordinal(output$dominant_season_percentile), " percentile."
  )
  output$change_signal_score <- round(pmin(100, 25 * output$dominant_change_abs_z) *
    (0.50 + 0.50 * suppressWarnings(as.numeric(output$form_score_confidence))), 1)
  output$identity_match_method <- "exact_mlbam_player_id"
  output$change_method <- "sabrhood_multimetric_change_context_v1"
  dplyr::arrange(
    tibble::as_tibble(output),
    dplyr::desc(.data$dominant_change_abs_z),
    dplyr::desc(.data$recent_pa)
  )
}
