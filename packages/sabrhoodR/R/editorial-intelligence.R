.safe_numeric <- function(value) suppressWarnings(as.numeric(value))

.weighted_mean_safe <- function(value, weight) {
  value <- .safe_numeric(value)
  weight <- .safe_numeric(weight)
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(value[keep], weight[keep])
}

.percentile <- function(value, higher_is_better = TRUE) {
  value <- .safe_numeric(value)
  if (!higher_is_better) value <- -value
  output <- rep(NA_real_, length(value))
  keep <- is.finite(value)
  if (sum(keep) == 1L) output[keep] <- 1
  if (sum(keep) > 1L) output[keep] <- (rank(value[keep], ties.method = "average") - 1) / (sum(keep) - 1)
  output
}

.team_rank <- function(value, higher_is_better = TRUE) {
  value <- .safe_numeric(value)
  ranked <- if (higher_is_better) -value else value
  as.integer(rank(ranked, ties.method = "min", na.last = "keep"))
}

.ordinal <- function(value) {
  value <- as.integer(value)
  suffix <- ifelse(value %% 100 %in% 11:13, "th", ifelse(value %% 10 == 1, "st", ifelse(value %% 10 == 2, "nd", ifelse(value %% 10 == 3, "rd", "th"))))
  paste0(value, suffix)
}

#' Build an active-player career milestone watch
#'
#' Matches current-season summaries to historical profiles using a unique,
#' normalized player name. The identity bridge is intentionally provisional
#' until an authoritative MLBAM-to-Lahman identifier crosswalk is supplied.
#'
#' @param hitters Current-season hitter performance summary.
#' @param pitchers Current-season pitcher performance summary.
#' @param career_profiles Output from [build_player_career_profiles()].
#' @param season_year Season represented by `hitters` and `pitchers`.
#' @param career_data_through Last season included in the career profiles.
#' @param minimum_significance Minimum historical recognition score.
#'
#' @return Ranked milestone-watch candidates.
#' @export
build_active_milestone_watch <- function(
    hitters,
    pitchers,
    career_profiles,
    season_year,
    career_data_through = season_year - 1L,
    minimum_significance = 20) {
  required_current <- c("player_name", "team")
  if (!is.data.frame(hitters) || !all(required_current %in% names(hitters))) {
    stop("`hitters` must contain player_name and team.", call. = FALSE)
  }
  if (!is.data.frame(pitchers) || !all(required_current %in% names(pitchers))) {
    stop("`pitchers` must contain player_name and team.", call. = FALSE)
  }
  required_profiles <- c("playerID", "player_name", "career_significance_score")
  if (!is.data.frame(career_profiles) || !all(required_profiles %in% names(career_profiles))) {
    stop("`career_profiles` must come from build_player_career_profiles().", call. = FALSE)
  }
  if (career_data_through >= season_year) {
    stop("`career_data_through` must precede `season_year` to avoid double counting.", call. = FALSE)
  }

  profile_key <- if ("player_name_key" %in% names(career_profiles)) {
    as.character(career_profiles$player_name_key)
  } else if ("player_name_raw" %in% names(career_profiles)) {
    .normalize_player_name(career_profiles$player_name_raw)
  } else {
    .normalize_player_name(career_profiles$player_name)
  }
  unique_profile_key <- !duplicated(profile_key) & !duplicated(profile_key, fromLast = TRUE) & nzchar(profile_key)
  profile_lookup <- career_profiles[unique_profile_key, , drop = FALSE]
  profile_lookup$.name_key <- profile_key[unique_profile_key]

  specifications <- list(
    list(data = hitters, role = "hitter", current = "hits", career = "career_H", stat = "hits", thresholds = c(500, 1000, 1500, 2000, 2500, 3000), window = 100),
    list(data = hitters, role = "hitter", current = "home_runs", career = "career_HR", stat = "home runs", thresholds = c(100, 200, 300, 400, 500, 600, 700), window = 25),
    list(data = pitchers, role = "pitcher", current = "strikeouts", career = "career_SO_pitch", stat = "strikeouts", thresholds = c(500, 1000, 1500, 2000, 2500, 3000, 4000, 5000), window = 100)
  )
  output <- list()
  output_index <- 1L
  for (spec in specifications) {
    current <- spec$data
    if (!all(c(spec$current, spec$career) %in% c(names(current), names(profile_lookup)))) next
    current$.name_key <- .normalize_player_name(current$player_name)
    matched <- match(current$.name_key, profile_lookup$.name_key)
    keep <- !is.na(matched)
    if (!any(keep)) next
    current <- current[keep, , drop = FALSE]
    profiles <- profile_lookup[matched[keep], , drop = FALSE]
    season_value <- .safe_numeric(current[[spec$current]])
    prior_value <- .safe_numeric(profiles[[spec$career]])
    career_to_date <- prior_value + season_value
    for (index in seq_len(nrow(current))) {
      future_threshold <- spec$thresholds[spec$thresholds > career_to_date[[index]]]
      reached_threshold <- spec$thresholds[prior_value[[index]] < spec$thresholds & career_to_date[[index]] >= spec$thresholds]
      if (length(reached_threshold)) {
        target <- max(reached_threshold)
        distance <- 0
        status <- "reached_this_season"
      } else if (length(future_threshold)) {
        target <- min(future_threshold)
        distance <- target - career_to_date[[index]]
        status <- "approaching"
      } else {
        next
      }
      if (status == "approaching" && distance > spec$window) next
      significance <- .safe_numeric(profiles$career_significance_score[[index]])
      if (!is.finite(significance) || significance < minimum_significance) next
      proximity <- if (distance == 0) 1 else pmax(0, 1 - distance / spec$window)
      story_score <- round(100 * (0.50 * proximity + 0.30 * significance / 100 + 0.20 * pmin(season_value[[index]] / pmax(target - prior_value[[index]], 1), 1)), 1)
      headline <- if (distance == 0) {
        paste0(current$player_name[[index]], " reached ", format(target, big.mark = ","), " career ", spec$stat)
      } else {
        paste0(current$player_name[[index]], " is ", format(distance, big.mark = ","), " ", spec$stat, " from ", format(target, big.mark = ","))
      }
      output[[output_index]] <- data.frame(
        player_id = if ("player_id" %in% names(current)) current$player_id[[index]] else NA,
        playerID = profiles$playerID[[index]],
        player_name = current$player_name[[index]],
        team = current$team[[index]],
        role = spec$role,
        milestone_stat = spec$stat,
        prior_career_value = prior_value[[index]],
        current_season_value = season_value[[index]],
        career_to_date_value = career_to_date[[index]],
        milestone_target = target,
        distance_to_milestone = distance,
        milestone_status = status,
        progress_pct = round(100 * career_to_date[[index]] / target, 1),
        recognition_tier = if ("recognition_tier" %in% names(profiles)) profiles$recognition_tier[[index]] else NA_character_,
        career_significance_score = significance,
        story_score = story_score,
        headline = headline,
        identity_match_method = "unique_normalized_name_v1",
        identity_match_confidence = 0.75,
        milestone_method = paste0("lahman_through_", career_data_through, "_plus_current_summary_v1"),
        stringsAsFactors = FALSE
      )
      output_index <- output_index + 1L
    }
  }
  if (!length(output)) return(tibble::tibble())
  result <- dplyr::bind_rows(output)
  dplyr::arrange(result, dplyr::desc(.data$story_score), .data$distance_to_milestone)
}

#' Build performance-based MLB race boards
#'
#' These boards rank descriptive season performance. They are not official
#' award predictions and do not incorporate voting, position, or team record.
#'
#' @param hitters Current-season hitter summary.
#' @param pitchers Current-season pitcher summary expressed as results allowed.
#' @param minimum_pa Minimum hitter plate appearances.
#' @param minimum_bf Minimum pitcher batters faced.
#'
#' @return A list containing offensive and run-prevention race boards.
#' @export
build_league_race_boards <- function(hitters, pitchers, minimum_pa = 100L, minimum_bf = 75L) {
  hitter_required <- c("player_name", "team", "pa", "ops", "woba_estimate", "hard_hit_rate", "run_value_per_pa", "pa_reliability")
  pitcher_required <- c("player_name", "team", "pa", "ops", "woba_estimate", "strikeout_rate", "hard_hit_rate", "pa_reliability")
  if (!is.data.frame(hitters) || !all(hitter_required %in% names(hitters))) stop("`hitters` is missing required summary fields.", call. = FALSE)
  if (!is.data.frame(pitchers) || !all(pitcher_required %in% names(pitchers))) stop("`pitchers` is missing required summary fields.", call. = FALSE)
  offense <- hitters[.safe_numeric(hitters$pa) >= minimum_pa, , drop = FALSE]
  prevention <- pitchers[.safe_numeric(pitchers$pa) >= minimum_bf, , drop = FALSE]
  offense$race_score <- round(100 * (
    0.30 * .percentile(offense$woba_estimate) +
      0.25 * .percentile(offense$ops) +
      0.20 * .percentile(offense$run_value_per_pa) +
      0.10 * .percentile(offense$hard_hit_rate) +
      0.15 * pmax(pmin(.safe_numeric(offense$pa_reliability), 1), 0)
  ), 1)
  offense$race_rank <- .team_rank(offense$race_score)
  offense$race_label <- "descriptive_offensive_performance_not_award_prediction"
  offense <- offense[order(offense$race_rank, -.safe_numeric(offense$pa)), , drop = FALSE]

  prevention$race_score <- round(100 * (
    0.30 * .percentile(prevention$woba_estimate, FALSE) +
      0.25 * .percentile(prevention$ops, FALSE) +
      0.20 * .percentile(prevention$strikeout_rate) +
      0.10 * .percentile(prevention$hard_hit_rate, FALSE) +
      0.15 * pmax(pmin(.safe_numeric(prevention$pa_reliability), 1), 0)
  ), 1)
  prevention$race_rank <- .team_rank(prevention$race_score)
  prevention$race_label <- "descriptive_run_prevention_not_award_prediction"
  prevention <- prevention[order(prevention$race_rank, -.safe_numeric(prevention$pa)), , drop = FALSE]
  list(
    offense = tibble::as_tibble(offense),
    run_prevention = tibble::as_tibble(prevention)
  )
}

#' Summarize team intelligence across offense, pitching, form, and bullpen state
#'
#' @param hitters Current hitter performance summary.
#' @param pitchers Current pitcher performance summary.
#' @param hitter_form Recent hitter form summary.
#' @param pitcher_form Recent pitcher form summary.
#' @param bullpen Bullpen availability summary.
#'
#' @return One row per team with component ranks and a transparent team index.
#' @export
summarize_team_intelligence <- function(hitters, pitchers, hitter_form, pitcher_form, bullpen) {
  inputs <- list(hitters = hitters, pitchers = pitchers, hitter_form = hitter_form, pitcher_form = pitcher_form, bullpen = bullpen)
  if (any(!vapply(inputs, is.data.frame, logical(1)))) stop("All inputs must be data frames.", call. = FALSE)
  teams <- sort(unique(c(as.character(hitters$team), as.character(pitchers$team))))
  teams <- teams[!is.na(teams) & nzchar(teams)]
  rows <- lapply(teams, function(team) {
    h <- hitters[hitters$team == team, , drop = FALSE]
    p <- pitchers[pitchers$team == team, , drop = FALSE]
    hf <- hitter_form[hitter_form$team == team, , drop = FALSE]
    pf <- pitcher_form[pitcher_form$team == team, , drop = FALSE]
    bp <- bullpen[bullpen$team == team, , drop = FALSE]
    form_values <- c(.safe_numeric(hf$form_score), .safe_numeric(pf$form_score))
    top_candidates <- rbind(
      data.frame(player = as.character(hf$player_name), role = "Hitter", score = .safe_numeric(hf$form_score)),
      data.frame(player = as.character(pf$player_name), role = "Pitcher", score = .safe_numeric(pf$form_score))
    )
    top_candidates <- top_candidates[is.finite(top_candidates$score), , drop = FALSE]
    top <- if (nrow(top_candidates)) top_candidates[which.max(top_candidates$score), , drop = FALSE] else data.frame(player = "No qualified signal", role = "", score = NA_real_)
    availability <- if ("availability_score" %in% names(bp)) .safe_numeric(bp$availability_score) else numeric()
    status <- if ("availability_status" %in% names(bp)) tolower(as.character(bp$availability_status)) else character()
    data.frame(
      team = team,
      offense_ops = .weighted_mean_safe(h$ops, h$pa),
      offense_woba = .weighted_mean_safe(h$woba_estimate, h$pa),
      offense_walk_rate = .weighted_mean_safe(h$walk_rate, h$pa),
      offense_strikeout_rate = .weighted_mean_safe(h$strikeout_rate, h$pa),
      offense_hard_hit_rate = .weighted_mean_safe(h$hard_hit_rate, h$batted_balls),
      opponent_ops = .weighted_mean_safe(p$ops, p$pa),
      opponent_woba = .weighted_mean_safe(p$woba_estimate, p$pa),
      pitching_strikeout_rate = .weighted_mean_safe(p$strikeout_rate, p$pa),
      pitching_walk_rate = .weighted_mean_safe(p$walk_rate, p$pa),
      pitching_hard_hit_rate = .weighted_mean_safe(p$hard_hit_rate, p$batted_balls),
      average_form = if (any(is.finite(form_values))) mean(form_values, na.rm = TRUE) else NA_real_,
      surging_signals = sum(form_values >= 65, na.rm = TRUE),
      cooling_signals = sum(form_values <= 35, na.rm = TRUE),
      top_signal = paste0(top$player[[1]], if (nzchar(top$role[[1]])) paste0(" (", top$role[[1]], ")") else ""),
      top_signal_score = top$score[[1]],
      bullpen_arms = nrow(bp),
      bullpen_available = sum(status %in% c("available", "active", "fresh"), na.rm = TRUE),
      bullpen_limited = sum(status %in% c("limited", "unavailable"), na.rm = TRUE),
      bullpen_availability = if (any(is.finite(availability))) mean(availability, na.rm = TRUE) else NA_real_,
      bullpen_pitches_3d = if ("pitches_last_3d" %in% names(bp)) sum(.safe_numeric(bp$pitches_last_3d), na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  result <- dplyr::bind_rows(rows)
  result$offense_rank <- .team_rank(result$offense_woba)
  result$run_prevention_rank <- .team_rank(result$opponent_woba, FALSE)
  result$form_rank <- .team_rank(result$average_form)
  result$bullpen_rank <- .team_rank(result$bullpen_availability)
  result$team_index <- round(100 * (
    0.35 * .percentile(result$offense_woba) +
      0.35 * .percentile(result$opponent_woba, FALSE) +
      0.20 * .percentile(result$average_form) +
      0.10 * .percentile(result$bullpen_availability)
  ), 1)
  result$team_index_rank <- .team_rank(result$team_index)
  result$bullpen_health <- ifelse(
    result$bullpen_availability >= 0.72, "ready",
    ifelse(result$bullpen_availability >= 0.50, "mixed", "taxed")
  )
  result$team_story <- paste0(
    result$team, " ranks ", .ordinal(result$offense_rank), " in offensive quality and ",
    .ordinal(result$run_prevention_rank), " in run prevention, with a ",
    result$bullpen_health, " bullpen and ", result$surging_signals, " surge signals."
  )
  result$team_intelligence_method <- "weighted_team_components_percentile_index_v1"
  result <- result[order(result$team_index_rank), , drop = FALSE]
  tibble::as_tibble(result)
}
