.insane_percentile <- function(x, higher_is_better = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  keep <- is.finite(x)
  output <- rep(NA_real_, length(x))
  if (!any(keep)) return(output)
  ranked <- rank(x[keep], ties.method = "average")
  pct <- if (sum(keep) == 1L) rep(1, sum(keep)) else (ranked - 1) / (sum(keep) - 1)
  if (!higher_is_better) pct <- 1 - pct
  output[keep] <- pct
  output
}

.finalize_insane_award <- function(data, award_id, award_name, description, formula, value_label, eligibility) {
  if (!nrow(data)) return(data.frame())
  data <- data[order(-data$score, -data$tiebreaker), , drop = FALSE]
  data$rank <- seq_len(nrow(data))
  field_sd <- stats::sd(data$score, na.rm = TRUE)
  if (!is.finite(field_sd) || field_sd == 0) field_sd <- 1
  leader_gap <- if (nrow(data) > 1L) data$score[[1L]] - data$score[[2L]] else field_sd
  separation_z <- (data$score[[1L]] - mean(data$score, na.rm = TRUE)) / field_sd
  closeness <- 100 * (1 - min(abs(leader_gap) / field_sd, 1))
  showcase <- 100 * (0.55 * min(max(separation_z / 3, 0), 1) + 0.45 * closeness / 100)
  data.frame(
    award_id = award_id, award_name = award_name, description = description, formula = formula,
    rank = data$rank, player_id = data$player_id, player_name = data$player_name, team = data$team,
    role = data$role, score = round(data$score, 1), display_value = data$display_value,
    value_label = value_label, evidence = data$evidence, eligibility = eligibility,
    leader_separation_z = round(separation_z, 2), leader_gap = round(leader_gap, 2),
    race_closeness = round(closeness, 1), showcase_score = round(showcase, 1),
    stringsAsFactors = FALSE
  )
}

#' Build the Insane Baseball Awards candidate boards
#'
#' Creates playful, transparent leaderboards from unusual player profiles and
#' count-specific outcomes. Each award also receives a showcase score based on
#' how far the leader separates from the field and how close the race is at the
#' top.
#'
#' @param hitter_summary Output from [summarize_hitters()].
#' @param pitcher_summary Output from [summarize_pitchers()].
#' @param pitches Canonical pitch view from [build_pitch_view()].
#' @param minimum_pa Minimum season PA/BF for profile awards.
#' @param minimum_split_pa Minimum count-specific PA.
#' @return Combined award candidate board.
#' @export
build_insane_baseball_awards <- function(hitter_summary, pitcher_summary, pitches, minimum_pa = 100L, minimum_split_pa = 15L) {
  stopifnot(is.data.frame(hitter_summary), is.data.frame(pitcher_summary), is.data.frame(pitches))
  profile_required <- c("player_id", "player_name", "team", "pa", "average_exit_velocity", "ground_ball_rate")
  pitch_required <- c("game_pk", "at_bat_index", "pitch_in_pa", "is_terminal_pitch", "balls_before", "strikes_before", "batter_id", "batter_name", "batting_team", "pitcher_id", "pitcher_name", "fielding_team", "event_key")
  if (length(setdiff(profile_required, names(hitter_summary)))) stop("`hitter_summary` is missing profile fields.", call. = FALSE)
  if (length(setdiff(pitch_required, names(pitches)))) stop("`pitches` is missing count-context fields.", call. = FALSE)

  earth <- hitter_summary[hitter_summary$pa >= minimum_pa & is.finite(hitter_summary$average_exit_velocity) & is.finite(hitter_summary$ground_ball_rate), , drop = FALSE]
  earth$ev_pct <- .insane_percentile(earth$average_exit_velocity)
  earth$gb_pct <- .insane_percentile(earth$ground_ball_rate)
  earth_rows <- data.frame(
    player_id = earth$player_id, player_name = earth$player_name, team = earth$team, role = "hitter",
    score = 100 * (0.5 * earth$ev_pct + 0.5 * earth$gb_pct), tiebreaker = earth$average_exit_velocity,
    display_value = paste0(round(earth$average_exit_velocity, 1), " mph | ", round(100 * earth$ground_ball_rate, 1), "% GB"),
    evidence = paste0("Exit velocity and ground-ball rate receive equal percentile weight."), stringsAsFactors = FALSE
  )

  terminal <- pitches[pitches$is_terminal_pitch %in% TRUE, , drop = FALSE]
  terminal <- terminal[!duplicated(paste(terminal$game_pk, terminal$at_bat_index, sep = "_")), , drop = FALSE]
  terminal$is_hit <- terminal$event_key %in% c("single", "double", "triple", "home_run")
  terminal$is_walk <- terminal$event_key %in% c("walk", "intent_walk", "intentional_walk")
  terminal$is_strikeout <- terminal$event_key %in% c("strikeout", "strikeout_double_play")
  terminal$is_out_of_zone <- if ("is_out_of_zone" %in% names(terminal)) terminal$is_out_of_zone %in% TRUE else FALSE
  terminal$total_bases <- ifelse(terminal$event_key == "single", 1, ifelse(terminal$event_key == "double", 2, ifelse(terminal$event_key == "triple", 3, ifelse(terminal$event_key == "home_run", 4, 0))))
  full_count_keys <- unique(pitches[pitches$balls_before == 3L & pitches$strikes_before == 2L,
    c("game_pk", "at_bat_index"), drop = FALSE])
  full <- merge(terminal, full_count_keys, by = c("game_pk", "at_bat_index"), all = FALSE, sort = FALSE)

  batter_full <- full |>
    dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
    dplyr::group_by(.data$batter_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$batter_name), team = .last_non_missing(.data$batting_team),
      split_pa = dplyr::n(), hits = sum(.data$is_hit), walks = sum(.data$is_walk), total_bases = sum(.data$total_bases), .groups = "drop") |>
    dplyr::filter(.data$split_pa >= minimum_split_pa)
  batter_full$obp <- (batter_full$hits + batter_full$walks) / batter_full$split_pa
  batter_full$slg_proxy <- batter_full$total_bases / pmax(batter_full$split_pa - batter_full$walks, 1)
  batter_full$ops_proxy <- batter_full$obp + batter_full$slg_proxy
  survivor_rows <- data.frame(
    player_id = batter_full$batter_id, player_name = batter_full$player_name, team = batter_full$team, role = "hitter",
    score = 100 * batter_full$ops_proxy, tiebreaker = batter_full$split_pa,
    display_value = sprintf("%.3f OPS", batter_full$ops_proxy),
    evidence = paste0(batter_full$hits, " hits and ", batter_full$walks, " walks in ", batter_full$split_pa, " full-count PA."), stringsAsFactors = FALSE
  )

  pitcher_full <- full |>
    dplyr::filter(!is.na(.data$pitcher_id), .data$pitcher_id != "") |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$pitcher_name), team = .last_non_missing(.data$fielding_team),
      split_pa = dplyr::n(), strikeouts = sum(.data$is_strikeout), .groups = "drop") |>
    dplyr::filter(.data$split_pa >= minimum_split_pa)
  pitcher_full$strikeout_rate <- pitcher_full$strikeouts / pitcher_full$split_pa
  executioner_rows <- data.frame(
    player_id = pitcher_full$pitcher_id, player_name = pitcher_full$player_name, team = pitcher_full$team, role = "pitcher",
    score = pitcher_full$strikeouts, tiebreaker = pitcher_full$strikeout_rate,
    display_value = paste0(pitcher_full$strikeouts, " K"),
    evidence = paste0(round(100 * pitcher_full$strikeout_rate, 1), "% strikeout rate across ", pitcher_full$split_pa, " full-count PA."), stringsAsFactors = FALSE
  )

  first_pitch <- terminal[terminal$pitch_in_pa == 1L, , drop = FALSE]
  ambush <- first_pitch |>
    dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
    dplyr::group_by(.data$batter_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$batter_name), team = .last_non_missing(.data$batting_team),
      split_pa = dplyr::n(), hits = sum(.data$is_hit), home_runs = sum(.data$event_key == "home_run"), total_bases = sum(.data$total_bases), .groups = "drop") |>
    dplyr::filter(.data$split_pa >= minimum_split_pa)
  ambush$slugging <- ambush$total_bases / ambush$split_pa
  ambush_rows <- data.frame(
    player_id = ambush$batter_id, player_name = ambush$player_name, team = ambush$team, role = "hitter",
    score = 100 * ambush$slugging, tiebreaker = ambush$home_runs,
    display_value = sprintf("%.3f SLG", ambush$slugging),
    evidence = paste0(ambush$hits, " hits, ", ambush$home_runs, " HR in ", ambush$split_pa, " one-pitch PA."), stringsAsFactors = FALSE
  )

  two_strike_keys <- unique(pitches[pitches$strikes_before >= 2L, c("game_pk", "at_bat_index"), drop = FALSE])
  two_strike <- merge(terminal, two_strike_keys, by = c("game_pk", "at_bat_index"), all = FALSE, sort = FALSE)
  escape <- two_strike |>
    dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
    dplyr::group_by(.data$batter_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$batter_name), team = .last_non_missing(.data$batting_team),
      split_pa = dplyr::n(), hits = sum(.data$is_hit), walks = sum(.data$is_walk), strikeouts = sum(.data$is_strikeout),
      total_bases = sum(.data$total_bases), .groups = "drop") |>
    dplyr::filter(.data$split_pa >= minimum_split_pa)
  escape$obp <- (escape$hits + escape$walks) / escape$split_pa
  escape$slg_proxy <- escape$total_bases / pmax(escape$split_pa - escape$walks, 1)
  escape$ops_proxy <- escape$obp + escape$slg_proxy
  escape_rows <- if (nrow(escape)) data.frame(
    player_id = escape$batter_id, player_name = escape$player_name, team = escape$team, role = rep("hitter", nrow(escape)),
    score = 100 * escape$ops_proxy, tiebreaker = -escape$strikeouts,
    display_value = sprintf("%.3f OPS", escape$ops_proxy),
    evidence = paste0(escape$hits, " hits, ", escape$walks, " walks, and ", escape$strikeouts,
      " strikeouts after reaching two strikes."), stringsAsFactors = FALSE
  ) else data.frame()

  zero_two_keys <- unique(pitches[pitches$balls_before == 0L & pitches$strikes_before >= 2L,
    c("game_pk", "at_bat_index"), drop = FALSE])
  no_mercy <- merge(terminal, zero_two_keys, by = c("game_pk", "at_bat_index"), all = FALSE, sort = FALSE) |>
    dplyr::filter(!is.na(.data$pitcher_id), .data$pitcher_id != "") |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$pitcher_name), team = .last_non_missing(.data$fielding_team),
      split_pa = dplyr::n(), strikeouts = sum(.data$is_strikeout), .groups = "drop") |>
    dplyr::filter(.data$split_pa >= minimum_split_pa)
  no_mercy$strikeout_rate <- no_mercy$strikeouts / no_mercy$split_pa
  no_mercy_rows <- if (nrow(no_mercy)) data.frame(
    player_id = no_mercy$pitcher_id, player_name = no_mercy$player_name, team = no_mercy$team, role = rep("pitcher", nrow(no_mercy)),
    score = no_mercy$strikeouts, tiebreaker = no_mercy$strikeout_rate,
    display_value = paste0(no_mercy$strikeouts, " K"),
    evidence = paste0(round(100 * no_mercy$strikeout_rate, 1), "% strikeout rate in ", no_mercy$split_pa,
      " plate appearances that reached 0-2."), stringsAsFactors = FALSE
  ) else data.frame()

  bad_ball_minimum <- max(5L, floor(minimum_split_pa / 2))
  bad_ball <- terminal[terminal$is_out_of_zone & terminal$is_hit, , drop = FALSE] |>
    dplyr::filter(!is.na(.data$batter_id), .data$batter_id != "") |>
    dplyr::group_by(.data$batter_id) |>
    dplyr::summarise(player_name = .last_non_missing(.data$batter_name), team = .last_non_missing(.data$batting_team),
      hits = dplyr::n(), total_bases = sum(.data$total_bases), .groups = "drop") |>
    dplyr::filter(.data$hits >= bad_ball_minimum)
  bad_ball_rows <- if (nrow(bad_ball)) data.frame(
    player_id = bad_ball$batter_id, player_name = bad_ball$player_name, team = bad_ball$team, role = rep("hitter", nrow(bad_ball)),
    score = bad_ball$total_bases, tiebreaker = bad_ball$hits,
    display_value = paste0(bad_ball$total_bases, " TB"),
    evidence = paste0(bad_ball$hits, " hits on terminal pitches outside the strike zone."), stringsAsFactors = FALSE
  ) else data.frame()

  profile_boards <- list()
  if (all(c("walk_rate", "strikeout_rate") %in% names(hitter_summary))) {
    velvet <- hitter_summary[hitter_summary$pa >= minimum_pa & is.finite(hitter_summary$walk_rate) & is.finite(hitter_summary$strikeout_rate), , drop = FALSE]
    velvet$score_value <- 100 * (0.5 * .insane_percentile(velvet$walk_rate) + 0.5 * .insane_percentile(velvet$strikeout_rate, FALSE))
    velvet_rows <- data.frame(player_id = velvet$player_id, player_name = velvet$player_name, team = velvet$team, role = "hitter",
      score = velvet$score_value, tiebreaker = velvet$walk_rate - velvet$strikeout_rate,
      display_value = paste0(round(100 * velvet$walk_rate, 1), "% BB | ", round(100 * velvet$strikeout_rate, 1), "% K"),
      evidence = "Walk-rate percentile and inverse strikeout-rate percentile receive equal weight.", stringsAsFactors = FALSE)
    profile_boards[["velvet"]] <- .finalize_insane_award(velvet_rows, "velvet_rope", "Velvet Rope Award",
      "Who controls the strike zone so well that pitchers rarely get an easy out?",
      "50% walk-rate percentile + 50% inverse strikeout-rate percentile", "Discipline profile", paste0("Minimum ", minimum_pa, " PA"))
  }
  if (all(c("pull_rate", "fly_ball_rate") %in% names(hitter_summary))) {
    porch <- hitter_summary[hitter_summary$pa >= minimum_pa & is.finite(hitter_summary$pull_rate) & is.finite(hitter_summary$fly_ball_rate), , drop = FALSE]
    porch$score_value <- 100 * (0.5 * .insane_percentile(porch$pull_rate) + 0.5 * .insane_percentile(porch$fly_ball_rate))
    porch_rows <- data.frame(player_id = porch$player_id, player_name = porch$player_name, team = porch$team, role = "hitter",
      score = porch$score_value, tiebreaker = porch$pull_rate,
      display_value = paste0(round(100 * porch$pull_rate, 1), "% pull | ", round(100 * porch$fly_ball_rate, 1), "% FB"),
      evidence = "Pull-rate and fly-ball-rate percentiles receive equal weight.", stringsAsFactors = FALSE)
    profile_boards[["porch"]] <- .finalize_insane_award(porch_rows, "short_porch_architect", "Short Porch Architect",
      "Who most consistently puts airborne contact toward the pull-side seats?",
      "50% pull-rate percentile + 50% fly-ball-rate percentile", "Pulled-air-ball profile", paste0("Minimum ", minimum_pa, " PA"))
  }

  boards <- list(
    .finalize_insane_award(earth_rows, "earthworm_killer", "Earthworm Killer Award", "Who combines the loudest contact with the strongest ground-ball habit?", "50% exit-velocity percentile + 50% ground-ball-rate percentile", "EV and GB profile", paste0("Minimum ", minimum_pa, " PA")),
    .finalize_insane_award(survivor_rows, "full_count_survivor", "Full Count Survivor", "Who produces the most after the count reaches its final possible pitch?", "On-base percentage + slugging proxy in 3-2 plate appearances", "Full-count OPS", paste0("Minimum ", minimum_split_pa, " full-count PA")),
    .finalize_insane_award(executioner_rows, "full_count_executioner", "Full Count Executioner", "Which pitcher has finished the most full counts with a strikeout?", "Total strikeouts in 3-2 plate appearances; rate breaks ties", "Full-count strikeouts", paste0("Minimum ", minimum_split_pa, " full-count PA")),
    .finalize_insane_award(ambush_rows, "ambush_artist", "Ambush Artist", "Who does the most damage when the plate appearance ends on pitch one?", "Slugging percentage in one-pitch plate appearances", "First-pitch-ending SLG", paste0("Minimum ", minimum_split_pa, " one-pitch PA")),
    .finalize_insane_award(escape_rows, "two_strike_escape_artist", "Two-Strike Escape Artist", "Who keeps producing after the pitcher reaches the doorstep?", "OPS proxy in plate appearances that reached two strikes", "Two-strike OPS", paste0("Minimum ", minimum_split_pa, " two-strike PA")),
    .finalize_insane_award(no_mercy_rows, "no_mercy", "No Mercy Award", "Which pitcher most often turns an 0-2 advantage into a strikeout?", "Total strikeouts in plate appearances that reached 0-2; rate breaks ties", "0-2 strikeouts", paste0("Minimum ", minimum_split_pa, " 0-2 PA")),
    .finalize_insane_award(bad_ball_rows, "bad_ball_bandit", "Bad Ball Bandit", "Who steals the most total bases from pitches outside the strike zone?", "Total bases on terminal pitches outside the strike zone", "Out-of-zone total bases", paste0("Minimum ", bad_ball_minimum, " out-of-zone hits"))
  )
  boards <- c(boards, profile_boards)
  output <- do.call(rbind, boards[vapply(boards, nrow, integer(1)) > 0L])
  award_scores <- unique(output[c("award_id", "showcase_score")])
  featured_ids <- utils::head(award_scores$award_id[order(-award_scores$showcase_score)], 3L)
  output$featured <- output$award_id %in% featured_ids
  output$award_method <- "insane_baseball_awards_v2"
  rownames(output) <- NULL
  output
}
