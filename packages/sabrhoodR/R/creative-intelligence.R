.replace_missing_score <- function(value, replacement = 0.5) {
  value <- .safe_numeric(value)
  value[!is.finite(value)] <- replacement
  pmax(pmin(value, 1), 0)
}

#' Build hitter and pitcher platoon-edge boards
#'
#' @param hitter_platoon Hitter summaries grouped by opponent hand.
#' @param pitcher_platoon Pitcher summaries grouped by opponent hand.
#' @param minimum_pa Minimum plate appearances required on each side.
#'
#' @return A list containing hitter and pitcher matchup-edge boards.
#' @export
build_platoon_edge_boards <- function(hitter_platoon, pitcher_platoon, minimum_pa = 40L) {
  required <- c("player_id", "player_name", "team", "opponent_hand", "pa", "ops", "woba_estimate", "pa_reliability")
  if (!is.data.frame(hitter_platoon) || !all(required %in% names(hitter_platoon))) stop("`hitter_platoon` is missing required split fields.", call. = FALSE)
  if (!is.data.frame(pitcher_platoon) || !all(required %in% names(pitcher_platoon))) stop("`pitcher_platoon` is missing required split fields.", call. = FALSE)

  build_board <- function(data, perspective) {
    data$opponent_hand <- toupper(as.character(data$opponent_hand))
    data <- data[data$opponent_hand %in% c("L", "R") & .safe_numeric(data$pa) >= minimum_pa, , drop = FALSE]
    ids <- unique(as.character(data$player_id))
    rows <- lapply(ids, function(id) {
      player <- data[as.character(data$player_id) == id, , drop = FALSE]
      left <- player[player$opponent_hand == "L", , drop = FALSE]
      right <- player[player$opponent_hand == "R", , drop = FALSE]
      if (!nrow(left) || !nrow(right)) return(NULL)
      left <- left[which.max(.safe_numeric(left$pa)), , drop = FALSE]
      right <- right[which.max(.safe_numeric(right$pa)), , drop = FALSE]
      woba_l <- .safe_numeric(left$woba_estimate[[1L]])
      woba_r <- .safe_numeric(right$woba_estimate[[1L]])
      if (!is.finite(woba_l) || !is.finite(woba_r)) return(NULL)
      if (perspective == "hitter") {
        stronger <- if (woba_l >= woba_r) "L" else "R"
        label <- if (stronger == "L") "left-handed pitching" else "right-handed pitching"
        headline <- paste0(left$player_name[[1L]], " is doing more damage against ", label)
      } else {
        stronger <- if (woba_l <= woba_r) "L" else "R"
        label <- if (stronger == "L") "left-handed batters" else "right-handed batters"
        headline <- paste0(left$player_name[[1L]], " is suppressing ", label, " more effectively")
      }
      data.frame(
        player_id = id,
        player_name = left$player_name[[1L]],
        team = left$team[[1L]],
        player_hand = if ("hand" %in% names(left)) left$hand[[1L]] else NA_character_,
        perspective = perspective,
        stronger_opponent_hand = stronger,
        pa_vs_l = .safe_numeric(left$pa[[1L]]),
        pa_vs_r = .safe_numeric(right$pa[[1L]]),
        ops_vs_l = .safe_numeric(left$ops[[1L]]),
        ops_vs_r = .safe_numeric(right$ops[[1L]]),
        woba_vs_l = woba_l,
        woba_vs_r = woba_r,
        woba_gap = abs(woba_l - woba_r),
        split_reliability = min(.safe_numeric(left$pa_reliability[[1L]]), .safe_numeric(right$pa_reliability[[1L]]), na.rm = TRUE),
        headline = headline,
        evidence = paste0("Estimated wOBA: ", sprintf("%.3f", woba_l), " vs L, ", sprintf("%.3f", woba_r), " vs R."),
        matchup_method = "two_sided_opponent_hand_woba_gap_v1",
        stringsAsFactors = FALSE
      )
    })
    board <- dplyr::bind_rows(rows)
    if (!nrow(board)) return(tibble::tibble())
    board$matchup_edge_score <- round(100 * (
      0.65 * .replace_missing_score(.percentile(board$woba_gap)) +
        0.20 * .replace_missing_score(board$split_reliability) +
        0.15 * .replace_missing_score(.percentile(pmin(board$pa_vs_l, board$pa_vs_r)))
    ), 1)
    board$matchup_edge_rank <- .team_rank(board$matchup_edge_score)
    board <- board[order(board$matchup_edge_rank), , drop = FALSE]
    tibble::as_tibble(board)
  }

  list(
    hitters = build_board(hitter_platoon, "hitter"),
    pitchers = build_board(pitcher_platoon, "pitcher")
  )
}

#' Build a signature-pitch identity board
#'
#' @param pitch_types Pitch-type performance summaries.
#' @param minimum_pitches Minimum pitch count.
#' @param minimum_swings Minimum swing count.
#'
#' @return Ranked pitch identities with quality components and visual fields.
#' @export
build_signature_pitch_board <- function(pitch_types, minimum_pitches = 100L, minimum_swings = 35L) {
  required <- c(
    "player_id", "player_name", "team", "pitch_name", "pitches", "swings",
    "usage_rate", "whiff_rate", "chase_rate", "hard_hit_rate", "putaway_rate",
    "pitch_reliability", "average_velocity", "average_horizontal_break",
    "average_induced_vertical_break"
  )
  if (!is.data.frame(pitch_types) || !all(required %in% names(pitch_types))) {
    stop("`pitch_types` is missing required pitch identity fields.", call. = FALSE)
  }
  board <- pitch_types[
    .safe_numeric(pitch_types$pitches) >= minimum_pitches & .safe_numeric(pitch_types$swings) >= minimum_swings,
    ,
    drop = FALSE
  ]
  if (!nrow(board)) return(tibble::tibble())
  board$pitch_quality_score <- round(100 * (
    0.30 * .replace_missing_score(.percentile(board$whiff_rate)) +
      0.18 * .replace_missing_score(.percentile(board$chase_rate)) +
      0.16 * .replace_missing_score(.percentile(board$hard_hit_rate, FALSE)) +
      0.16 * .replace_missing_score(.percentile(board$putaway_rate)) +
      0.10 * .replace_missing_score(.percentile(board$usage_rate)) +
      0.10 * .replace_missing_score(board$pitch_reliability)
  ), 1)
  pitch_name <- tolower(as.character(board$pitch_name))
  board$pitch_family <- ifelse(
    grepl("fastball|sinker|cutter", pitch_name), "Fastball",
    ifelse(grepl("slider|sweeper|curve|slurve", pitch_name), "Breaking", "Offspeed")
  )
  board$movement_total <- sqrt(.safe_numeric(board$average_horizontal_break)^2 + .safe_numeric(board$average_induced_vertical_break)^2)
  board$identity_headline <- paste0(
    board$player_name, "'s ", board$pitch_name, ": ",
    sprintf("%.1f", .safe_numeric(board$average_velocity)), " mph, ",
    sprintf("%.1f", 100 * .safe_numeric(board$whiff_rate)), "% whiffs"
  )
  board$pitch_identity_method <- "outcome_shape_usage_percentile_v1"
  board$pitch_quality_rank <- .team_rank(board$pitch_quality_score)
  board <- board[order(board$pitch_quality_rank, -.safe_numeric(board$pitches)), , drop = FALSE]
  tibble::as_tibble(board)
}

#' Build a diversified daily editorial story queue
#'
#' @param hitter_form Recent hitter-form summary.
#' @param pitcher_form Recent pitcher-form summary.
#' @param offensive_race Offensive performance race board.
#' @param prevention_race Run-prevention race board.
#' @param active_milestones Active milestone watch.
#' @param historical Historical anniversary candidates.
#' @param team_intelligence Team intelligence summary.
#' @param signature_pitches Signature pitch board.
#'
#' @return Ranked and category-diversified editorial candidates.
#' @export
build_daily_story_queue <- function(
    hitter_form,
    pitcher_form,
    offensive_race,
    prevention_race,
    active_milestones,
    historical,
    team_intelligence,
    signature_pitches) {
  candidate <- list()
  add <- function(category, subject, team, headline, evidence, source_score, confidence, destination, id) {
    data.frame(
      story_id = paste(category, id, sep = "_"), category = category,
      subject = as.character(subject), team = as.character(team),
      headline = as.character(headline), evidence = as.character(evidence),
      source_score = .safe_numeric(source_score), confidence = .safe_numeric(confidence),
      destination = destination, stringsAsFactors = FALSE
    )
  }
  candidate[[1L]] <- add("hitter_form", hitter_form$player_name, hitter_form$team,
    paste0(hitter_form$player_name, " is changing the recent-performance baseline"),
    paste0("Recent OPS ", sprintf("%.3f", .safe_numeric(hitter_form$recent_ops)), "; change ", sprintf("%+.3f", .safe_numeric(hitter_form$ops_delta)), "."),
    hitter_form$form_score, hitter_form$form_score_confidence, "today.html", hitter_form$player_id)
  candidate[[2L]] <- add("pitcher_form", pitcher_form$player_name, pitcher_form$team,
    paste0(pitcher_form$player_name, " is changing the contact-quality conversation"),
    paste0("Recent OPS allowed ", sprintf("%.3f", .safe_numeric(pitcher_form$recent_ops)), "; change ", sprintf("%+.3f", .safe_numeric(pitcher_form$ops_delta)), "."),
    pitcher_form$form_score, pitcher_form$form_score_confidence, "today.html", pitcher_form$player_id)
  candidate[[3L]] <- add("offensive_race", offensive_race$player_name, offensive_race$team,
    paste0(offensive_race$player_name, " is building one of baseball's strongest offensive cases"),
    paste0("OPS ", sprintf("%.3f", .safe_numeric(offensive_race$ops)), "; race score ", sprintf("%.1f", .safe_numeric(offensive_race$race_score)), "."),
    offensive_race$race_score, offensive_race$pa_reliability, "races.html", offensive_race$player_id)
  candidate[[4L]] <- add("run_prevention", prevention_race$player_name, prevention_race$team,
    paste0(prevention_race$player_name, " is suppressing offense at an elite rate"),
    paste0("OPS allowed ", sprintf("%.3f", .safe_numeric(prevention_race$ops)), "; race score ", sprintf("%.1f", .safe_numeric(prevention_race$race_score)), "."),
    prevention_race$race_score, prevention_race$pa_reliability, "races.html", prevention_race$player_id)
  candidate[[5L]] <- add("milestone", active_milestones$player_name, active_milestones$team,
    active_milestones$headline,
    paste0("Career estimate ", format(round(.safe_numeric(active_milestones$career_to_date_value)), big.mark = ","), "."),
    active_milestones$story_score, active_milestones$identity_match_confidence, "history.html", active_milestones$player_id)
  candidate[[6L]] <- add("history", historical$subject_name, "MLB history", historical$headline,
    historical$career_summary, historical$story_score, historical$confidence, "history.html", historical$note_id)
  candidate[[7L]] <- add("team", team_intelligence$team, team_intelligence$team,
    paste0(team_intelligence$team, " has a new league-wide reporting lead"),
    team_intelligence$team_story, team_intelligence$team_index, .replace_missing_score(team_intelligence$team_index / 100), "teams.html", team_intelligence$team)
  candidate[[8L]] <- add("pitch_identity", signature_pitches$player_name, signature_pitches$team,
    signature_pitches$identity_headline,
    paste0("Pitch quality score ", sprintf("%.1f", .safe_numeric(signature_pitches$pitch_quality_score)), "."),
    signature_pitches$pitch_quality_score, signature_pitches$pitch_reliability, "pitch-lab.html", paste(signature_pitches$player_id, signature_pitches$pitch_type, sep = "_"))
  queue <- dplyr::bind_rows(candidate)
  queue$source_score <- pmax(pmin(queue$source_score, 100), 0)
  queue$confidence <- pmax(pmin(queue$confidence, 1), 0)
  queue$story_score <- round(0.82 * queue$source_score + 18 * queue$confidence, 1)
  queue$story_score_method <- "cross_product_editorial_queue_v1"
  queue <- queue[order(-queue$story_score, queue$category), , drop = FALSE]
  queue$category_rank <- stats::ave(-queue$story_score, queue$category, FUN = function(value) rank(value, ties.method = "first"))
  queue$daily_shortlist <- queue$category_rank <= 2
  queue$queue_rank <- seq_len(nrow(queue))
  tibble::as_tibble(queue)
}

#' Build three broadcaster-ready research notes per team
#'
#' @param team_intelligence Team intelligence summary.
#' @param hitter_form Recent hitter-form summary.
#' @param pitcher_form Recent pitcher-form summary.
#' @param signature_pitches Signature pitch board.
#' @param hitter_matchups Hitter matchup-edge board.
#' @param pitcher_matchups Pitcher matchup-edge board.
#' @param active_milestones Active milestone watch.
#'
#' @return Three concise, evidence-linked research leads per team.
#' @export
build_team_broadcast_notes <- function(
    team_intelligence,
    hitter_form,
    pitcher_form,
    signature_pitches,
    hitter_matchups,
    pitcher_matchups,
    active_milestones) {
  if (!is.data.frame(team_intelligence) || !all(c("team", "team_story", "team_index") %in% names(team_intelligence))) {
    stop("`team_intelligence` is missing required fields.", call. = FALSE)
  }
  teams <- as.character(team_intelligence$team)
  output <- list()
  output_index <- 1L
  for (team in teams) {
    team_row <- team_intelligence[team_intelligence$team == team, , drop = FALSE][1L, ]
    output[[output_index]] <- data.frame(
      team = team, note_order = 1L, note_category = "team_identity", subject = team,
      headline = paste0(team, "'s analytical identity"), evidence = team_row$team_story[[1L]],
      signal_score = .safe_numeric(team_row$team_index[[1L]]), destination = "teams.html",
      note_method = "team_broadcast_three_v1", stringsAsFactors = FALSE
    )
    output_index <- output_index + 1L

    form_candidates <- dplyr::bind_rows(
      data.frame(
        player_name = as.character(hitter_form$player_name), team = as.character(hitter_form$team),
        role = "hitter", score = .safe_numeric(hitter_form$form_score),
        recent = .safe_numeric(hitter_form$recent_ops), baseline = .safe_numeric(hitter_form$baseline_ops)
      ),
      data.frame(
        player_name = as.character(pitcher_form$player_name), team = as.character(pitcher_form$team),
        role = "pitcher", score = .safe_numeric(pitcher_form$form_score),
        recent = .safe_numeric(pitcher_form$recent_ops), baseline = .safe_numeric(pitcher_form$baseline_ops)
      )
    )
    form_candidates <- form_candidates[form_candidates$team == team & is.finite(form_candidates$score), , drop = FALSE]
    if (nrow(form_candidates)) {
      form <- form_candidates[which.max(form_candidates$score), , drop = FALSE]
      evidence <- if (form$role[[1L]] == "hitter") {
        paste0("Recent OPS ", sprintf("%.3f", form$recent[[1L]]), " versus ", sprintf("%.3f", form$baseline[[1L]]), " previously.")
      } else {
        paste0("Recent OPS allowed ", sprintf("%.3f", form$recent[[1L]]), " versus ", sprintf("%.3f", form$baseline[[1L]]), " previously.")
      }
      output[[output_index]] <- data.frame(
        team = team, note_order = 2L, note_category = "form_driver", subject = form$player_name[[1L]],
        headline = paste0(form$player_name[[1L]], " is the strongest recent ", form$role[[1L]], " signal"),
        evidence = evidence, signal_score = form$score[[1L]], destination = "today.html",
        note_method = "team_broadcast_three_v1", stringsAsFactors = FALSE
      )
      output_index <- output_index + 1L
    }

    hooks <- list()
    pitch <- signature_pitches[signature_pitches$team == team, , drop = FALSE]
    if (nrow(pitch)) {
      pitch <- pitch[which.max(.safe_numeric(pitch$pitch_quality_score)), , drop = FALSE]
      hooks[[length(hooks) + 1L]] <- data.frame(
        category = "signature_pitch", subject = pitch$player_name[[1L]], headline = pitch$identity_headline[[1L]],
        evidence = paste0("Quality score ", sprintf("%.1f", .safe_numeric(pitch$pitch_quality_score[[1L]])),
          "; usage ", sprintf("%.1f%%", 100 * .safe_numeric(pitch$usage_rate[[1L]])), "."),
        score = .safe_numeric(pitch$pitch_quality_score[[1L]]), destination = "pitch-lab.html"
      )
    }
    milestone <- active_milestones[active_milestones$team == team, , drop = FALSE]
    if (nrow(milestone)) {
      milestone <- milestone[which.max(.safe_numeric(milestone$story_score)), , drop = FALSE]
      hooks[[length(hooks) + 1L]] <- data.frame(
        category = "milestone", subject = milestone$player_name[[1L]], headline = milestone$headline[[1L]],
        evidence = paste0("Career estimate ", format(round(.safe_numeric(milestone$career_to_date_value[[1L]])), big.mark = ","), "."),
        score = .safe_numeric(milestone$story_score[[1L]]), destination = "history.html"
      )
    }
    matchup <- dplyr::bind_rows(hitter_matchups, pitcher_matchups)
    matchup <- matchup[matchup$team == team, , drop = FALSE]
    if (nrow(matchup)) {
      matchup <- matchup[which.max(.safe_numeric(matchup$matchup_edge_score)), , drop = FALSE]
      hooks[[length(hooks) + 1L]] <- data.frame(
        category = "matchup_edge", subject = matchup$player_name[[1L]], headline = matchup$headline[[1L]],
        evidence = matchup$evidence[[1L]], score = .safe_numeric(matchup$matchup_edge_score[[1L]]),
        destination = "matchups.html"
      )
    }
    if (length(hooks)) {
      hook <- dplyr::bind_rows(hooks)
      hook <- hook[which.max(hook$score), , drop = FALSE]
      output[[output_index]] <- data.frame(
        team = team, note_order = 3L, note_category = hook$category[[1L]], subject = hook$subject[[1L]],
        headline = hook$headline[[1L]], evidence = hook$evidence[[1L]], signal_score = hook$score[[1L]],
        destination = hook$destination[[1L]], note_method = "team_broadcast_three_v1", stringsAsFactors = FALSE
      )
      output_index <- output_index + 1L
    }
  }
  notes <- dplyr::bind_rows(output)
  dplyr::arrange(notes, .data$team, .data$note_order)
}
