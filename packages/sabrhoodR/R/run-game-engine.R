.run_game_id <- function(x) {
  output <- as.character(x)
  output[is.na(x) | output %in% c("", "NA")] <- NA_character_
  output
}

.run_game_probability <- function(x) {
  pmin(pmax(as.numeric(x), 1e-5), 1 - 1e-5)
}

.run_game_logit <- function(x) {
  stats::qlogis(.run_game_probability(x))
}

.run_game_pitch_family <- function(code, name = NA_character_) {
  code <- toupper(as.character(code))
  name <- tolower(as.character(name))
  dplyr::case_when(
    code %in% c("FF", "FA", "FT", "SI", "FC") |
      grepl("fastball|sinker|cutter", name) ~ "fastball",
    code %in% c("SL", "ST", "SV", "CU", "KC", "CS") |
      grepl("slider|sweeper|curve|slurve", name) ~ "breaking",
    code %in% c("CH", "FS", "FO", "SC", "KN") |
      grepl("change|split|fork|screw|knuckle", name) ~ "offspeed",
    TRUE ~ "other"
  )
}

.run_game_first <- function(x, fallback = NA_character_) {
  available <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(available)) available[[1L]] else fallback
}

.run_game_column <- function(data, candidates, default = NA) {
  existing <- candidates[candidates %in% names(data)]
  if (!length(existing)) return(rep(default, nrow(data)))
  data[[existing[[1L]]]]
}

#' Reconstruct active-catcher defensive stints
#'
#' Builds game-level catcher intervals from a supplied starting-catcher table
#' and MLB defensive substitution events. When starts are unavailable, a
#' player reference with a `position` column can provide a clearly labeled
#' game-lineup inference.
#'
#' @param pbp Raw MLB play-by-play.
#' @param starting_catchers Optional data frame with game, team, catcher ID,
#'   catcher name, and optional starting at-bat index.
#' @param player_reference Optional player table containing `player_id`,
#'   `player_name`, and `position`.
#'
#' @return One row per catcher stint.
#' @export
build_catcher_stints <- function(
  pbp,
  starting_catchers = NULL,
  player_reference = NULL
) {
  if (!is.data.frame(pbp) || !nrow(pbp)) {
    stop("`pbp` must be a non-empty raw play-by-play data frame.", call. = FALSE)
  }

  game_pk <- .run_game_id(.run_game_column(pbp, c("game_pk", "gamePk")))
  at_bat_index <- suppressWarnings(as.integer(.run_game_column(
    pbp,
    c("about.atBatIndex", "atBatIndex"),
    0L
  )))
  fielding_team <- as.character(.run_game_column(
    pbp,
    c("fielding_team", "fieldingTeam")
  ))
  batting_team <- as.character(.run_game_column(
    pbp,
    c("batting_team", "battingTeam")
  ))

  starts <- data.frame()
  if (is.data.frame(starting_catchers) && nrow(starting_catchers)) {
    starts <- data.frame(
      game_pk = .run_game_id(.run_game_column(
        starting_catchers,
        c("game_pk", "game_id", "gamePk")
      )),
      fielding_team = as.character(.run_game_column(
        starting_catchers,
        c("fielding_team", "team", "team_name")
      )),
      catcher_id = .run_game_id(.run_game_column(
        starting_catchers,
        c("catcher_id", "player_id")
      )),
      catcher_name = as.character(.run_game_column(
        starting_catchers,
        c("catcher_name", "player_name")
      )),
      start_at_bat_index = suppressWarnings(as.integer(.run_game_column(
        starting_catchers,
        c("start_at_bat_index", "at_bat_index"),
        -1L
      ))),
      assignment_method = "official_starting_defense",
      assignment_confidence = 1,
      stringsAsFactors = FALSE
    )
    starts$start_at_bat_index[is.na(starts$start_at_bat_index)] <- -1L
  } else if (
    is.data.frame(player_reference) &&
      nrow(player_reference) &&
      all(c("player_id", "position") %in% names(player_reference))
  ) {
    is_pitch <- .logical_value(.run_game_column(pbp, c("isPitch", "is_pitch"), FALSE))
    batter_id <- .run_game_id(.run_game_column(
      pbp,
      c("matchup.batter.id", "batter_id")
    ))
    batter_name <- as.character(.run_game_column(
      pbp,
      c("matchup.batter.fullName", "batter_name")
    ))
    batting_order <- suppressWarnings(as.integer(.run_game_column(
      pbp,
      c("battingOrder", "batting_order")
    )))
    appearances <- unique(data.frame(
      game_pk = game_pk[is_pitch],
      fielding_team = batting_team[is_pitch],
      catcher_id = batter_id[is_pitch],
      catcher_name = batter_name[is_pitch],
      batting_order = batting_order[is_pitch],
      stringsAsFactors = FALSE
    ))
    reference_position <- as.character(player_reference$position)
    catcher_ids <- .run_game_id(player_reference$player_id[
      grepl("(^|[/, ])C($|[/, ])|catcher", reference_position, ignore.case = TRUE)
    ])
    appearances <- appearances[
      !is.na(appearances$catcher_id) &
        appearances$catcher_id %in% catcher_ids,
      ,
      drop = FALSE
    ]
    if (nrow(appearances)) {
      appearances$batting_order[!is.finite(appearances$batting_order)] <- 999L
      appearances <- appearances[
        order(appearances$game_pk, appearances$fielding_team, appearances$batting_order),
        ,
        drop = FALSE
      ]
      appearances <- appearances[
        !duplicated(appearances[c("game_pk", "fielding_team")]),
        ,
        drop = FALSE
      ]
      starts <- data.frame(
        game_pk = appearances$game_pk,
        fielding_team = appearances$fielding_team,
        catcher_id = appearances$catcher_id,
        catcher_name = appearances$catcher_name,
        start_at_bat_index = -1L,
        assignment_method = "inferred_game_catcher_candidate",
        assignment_confidence = 0.65,
        stringsAsFactors = FALSE
      )
    }
  }

  position_name <- as.character(.run_game_column(
    pbp,
    c("position.name", "position_name")
  ))
  event_type <- .normalize_event_key(.run_game_column(
    pbp,
    c("details.eventType", "event_type")
  ))
  substitution <- grepl("catcher", position_name, ignore.case = TRUE) &
    event_type %in% c("defensive_substitution", "defensive_switch")
  replacement_count <- sum(substitution)
  replacements <- data.frame(
    game_pk = game_pk[substitution],
    fielding_team = fielding_team[substitution],
    catcher_id = .run_game_id(.run_game_column(
      pbp,
      c("player.id", "player_id")
    ))[substitution],
    catcher_name = as.character(.run_game_column(
      pbp,
      c("player.fullName", "player_name", "details.description")
    ))[substitution],
    start_at_bat_index = at_bat_index[substitution],
    assignment_method = rep("official_defensive_substitution", replacement_count),
    assignment_confidence = rep(1, replacement_count),
    stringsAsFactors = FALSE
  )
  replacement_description <- as.character(.run_game_column(
    pbp,
    c("details.description", "description")
  ))[substitution]
  parsed_name <- ifelse(
    grepl("^Defensive Substitution:", replacement_description, ignore.case = TRUE),
    sub(
      "^Defensive Substitution: ([^,]+?) replaces.*",
      "\\1",
      replacement_description,
      ignore.case = TRUE
    ),
    sub(
      "^Defensive switch from .* for ([^,.]+).*",
      "\\1",
      replacement_description,
      ignore.case = TRUE
    )
  )
  missing_name <- is.na(replacements$catcher_name) |
    !nzchar(replacements$catcher_name) |
    replacements$catcher_name == replacement_description
  replacements$catcher_name[missing_name] <- parsed_name[missing_name]
  replacements <- replacements[
    !is.na(replacements$game_pk) &
      nzchar(replacements$fielding_team) &
      !is.na(replacements$catcher_id),
    ,
    drop = FALSE
  ]

  entries <- dplyr::bind_rows(starts, replacements)
  entries <- entries[
    !is.na(entries$game_pk) &
      nzchar(entries$fielding_team) &
      !is.na(entries$catcher_id),
    ,
    drop = FALSE
  ]
  if (!nrow(entries)) {
    return(tibble::tibble(
      game_pk = character(),
      fielding_team = character(),
      catcher_id = character(),
      catcher_name = character(),
      start_at_bat_index = integer(),
      end_at_bat_index = numeric(),
      assignment_method = character(),
      assignment_confidence = numeric()
    ))
  }

  entries <- entries[
    order(
      entries$game_pk,
      entries$fielding_team,
      entries$start_at_bat_index,
      -entries$assignment_confidence
    ),
    ,
    drop = FALSE
  ]
  entries <- entries[
    !duplicated(entries[c("game_pk", "fielding_team", "start_at_bat_index")]),
    ,
    drop = FALSE
  ]
  stint_key <- paste(entries$game_pk, entries$fielding_team, sep = "\034")
  entries$end_at_bat_index <- Inf
  for (indices in split(seq_len(nrow(entries)), stint_key)) {
    if (length(indices) > 1L) {
      entries$end_at_bat_index[indices[-length(indices)]] <-
        entries$start_at_bat_index[indices[-1L]]
    }
  }
  tibble::as_tibble(entries)
}

#' Attach active catchers to pitch or opportunity rows
#'
#' @param data Data frame containing game, fielding-team, and at-bat fields.
#' @param catcher_stints Output from [build_catcher_stints()].
#'
#' @return `data` with catcher identity and assignment fields.
#' @export
assign_active_catcher <- function(data, catcher_stints) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  output <- data
  output$catcher_id <- NA_character_
  output$catcher_name <- NA_character_
  output$catcher_assignment_method <- NA_character_
  output$catcher_assignment_confidence <- NA_real_
  if (!nrow(output) || !is.data.frame(catcher_stints) || !nrow(catcher_stints)) {
    return(output)
  }
  required <- c("game_pk", "fielding_team", "at_bat_index")
  if (length(setdiff(required, names(output)))) {
    stop("`data` must contain game_pk, fielding_team, and at_bat_index.", call. = FALSE)
  }

  row_key <- paste(output$game_pk, output$fielding_team, sep = "\034")
  stint_key <- paste(catcher_stints$game_pk, catcher_stints$fielding_team, sep = "\034")
  row_groups <- split(seq_len(nrow(output)), row_key)
  stint_groups <- split(seq_len(nrow(catcher_stints)), stint_key)
  for (key in intersect(names(row_groups), names(stint_groups))) {
    rows <- row_groups[[key]]
    stints <- stint_groups[[key]]
    stints <- stints[order(catcher_stints$start_at_bat_index[stints])]
    starts <- catcher_stints$start_at_bat_index[stints]
    interval <- findInterval(output$at_bat_index[rows], starts)
    valid <- interval > 0L
    selected <- stints[pmax(interval, 1L)]
    valid <- valid &
      output$at_bat_index[rows] < catcher_stints$end_at_bat_index[selected]
    if (!any(valid)) next
    target <- rows[valid]
    selected <- selected[valid]
    output$catcher_id[target] <- as.character(catcher_stints$catcher_id[selected])
    output$catcher_name[target] <- as.character(catcher_stints$catcher_name[selected])
    output$catcher_assignment_method[target] <-
      as.character(catcher_stints$assignment_method[selected])
    output$catcher_assignment_confidence[target] <-
      as.numeric(catcher_stints$assignment_confidence[selected])
  }
  output
}

.run_game_runner_before <- function(pbp, pitches) {
  order_index <- order(
    pitches$game_date,
    pitches$game_pk,
    pitches$at_bat_index,
    pitches$pitch_number,
    pitches$source_row,
    na.last = TRUE
  )
  ordered <- pitches[order_index, , drop = FALSE]
  pa_key <- paste(ordered$game_pk, ordered$at_bat_index, sep = "\034")
  terminal <- ordered[cumsum(rle(pa_key)$lengths), , drop = FALSE]
  terminal_raw <- pbp[terminal$source_row, , drop = FALSE]
  terminal$runner_first_after_id <- .run_game_id(.run_game_column(
    terminal_raw,
    c("matchup.postOnFirst.id", "runner_on_first_id")
  ))
  terminal$runner_second_after_id <- .run_game_id(.run_game_column(
    terminal_raw,
    c("matchup.postOnSecond.id", "runner_on_second_id")
  ))
  terminal$runner_third_after_id <- .run_game_id(.run_game_column(
    terminal_raw,
    c("matchup.postOnThird.id", "runner_on_third_id")
  ))
  half_key <- paste(
    terminal$game_pk,
    terminal$inning,
    terminal$half_inning,
    sep = "\034"
  )
  first_in_half <- !duplicated(half_key)
  lag_identity <- function(x) {
    prior <- c(NA_character_, x[-length(x)])
    prior[first_in_half] <- NA_character_
    prior
  }
  terminal$runner_first_before_id <- lag_identity(terminal$runner_first_after_id)
  terminal$runner_second_before_id <- lag_identity(terminal$runner_second_after_id)
  terminal$runner_third_before_id <- lag_identity(terminal$runner_third_after_id)
  terminal[, c(
    "game_pk", "at_bat_index", "runner_first_before_id",
    "runner_second_before_id", "runner_third_before_id"
  )]
}

.run_game_attempt_actions <- function(pbp, pitches) {
  source_row <- seq_len(nrow(pbp))
  event_type <- .normalize_event_key(.run_game_column(
    pbp,
    c("details.eventType", "details.event", "event_type")
  ))
  target_base <- dplyr::case_when(
    event_type %in% c("stolen_base_2b", "caught_stealing_2b", "pickoff_caught_stealing_2b") ~ 2L,
    event_type %in% c("stolen_base_3b", "caught_stealing_3b", "pickoff_caught_stealing_3b") ~ 3L,
    TRUE ~ NA_integer_
  )
  keep <- is.finite(target_base)
  if (!any(keep)) return(data.frame())
  actions <- data.frame(
    source_row = source_row[keep],
    game_pk = .run_game_id(.run_game_column(pbp, c("game_pk", "gamePk")))[keep],
    at_bat_index = suppressWarnings(as.integer(.run_game_column(
      pbp,
      c("about.atBatIndex", "atBatIndex")
    )))[keep],
    runner_id = .run_game_id(.run_game_column(
      pbp,
      c("player.id", "runner_id")
    ))[keep],
    target_base = target_base[keep],
    success = grepl("^stolen_base", event_type[keep]),
    outcome_type = event_type[keep],
    stringsAsFactors = FALSE
  )
  pitch_key <- paste(pitches$game_pk, pitches$at_bat_index, sep = "\034")
  action_key <- paste(actions$game_pk, actions$at_bat_index, sep = "\034")
  pitch_groups <- split(seq_len(nrow(pitches)), pitch_key)
  actions$associated_pitch_row <- NA_integer_
  for (key in intersect(unique(action_key), names(pitch_groups))) {
    pitch_rows <- pitch_groups[[key]]
    action_rows <- which(action_key == key)
    ordered_pitch_rows <- pitch_rows[order(pitches$source_row[pitch_rows])]
    locations <- findInterval(
      actions$source_row[action_rows] - 1L,
      pitches$source_row[ordered_pitch_rows]
    )
    valid <- locations > 0L
    actions$associated_pitch_row[action_rows[valid]] <-
      ordered_pitch_rows[locations[valid]]
  }
  actions[is.finite(actions$associated_pitch_row), , drop = FALSE]
}

#' Build pitch-level stolen-base opportunity windows
#'
#' Each output row represents information available before an eligible pitch.
#' Attempts are attached to the pitch immediately preceding the recorded
#' stolen-base or caught-stealing action.
#'
#' @param pbp Raw MLB play-by-play.
#' @param catcher_stints Optional active-catcher intervals.
#' @param starting_catchers Optional starting defense used when stints are not
#'   supplied.
#' @param player_reference Optional player-position reference for catcher
#'   inference.
#'
#' @return One row per eligible pitch opportunity for second or third base.
#' @export
build_run_game_pitch_opportunities <- function(
  pbp,
  catcher_stints = NULL,
  starting_catchers = NULL,
  player_reference = NULL
) {
  if (!is.data.frame(pbp) || !nrow(pbp)) {
    stop("`pbp` must be a non-empty raw play-by-play data frame.", call. = FALSE)
  }
  pitches <- build_pitch_view(pbp)
  runner_state <- .run_game_runner_before(pbp, pitches)
  state_key <- paste(runner_state$game_pk, runner_state$at_bat_index, sep = "\034")
  pitch_key <- paste(pitches$game_pk, pitches$at_bat_index, sep = "\034")
  state_match <- match(pitch_key, state_key)
  pitches$runner_first_id <- runner_state$runner_first_before_id[state_match]
  pitches$runner_second_id <- runner_state$runner_second_before_id[state_match]
  pitches$runner_third_id <- runner_state$runner_third_before_id[state_match]

  batter_lookup <- unique(data.frame(
    runner_id = .run_game_id(.run_game_column(
      pbp,
      c("matchup.batter.id", "batter_id")
    )),
    runner_name = as.character(.run_game_column(
      pbp,
      c("matchup.batter.fullName", "batter_name")
    )),
    stringsAsFactors = FALSE
  ))
  batter_lookup <- batter_lookup[
    !is.na(batter_lookup$runner_id) & !duplicated(batter_lookup$runner_id),
    ,
    drop = FALSE
  ]
  attempts <- .run_game_attempt_actions(pbp, pitches)
  attempt_pitch <- if (nrow(attempts)) attempts$associated_pitch_row else integer()

  make_target <- function(target_base) {
    if (target_base == 2L) {
      eligible <- !is.na(pitches$runner_first_id) &
        is.na(pitches$runner_second_id) &
        is.na(pitches$runner_third_id)
      runner_id <- pitches$runner_first_id
      opportunity_type <- "steal_second"
    } else {
      eligible <- !is.na(pitches$runner_second_id) &
        is.na(pitches$runner_third_id)
      runner_id <- pitches$runner_second_id
      opportunity_type <- "steal_third"
    }
    rows <- which(eligible)
    if (!length(rows)) return(tibble::tibble())

    target_attempts <- attempts[attempts$target_base == target_base, , drop = FALSE]
    if (nrow(target_attempts)) {
      target_key <- paste(
        target_attempts$game_pk,
        target_attempts$at_bat_index,
        sep = "\034"
      )
      attempt_limit <- stats::setNames(
        pitches$pitch_number[target_attempts$associated_pitch_row],
        target_key
      )
      current_key <- pitch_key[rows]
      limit <- unname(attempt_limit[current_key])
      rows <- rows[is.na(limit) | pitches$pitch_number[rows] <= limit]
    }
    target_attempt_pitch <- if (nrow(target_attempts)) {
      target_attempts$associated_pitch_row
    } else {
      integer()
    }
    attempted_index <- match(rows, target_attempt_pitch)
    attempted <- !is.na(attempted_index)
    success <- rep(NA, length(rows))
    outcome_type <- rep("hold", length(rows))
    if (any(attempted)) {
      success[attempted] <- target_attempts$success[attempted_index[attempted]]
      outcome_type[attempted] <-
        target_attempts$outcome_type[attempted_index[attempted]]
    }
    runner <- runner_id[rows]
    runner_name <- batter_lookup$runner_name[match(runner, batter_lookup$runner_id)]
    score_margin <- ifelse(
      pitches$is_top_inning[rows],
      pitches$away_score_current[rows] - pitches$home_score_current[rows],
      pitches$home_score_current[rows] - pitches$away_score_current[rows]
    )
    tibble::tibble(
      game_pk = pitches$game_pk[rows],
      game_date = pitches$game_date[rows],
      at_bat_index = pitches$at_bat_index[rows],
      pitch_number = pitches$pitch_number[rows],
      play_id = pitches$play_id[rows],
      inning = pitches$inning[rows],
      half_inning = pitches$half_inning[rows],
      batting_team = pitches$batting_team[rows],
      fielding_team = pitches$fielding_team[rows],
      pitcher_id = pitches$pitcher_id[rows],
      pitcher_name = pitches$pitcher_name[rows],
      pitcher_hand = pitches$pitcher_hand[rows],
      catcher_id = NA_character_,
      catcher_name = NA_character_,
      runner_id = runner,
      runner_name = runner_name,
      batter_id = pitches$batter_id[rows],
      batter_name = pitches$batter_name[rows],
      batter_side = pitches$batter_side[rows],
      target_base = target_base,
      opportunity_type = opportunity_type,
      balls = pitches$balls_before[rows],
      strikes = pitches$strikes_before[rows],
      count_key = pitches$count_key[rows],
      outs = pitches$outs_before[rows],
      score_margin = score_margin,
      disengagement_count = pmax(pitches$disengagement_count[rows], 0L, na.rm = TRUE),
      pitch_type = pitches$pitch_type[rows],
      pitch_name = pitches$pitch_name[rows],
      pitch_family = .run_game_pitch_family(
        pitches$pitch_type[rows],
        pitches$pitch_name[rows]
      ),
      plate_time = pitches$plate_time[rows],
      start_speed = pitches$start_speed[rows],
      plate_x = pitches$plate_x[rows],
      plate_z = pitches$plate_z[rows],
      pitchout = pitches$call_description[rows] %in% c("Pitchout", "Swinging Pitchout"),
      runner_going = pitches$runner_going[rows] | attempted,
      attempted = attempted,
      success = as.logical(success),
      outcome_type = outcome_type
    )
  }

  output <- dplyr::bind_rows(make_target(2L), make_target(3L))
  if (!nrow(output)) return(output)
  if (is.null(catcher_stints)) {
    catcher_stints <- build_catcher_stints(
      pbp,
      starting_catchers = starting_catchers,
      player_reference = player_reference
    )
  }
  assigned <- assign_active_catcher(
    output[, setdiff(
      names(output),
      c("catcher_id", "catcher_name")
    ), drop = FALSE],
    catcher_stints
  )
  assigned$opportunity_id <- paste(
    assigned$game_pk,
    assigned$at_bat_index,
    assigned$pitch_number,
    assigned$target_base,
    sep = "-"
  )
  assigned |>
    dplyr::arrange(
      .data$game_date,
      .data$game_pk,
      .data$at_bat_index,
      .data$pitch_number,
      .data$target_base
    )
}

.fit_shrunk_logit_effects <- function(data, outcome, specifications, iterations = 6L) {
  y <- as.numeric(data[[outcome]] %in% TRUE)
  base_rate <- .run_game_probability(mean(y))
  base_logit <- .run_game_logit(base_rate)
  effects <- lapply(specifications, function(specification) {
    levels <- unique(as.character(data[[specification$column]]))
    levels <- levels[!is.na(levels) & nzchar(levels)]
    stats::setNames(rep(0, length(levels)), levels)
  })
  names(effects) <- names(specifications)

  row_effect <- function(effect, values) {
    output <- unname(effect[as.character(values)])
    output[!is.finite(output)] <- 0
    output
  }
  eta <- rep(base_logit, nrow(data))
  for (iteration in seq_len(iterations)) {
    for (name in names(specifications)) {
      specification <- specifications[[name]]
      values <- as.character(data[[specification$column]])
      current <- row_effect(effects[[name]], values)
      eta_without <- eta - current
      probability_without <- stats::plogis(eta_without)
      valid <- !is.na(values) & nzchar(values)
      groups <- split(which(valid), values[valid])
      updated <- effects[[name]]
      for (level in names(groups)) {
        rows <- groups[[level]]
        expected_rate <- mean(probability_without[rows])
        observed_shrunk <- (
          sum(y[rows]) + specification$prior * expected_rate
        ) / (length(rows) + specification$prior)
        updated[[level]] <- max(
          -1.75,
          min(
            1.75,
            .run_game_logit(observed_shrunk) -
              .run_game_logit(expected_rate)
          )
        )
      }
      effects[[name]] <- updated
      eta <- eta_without + row_effect(updated, values)
    }
  }
  probability <- stats::plogis(eta)
  without <- lapply(names(specifications), function(name) {
    stats::plogis(
      eta - row_effect(
        effects[[name]],
        data[[specifications[[name]]$column]]
      )
    )
  })
  names(without) <- names(specifications)
  list(
    base_rate = base_rate,
    probability = probability,
    probability_without = without,
    effects = effects,
    specifications = specifications
  )
}

.run_game_effect_board <- function(
  data,
  outcome,
  model,
  effect_name,
  id_column,
  name_column,
  team_column
) {
  id <- as.character(data[[id_column]])
  valid <- !is.na(id) & nzchar(id)
  groups <- split(which(valid), id[valid])
  if (!length(groups)) return(tibble::tibble())
  y <- as.numeric(data[[outcome]] %in% TRUE)
  effect <- model$effects[[effect_name]]
  rows <- lapply(names(groups), function(level) {
    indices <- groups[[level]]
    without <- model$probability_without[[effect_name]][indices]
    with <- model$probability[indices]
    data.frame(
      player_id = level,
      player_name = .run_game_first(as.character(data[[name_column]][indices])),
      team = .run_game_first(as.character(data[[team_column]][indices])),
      opportunities = length(indices),
      outcomes = sum(y[indices]),
      observed_rate = mean(y[indices]),
      expected_rate_without_player = mean(without),
      adjusted_rate = mean(with),
      outcomes_above_expected = sum(y[indices] - without),
      logit_effect = unname(effect[level]),
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

#' Build adjusted pitcher, catcher, runner, and battery run-game ratings
#'
#' Fits league-shrunk additive logit effects separately for the decision to run
#' and for success after an attempt. Pitcher and catcher effects are estimated
#' together, allowing pitcher ratings to be catcher-adjusted and catcher
#' ratings to be pitcher-adjusted.
#'
#' @param opportunities Output from
#'   [build_run_game_pitch_opportunities()].
#'
#' @return A named list of public rating tables and a model card.
#' @export
build_run_game_ratings <- function(opportunities) {
  required <- c(
    "attempted", "success", "runner_id", "pitcher_id", "count_key",
    "disengagement_count", "pitch_family", "target_base"
  )
  missing <- setdiff(required, names(opportunities))
  if (length(missing)) {
    stop("Run-game opportunities are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- as.data.frame(opportunities)
  data$runner_group <- ifelse(is.na(data$runner_id), "", as.character(data$runner_id))
  data$pitcher_group <- ifelse(is.na(data$pitcher_id), "", as.character(data$pitcher_id))
  data$catcher_group <- ifelse(is.na(data$catcher_id), "", as.character(data$catcher_id))
  data$count_group <- paste(data$target_base, data$count_key, sep = ":")
  data$disengagement_group <- paste(
    data$target_base,
    pmin(as.numeric(data$disengagement_count), 2),
    sep = ":"
  )
  data$pitch_context_group <- paste(
    data$target_base,
    data$count_key,
    data$pitch_family,
    sep = ":"
  )

  attempt_model <- .fit_shrunk_logit_effects(
    data,
    "attempted",
    list(
      runner = list(column = "runner_group", prior = 35),
      pitcher = list(column = "pitcher_group", prior = 90),
      catcher = list(column = "catcher_group", prior = 120),
      count = list(column = "count_group", prior = 160),
      disengagement = list(column = "disengagement_group", prior = 160)
    )
  )
  attempt_data <- data[data$attempted %in% TRUE & !is.na(data$success), , drop = FALSE]
  if (!nrow(attempt_data)) {
    stop("No resolved stolen-base attempts were available.", call. = FALSE)
  }
  success_model <- .fit_shrunk_logit_effects(
    attempt_data,
    "success",
    list(
      runner = list(column = "runner_group", prior = 18),
      pitcher = list(column = "pitcher_group", prior = 28),
      catcher = list(column = "catcher_group", prior = 28),
      pitch_context = list(column = "pitch_context_group", prior = 55)
    )
  )

  attempt_runner <- .run_game_effect_board(
    data, "attempted", attempt_model, "runner",
    "runner_id", "runner_name", "batting_team"
  )
  success_runner <- .run_game_effect_board(
    attempt_data, "success", success_model, "runner",
    "runner_id", "runner_name", "batting_team"
  )
  runner <- dplyr::full_join(
    dplyr::rename(
      attempt_runner,
      eligible_pitches = .data$opportunities,
      attempts = .data$outcomes,
      attempt_rate = .data$observed_rate,
      expected_attempt_rate = .data$expected_rate_without_player,
      adjusted_attempt_rate = .data$adjusted_rate,
      attempts_above_expected = .data$outcomes_above_expected,
      attempt_logit_effect = .data$logit_effect
    ),
    dplyr::rename(
      success_runner,
      attempts_modeled = .data$opportunities,
      successes = .data$outcomes,
      success_rate = .data$observed_rate,
      expected_success_rate = .data$expected_rate_without_player,
      adjusted_success_rate = .data$adjusted_rate,
      successes_above_expected = .data$outcomes_above_expected,
      success_logit_effect = .data$logit_effect
    ),
    by = c("player_id", "player_name", "team")
  )
  runner$stealing_runs <- 0.65 * runner$successes_above_expected
  runner$reliability <- runner$eligible_pitches / (runner$eligible_pitches + 35)

  make_prevention_board <- function(role) {
    id_column <- paste0(role, "_id")
    name_column <- paste0(role, "_name")
    team_column <- "fielding_team"
    attempt_board <- .run_game_effect_board(
      data, "attempted", attempt_model, role,
      id_column, name_column, team_column
    )
    success_board <- .run_game_effect_board(
      attempt_data, "success", success_model, role,
      id_column, name_column, team_column
    )
    board <- dplyr::full_join(
      dplyr::rename(
        attempt_board,
        eligible_pitches = .data$opportunities,
        attempts = .data$outcomes,
        attempt_rate = .data$observed_rate,
        expected_attempt_rate = .data$expected_rate_without_player,
        adjusted_attempt_rate = .data$adjusted_rate,
        attempts_above_expected = .data$outcomes_above_expected,
        attempt_logit_effect = .data$logit_effect
      ),
      dplyr::rename(
        success_board,
        attempts_modeled = .data$opportunities,
        stolen_bases = .data$outcomes,
        success_rate_allowed = .data$observed_rate,
        expected_success_rate_allowed = .data$expected_rate_without_player,
        adjusted_success_rate_allowed = .data$adjusted_rate,
        advances_above_expected = .data$outcomes_above_expected,
        success_logit_effect = .data$logit_effect
      ),
      by = c("player_id", "player_name", "team")
    )
    board$attempt_index <- 100 * board$adjusted_attempt_rate /
      attempt_model$base_rate
    board$success_allowed_index <- 100 * board$adjusted_success_rate_allowed /
      success_model$base_rate
    board$attempts_prevented <- -board$attempts_above_expected
    board$base_advances_prevented <- -board$advances_above_expected
    board$stealing_runs_saved <- 0.65 * board$base_advances_prevented
    prior <- if (role == "pitcher") 90 else 120
    board$reliability <- board$eligible_pitches / (board$eligible_pitches + prior)
    board
  }
  pitcher <- make_prevention_board("pitcher")
  catcher <- make_prevention_board("catcher")

  covered_attempts <- attempt_data[
    !is.na(attempt_data$catcher_id) & nzchar(as.character(attempt_data$catcher_id)),
    ,
    drop = FALSE
  ]
  if (nrow(covered_attempts)) {
    pair_key <- paste(
      covered_attempts$pitcher_id,
      covered_attempts$catcher_id,
      sep = "\034"
    )
    pair_groups <- split(seq_len(nrow(covered_attempts)), pair_key)
    battery <- lapply(pair_groups, function(indices) {
      expected <- success_model$probability_without$pitcher[indices]
      expected <- stats::plogis(
        .run_game_logit(expected) -
          unname(success_model$effects$catcher[
            covered_attempts$catcher_group[indices]
          ])
      )
      data.frame(
        pitcher_id = .run_game_first(covered_attempts$pitcher_id[indices]),
        pitcher_name = .run_game_first(covered_attempts$pitcher_name[indices]),
        catcher_id = .run_game_first(covered_attempts$catcher_id[indices]),
        catcher_name = .run_game_first(covered_attempts$catcher_name[indices]),
        fielding_team = .run_game_first(covered_attempts$fielding_team[indices]),
        attempts = length(indices),
        stolen_bases = sum(covered_attempts$success[indices] %in% TRUE),
        success_rate_allowed = mean(covered_attempts$success[indices] %in% TRUE),
        independent_expected_success_rate = mean(expected),
        battery_advances_prevented = sum(
          expected - as.numeric(covered_attempts$success[indices] %in% TRUE)
        ),
        stringsAsFactors = FALSE
      )
    })
    battery <- tibble::as_tibble(do.call(rbind, battery))
    battery$battery_runs_saved <- 0.65 * battery$battery_advances_prevented
    battery$reliability <- battery$attempts / (battery$attempts + 20)
  } else {
    battery <- tibble::tibble()
  }

  model_card <- tibble::tibble(
    model_version = "run_game_shrunk_logit_v1",
    opportunity_definition = "one row per eligible pitch before an attempt",
    eligible_pitches = nrow(data),
    attempts = nrow(attempt_data),
    successes = sum(attempt_data$success %in% TRUE),
    pitchers = dplyr::n_distinct(data$pitcher_id),
    catchers = dplyr::n_distinct(data$catcher_id[!is.na(data$catcher_id)]),
    catcher_coverage = mean(!is.na(data$catcher_id)),
    league_attempt_rate = attempt_model$base_rate,
    league_success_rate = success_model$base_rate,
    publication_status = "development; chronological validation required"
  )
  list(
    runner = runner,
    pitcher = pitcher,
    catcher = catcher,
    battery = battery,
    model_card = model_card
  )
}

#' Summarize pitcher count windows for the running game
#'
#' @param opportunities Pitch-level run-game opportunities.
#' @param prior_pitches League-prior pitch count for rate shrinkage.
#'
#' @return Pitcher-by-count running-window table.
#' @export
build_run_game_count_windows <- function(opportunities, prior_pitches = 30) {
  if (!is.data.frame(opportunities) || !nrow(opportunities)) {
    return(tibble::tibble())
  }
  league_offspeed <- mean(opportunities$pitch_family == "offspeed", na.rm = TRUE)
  league_breaking <- mean(opportunities$pitch_family == "breaking", na.rm = TRUE)
  league_attempt <- mean(opportunities$attempted %in% TRUE, na.rm = TRUE)
  league_plate_time <- mean(opportunities$plate_time, na.rm = TRUE)
  output <- opportunities |>
    dplyr::group_by(
      .data$pitcher_id,
      .data$pitcher_name,
      .data$fielding_team,
      .data$target_base,
      .data$count_key
    ) |>
    dplyr::summarise(
      eligible_pitches = dplyr::n(),
      attempts = sum(.data$attempted %in% TRUE),
      successes = sum(.data$success %in% TRUE, na.rm = TRUE),
      offspeed_pitches = sum(.data$pitch_family == "offspeed", na.rm = TRUE),
      breaking_pitches = sum(.data$pitch_family == "breaking", na.rm = TRUE),
      fastballs = sum(.data$pitch_family == "fastball", na.rm = TRUE),
      pitchouts = sum(.data$pitchout %in% TRUE),
      average_plate_time = mean(.data$plate_time, na.rm = TRUE),
      average_disengagements = mean(.data$disengagement_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      observed_offspeed_rate = .data$offspeed_pitches / .data$eligible_pitches,
      projected_offspeed_rate = (
        .data$offspeed_pitches + prior_pitches * league_offspeed
      ) / (.data$eligible_pitches + prior_pitches),
      projected_breaking_rate = (
        .data$breaking_pitches + prior_pitches * league_breaking
      ) / (.data$eligible_pitches + prior_pitches),
      observed_attempt_rate = .data$attempts / .data$eligible_pitches,
      projected_attempt_rate = (
        .data$attempts + prior_pitches * league_attempt
      ) / (.data$eligible_pitches + prior_pitches),
      observed_success_rate = ifelse(
        .data$attempts > 0,
        .data$successes / .data$attempts,
        NA_real_
      ),
      flight_time_index = ifelse(
        is.finite(.data$average_plate_time) & is.finite(league_plate_time),
        100 * .data$average_plate_time / league_plate_time,
        NA_real_
      ),
      run_window_index =
        50 * .data$projected_offspeed_rate / max(league_offspeed, 0.001) +
        30 * .data$projected_breaking_rate / max(league_breaking, 0.001) +
        20 * ifelse(is.finite(.data$flight_time_index), .data$flight_time_index / 100, 1),
      reliability = .data$eligible_pitches /
        (.data$eligible_pitches + prior_pitches),
      model_version = "run_game_count_window_v1"
    )
  output
}

#' Generate concise pitcher run-window notes
#'
#' @param count_windows Output from [build_run_game_count_windows()].
#' @param minimum_pitches Minimum count-specific sample before a note is used.
#'
#' @return One note per pitcher and target base.
#' @export
build_run_game_notes <- function(count_windows, minimum_pitches = 15L) {
  if (!is.data.frame(count_windows) || !nrow(count_windows)) {
    return(tibble::tibble())
  }
  eligible <- count_windows[
    count_windows$eligible_pitches >= minimum_pitches,
    ,
    drop = FALSE
  ]
  if (!nrow(eligible)) return(tibble::tibble())
  eligible <- eligible[
    order(
      eligible$pitcher_id,
      eligible$target_base,
      -eligible$run_window_index,
      -eligible$eligible_pitches
    ),
    ,
    drop = FALSE
  ]
  best <- eligible[
    !duplicated(eligible[c("pitcher_id", "target_base")]),
    ,
    drop = FALSE
  ]
  best$note_type <- "best_count_to_run"
  best$note <- paste0(
    "Best projected count window: ",
    best$count_key,
    ". ",
    round(100 * best$projected_offspeed_rate),
    "% offspeed and ",
    round(100 * best$projected_breaking_rate),
    "% breaking balls across ",
    best$eligible_pitches,
    " eligible pitches; reliability ",
    round(100 * best$reliability),
    "%."
  )
  tibble::as_tibble(best[, c(
    "pitcher_id", "pitcher_name", "fielding_team", "target_base",
    "note_type", "note", "count_key", "eligible_pitches",
    "run_window_index", "reliability", "model_version"
  )])
}
