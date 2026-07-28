#' Extract the primary fielding credit from official play descriptions
#'
#' @param description MLB play descriptions.
#'
#' @return A tibble containing the credited fielder name and position.
#' @export
extract_fielding_credit <- function(description) {
  description <- as.character(description)
  position_pattern <- paste(
    c(
      "pitcher", "catcher", "first baseman", "second baseman",
      "third baseman", "shortstop", "left fielder", "center fielder",
      "right fielder"
    ),
    collapse = "|"
  )
  credit_pattern <- paste0(
    "(", position_pattern, ")\\s+",
    "([^,.]+?)(?=\\s+to\\s+(?:", position_pattern, ")|,|\\.|$)"
  )
  position_map <- c(
    pitcher = "P",
    catcher = "C",
    `first baseman` = "1B",
    `second baseman` = "2B",
    `third baseman` = "3B",
    shortstop = "SS",
    `left fielder` = "LF",
    `center fielder` = "CF",
    `right fielder` = "RF"
  )
  rows <- lapply(description, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(data.frame(
        fielder_name = NA_character_,
        position = NA_character_,
        position_name = NA_character_,
        credit_confidence = 0,
        stringsAsFactors = FALSE
      ))
    }
    match <- regexec(credit_pattern, value, perl = TRUE, ignore.case = TRUE)
    parts <- regmatches(value, match)[[1L]]
    if (length(parts) < 3L) {
      return(data.frame(
        fielder_name = NA_character_,
        position = NA_character_,
        position_name = NA_character_,
        credit_confidence = 0,
        stringsAsFactors = FALSE
      ))
    }
    position_name <- tolower(trimws(parts[[2L]]))
    fielder_name <- trimws(parts[[3L]])
    fielder_name <- sub("\\s+(?:and|who)\\s.*$", "", fielder_name, ignore.case = TRUE)
    data.frame(
      fielder_name = fielder_name,
      position = unname(position_map[position_name]),
      position_name = position_name,
      credit_confidence = 0.9,
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

.fielding_normalize_name <- function(value) {
  value <- iconv(as.character(value), from = "", to = "ASCII//TRANSLIT")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", " ", value)
  trimws(gsub("\\s+", " ", value))
}

.fielding_team_names <- c(
  `108` = "Los Angeles Angels",
  `109` = "Arizona Diamondbacks",
  `110` = "Baltimore Orioles",
  `111` = "Boston Red Sox",
  `112` = "Chicago Cubs",
  `113` = "Cincinnati Reds",
  `114` = "Cleveland Guardians",
  `115` = "Colorado Rockies",
  `116` = "Detroit Tigers",
  `117` = "Houston Astros",
  `118` = "Kansas City Royals",
  `119` = "Los Angeles Dodgers",
  `120` = "Washington Nationals",
  `121` = "New York Mets",
  `133` = "Athletics",
  `134` = "Pittsburgh Pirates",
  `135` = "San Diego Padres",
  `136` = "Seattle Mariners",
  `137` = "San Francisco Giants",
  `138` = "St. Louis Cardinals",
  `139` = "Tampa Bay Rays",
  `140` = "Texas Rangers",
  `141` = "Toronto Blue Jays",
  `142` = "Minnesota Twins",
  `143` = "Philadelphia Phillies",
  `144` = "Atlanta Braves",
  `145` = "Chicago White Sox",
  `146` = "Miami Marlins",
  `147` = "New York Yankees",
  `158` = "Milwaukee Brewers"
)

.fielding_canonical_team <- function(team_id, fallback) {
  canonical <- unname(.fielding_team_names[as.character(team_id)])
  use_fallback <- is.na(canonical) | !nzchar(canonical)
  canonical[use_fallback] <- as.character(fallback)[use_fallback]
  canonical
}

.fielding_attach_player_id <- function(data, player_reference) {
  data$fielder_id <- NA_character_
  data$fielder_assignment_method <- "description_only"
  if (!is.data.frame(player_reference) || !nrow(player_reference)) return(data)
  id_column <- intersect(c("player_id", "id", "mlbam_id"), names(player_reference))
  name_column <- intersect(c("player_name", "name"), names(player_reference))
  team_column <- intersect(c("team", "team_name"), names(player_reference))
  if (!length(id_column) || !length(name_column)) return(data)
  reference <- data.frame(
    player_id = as.character(player_reference[[id_column[[1L]]]]),
    player_name = as.character(player_reference[[name_column[[1L]]]]),
    team = if (length(team_column)) {
      as.character(player_reference[[team_column[[1L]]]])
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  reference$name_key <- .fielding_normalize_name(reference$player_name)
  reference$team_key <- .fielding_normalize_name(reference$team)
  reference <- reference[
    !is.na(reference$player_id) & nzchar(reference$player_id) &
      nzchar(reference$name_key),
    ,
    drop = FALSE
  ]
  name_key <- .fielding_normalize_name(data$fielder_name)
  team_key <- .fielding_normalize_name(data$fielding_team)
  for (index in seq_len(nrow(data))) {
    candidates <- which(reference$name_key == name_key[[index]])
    if (length(candidates) > 1L && nzchar(team_key[[index]])) {
      team_match <- candidates[reference$team_key[candidates] == team_key[[index]]]
      if (length(team_match)) candidates <- team_match
    }
    if (length(candidates)) {
      data$fielder_id[[index]] <- reference$player_id[candidates[[1L]]]
      data$fielder_assignment_method[[index]] <- "description_roster_match"
    }
  }
  data
}

.fielding_trajectory <- function(data) {
  source <- if ("batted_ball_type" %in% names(data)) {
    as.character(data$batted_ball_type)
  } else {
    as.character(data$trajectory)
  }
  key <- .normalize_event_key(source)
  dplyr::case_when(
    grepl("ground", key) ~ "ground_ball",
    grepl("line", key) ~ "line_drive",
    grepl("popup|pop_up|bunt_popup", key) ~ "popup",
    grepl("fly", key) ~ "fly_ball",
    TRUE ~ "other"
  )
}

#' Build batted-ball fielding opportunities
#'
#' Expected out probability is estimated from observed league conversion rates
#' in batted-ball context cells. It is an auditable SABRhood development model,
#' not official Statcast Catch Probability or Outs Above Average.
#'
#' @param pbp Raw MLB play-by-play or a canonical pitch table.
#' @param player_reference Optional player table used to resolve fielder IDs.
#' @param context_prior Prior opportunities used to shrink detailed context
#'   cells toward broader trajectory-location rates.
#'
#' @return One row per credited ball in play.
#' @export
build_fielding_opportunity_view <- function(
  pbp,
  player_reference = NULL,
  context_prior = 25
) {
  if (!is.data.frame(pbp) || !nrow(pbp)) {
    stop("`pbp` must be a non-empty data frame.", call. = FALSE)
  }
  pitches <- if (all(c("pitch_in_pa", "plate_x", "event_key") %in% names(pbp))) {
    pbp
  } else {
    build_pitch_view(pbp)
  }
  appearances <- build_plate_appearance_view(pitches)
  if ("result_description" %in% names(appearances)) {
    use_result <- !is.na(appearances$result_description) &
      nzchar(appearances$result_description)
    appearances$description[use_result] <-
      appearances$result_description[use_result]
  }
  appearances <- appearances[
    appearances$is_in_play %in% TRUE |
      appearances$event_key %in% c(
        "field_out", "force_out", "grounded_into_double_play",
        "fielders_choice_out", "field_error", "double_play",
        "sac_fly", "sac_bunt"
      ),
    ,
    drop = FALSE
  ]
  if (!nrow(appearances)) return(tibble::tibble())
  credit <- extract_fielding_credit(appearances$description)
  output <- cbind(
    as.data.frame(appearances, stringsAsFactors = FALSE),
    as.data.frame(credit, stringsAsFactors = FALSE)
  )
  output <- output[
    !is.na(output$fielder_name) & nzchar(output$fielder_name),
    ,
    drop = FALSE
  ]
  if (!nrow(output)) return(tibble::tibble())
  output <- .fielding_attach_player_id(output, player_reference)
  output$trajectory_group <- .fielding_trajectory(output)
  output$hit_location_group <- ifelse(
    is.finite(output$hit_location),
    as.character(output$hit_location),
    "unknown"
  )
  output$speed_bin <- ifelse(
    is.finite(output$launch_speed),
    as.character(5 * floor(output$launch_speed / 5)),
    "unknown"
  )
  output$angle_bin <- ifelse(
    is.finite(output$launch_angle),
    as.character(10 * floor(output$launch_angle / 10)),
    "unknown"
  )
  output$distance_bin <- ifelse(
    is.finite(output$hit_distance),
    as.character(25 * floor(output$hit_distance / 25)),
    "unknown"
  )
  output$converted_out <- output$is_out %in% TRUE
  broad_key <- paste(
    output$trajectory_group,
    output$hit_location_group,
    sep = ":"
  )
  context_key <- paste(
    broad_key,
    output$speed_bin,
    output$angle_bin,
    output$distance_bin,
    sep = ":"
  )
  broad_groups <- split(seq_len(nrow(output)), broad_key)
  broad_rate <- vapply(broad_groups, function(rows) {
    mean(output$converted_out[rows])
  }, numeric(1))
  league_rate <- mean(output$converted_out)
  broad_expected <- unname(broad_rate[broad_key])
  broad_expected[!is.finite(broad_expected)] <- league_rate
  context_groups <- split(seq_len(nrow(output)), context_key)
  context_expected <- numeric(nrow(output))
  for (rows in context_groups) {
    prior_rate <- mean(broad_expected[rows])
    context_expected[rows] <- (
      sum(output$converted_out[rows]) + context_prior * prior_rate
    ) / (length(rows) + context_prior)
  }
  output$expected_out_probability <- pmin(pmax(context_expected, 0.02), 0.98)
  output$out_credit <- as.numeric(output$converted_out) -
    output$expected_out_probability
  out_value <- ifelse(output$position %in% c("LF", "CF", "RF"), 0.90, 0.75)
  output$estimated_range_runs <- output$out_credit * out_value
  output$estimated_play_difficulty <- ifelse(
    output$converted_out,
    1 - output$expected_out_probability,
    output$expected_out_probability
  )
  output$opportunity_model <- "sabrhood_batted_ball_context_v1"
  tibble::as_tibble(output)
}

.fielding_position_score <- function(rate, position, reliability) {
  output <- rep(100, length(rate))
  for (level in unique(position)) {
    rows <- which(position == level & is.finite(rate))
    if (!length(rows)) next
    center <- stats::median(rate[rows], na.rm = TRUE)
    spread <- stats::mad(rate[rows], center = center, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(spread) || spread < 0.01) {
      spread <- stats::sd(rate[rows], na.rm = TRUE)
    }
    if (!is.finite(spread) || spread < 0.01) spread <- 1
    output[rows] <- 100 + 15 * ((rate[rows] - center) / spread) *
      sqrt(pmax(reliability[rows], 0))
  }
  pmin(pmax(output, 40), 160)
}

#' Summarize custom player and team fielding ratings
#'
#' @param opportunities Output from [build_fielding_opportunity_view()].
#' @param prior_opportunities Prior used in the reliability calculation.
#'
#' @return A list containing player and team rating tables.
#' @export
build_fielding_ratings <- function(opportunities, prior_opportunities = 80) {
  required <- c(
    "fielder_id", "fielder_name", "fielding_team", "position",
    "converted_out", "expected_out_probability", "estimated_range_runs"
  )
  missing <- setdiff(required, names(opportunities))
  if (length(missing)) {
    stop("Fielding opportunities are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- as.data.frame(opportunities)
  data <- data[
    !is.na(data$fielder_id) & nzchar(as.character(data$fielder_id)),
    ,
    drop = FALSE
  ]
  summarize_board <- function(groups, include_position = TRUE) {
    rows <- lapply(groups, function(indices) {
      result <- data.frame(
        opportunities = length(indices),
        converted_outs = sum(data$converted_out[indices]),
        expected_outs = sum(data$expected_out_probability[indices]),
        outs_above_expected = sum(
          as.numeric(data$converted_out[indices]) -
            data$expected_out_probability[indices]
        ),
        estimated_range_runs = sum(data$estimated_range_runs[indices]),
        routine_opportunities = sum(
          data$expected_out_probability[indices] >= 0.85
        ),
        routine_conversion_rate = mean(
          data$converted_out[indices][
            data$expected_out_probability[indices] >= 0.85
          ],
          na.rm = TRUE
        ),
        difficult_conversions = sum(
          data$converted_out[indices] &
            data$expected_out_probability[indices] <= 0.50
        ),
        stringsAsFactors = FALSE
      )
      if (!is.finite(result$routine_conversion_rate)) {
        result$routine_conversion_rate <- NA_real_
      }
      result
    })
    do.call(rbind, rows)
  }
  player_groups <- split(
    seq_len(nrow(data)),
    paste(data$fielder_id, data$fielding_team, data$position, sep = "\034")
  )
  player <- summarize_board(player_groups)
  player$player_id <- vapply(player_groups, function(rows) {
    as.character(data$fielder_id[rows[[1L]]])
  }, character(1))
  player$player_name <- vapply(player_groups, function(rows) {
    as.character(data$fielder_name[rows[[1L]]])
  }, character(1))
  player$team <- vapply(player_groups, function(rows) {
    as.character(data$fielding_team[rows[[1L]]])
  }, character(1))
  player$position <- vapply(player_groups, function(rows) {
    as.character(data$position[rows[[1L]]])
  }, character(1))
  player$reliability <- player$opportunities /
    (player$opportunities + prior_opportunities)
  player$adjusted_outs_above_expected <- player$outs_above_expected *
    player$reliability
  player$adjusted_range_runs <- player$estimated_range_runs *
    player$reliability
  player$range_runs_per_100 <- 100 * player$adjusted_range_runs /
    pmax(player$opportunities, 1)
  player$fielding_score <- .fielding_position_score(
    player$range_runs_per_100,
    player$position,
    player$reliability
  )
  player$model_version <- "sabrhood_fielding_context_v1"
  player <- player[, c(
    "player_id", "player_name", "team", "position", "opportunities",
    "converted_outs", "expected_outs", "outs_above_expected",
    "adjusted_outs_above_expected", "estimated_range_runs",
    "adjusted_range_runs", "range_runs_per_100", "routine_opportunities",
    "routine_conversion_rate", "difficult_conversions", "fielding_score",
    "reliability", "model_version"
  )]

  team_groups <- split(seq_len(nrow(data)), data$fielding_team)
  team <- summarize_board(team_groups, include_position = FALSE)
  team$team <- names(team_groups)
  team$reliability <- team$opportunities /
    (team$opportunities + 5 * prior_opportunities)
  team$adjusted_range_runs <- team$estimated_range_runs * team$reliability
  team$fielding_score <- .fielding_position_score(
    100 * team$adjusted_range_runs / pmax(team$opportunities, 1),
    rep("TEAM", nrow(team)),
    team$reliability
  )
  team$model_version <- "sabrhood_fielding_context_v1"
  list(
    player = tibble::as_tibble(player),
    team = tibble::as_tibble(team)
  )
}

.fielding_advancement_rows <- function(pbp) {
  plate_appearances <- build_plate_appearance_view(pbp)
  if ("result_description" %in% names(plate_appearances)) {
    use_result <- !is.na(plate_appearances$result_description) &
      nzchar(plate_appearances$result_description)
    plate_appearances$description[use_result] <-
      plate_appearances$result_description[use_result]
  }
  terminal_raw <- pbp[plate_appearances$source_row, , drop = FALSE]
  as_id <- function(value) {
    output <- as.character(value)
    output[is.na(value) | output %in% c("", "NA")] <- NA_character_
    output
  }
  plate_appearances$runner_first_after_id <- as_id(
    .column_or_default(terminal_raw, "matchup.postOnFirst.id")
  )
  plate_appearances$runner_second_after_id <- as_id(
    .column_or_default(terminal_raw, "matchup.postOnSecond.id")
  )
  plate_appearances$runner_third_after_id <- as_id(
    .column_or_default(terminal_raw, "matchup.postOnThird.id")
  )
  half_key <- paste(
    plate_appearances$game_pk,
    plate_appearances$inning,
    plate_appearances$half_inning,
    sep = "\034"
  )
  first_in_half <- !duplicated(half_key)
  lag_half <- function(value) {
    previous <- c(NA_character_, value[-length(value)])
    previous[first_in_half] <- NA_character_
    previous
  }
  plate_appearances$runner_first_before_id <- lag_half(
    plate_appearances$runner_first_after_id
  )
  plate_appearances$runner_second_before_id <- lag_half(
    plate_appearances$runner_second_after_id
  )
  plate_appearances$runner_third_before_id <- lag_half(
    plate_appearances$runner_third_after_id
  )
  batter_lookup <- unique(data.frame(
    runner_id = as_id(.column_or_default(
      pbp,
      c("matchup.batter.id", "batter_id")
    )),
    runner_name = as.character(.column_or_default(
      pbp,
      c("matchup.batter.fullName", "batter_name")
    )),
    stringsAsFactors = FALSE
  ))
  batter_lookup <- batter_lookup[
    !is.na(batter_lookup$runner_id) &
      !duplicated(batter_lookup$runner_id),
    ,
    drop = FALSE
  ]
  runner_name <- function(id) {
    batter_lookup$runner_name[match(as.character(id), batter_lookup$runner_id)]
  }
  post_contains <- function(runner_id, bases = 1:3) {
    post <- cbind(
      plate_appearances$runner_first_after_id,
      plate_appearances$runner_second_after_id,
      plate_appearances$runner_third_after_id
    )
    vapply(seq_len(nrow(post)), function(index) {
      values <- post[index, bases, drop = TRUE]
      !is.na(runner_id[[index]]) &
        any(!is.na(values) & values == runner_id[[index]])
    }, logical(1))
  }
  rows <- list()
  add_rows <- function(eligible, type, runner_column, success, run_value) {
    eligible[is.na(eligible)] <- FALSE
    if (!any(eligible)) return(invisible(NULL))
    selected <- plate_appearances[eligible, , drop = FALSE]
    id <- selected[[runner_column]]
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      game_pk = selected$game_pk,
      game_date = selected$game_date,
      at_bat_index = selected$at_bat_index,
      inning = selected$inning,
      half_inning = selected$half_inning,
      batting_team = selected$batting_team,
      fielding_team = selected$fielding_team,
      runner_id = id,
      runner_name = runner_name(id),
      opportunity_type = type,
      success = as.logical(success[eligible]),
      advancement_run_value = run_value,
      event_key = selected$event_key,
      outs_before = selected$outs_before,
      launch_speed = selected$launch_speed,
      launch_angle = selected$launch_angle,
      hit_location = selected$hit_location,
      batted_ball_type = selected$batted_ball_type,
      description = selected$description
    )
    invisible(NULL)
  }
  event <- plate_appearances$event_key
  runs <- plate_appearances$runs_scored_on_play
  r1 <- plate_appearances$runner_first_before_id
  r2 <- plate_appearances$runner_second_before_id
  r3 <- plate_appearances$runner_third_before_id
  ahead_second <- as.integer(!is.na(r3))
  ahead_first <- as.integer(!is.na(r2)) + as.integer(!is.na(r3))

  add_rows(
    event == "single" & !is.na(r2),
    "single_second_scores",
    "runner_second_before_id",
    !post_contains(r2) & runs >= ahead_second + 1L,
    0.55
  )
  add_rows(
    event == "single" & !is.na(r1),
    "single_first_to_third",
    "runner_first_before_id",
    post_contains(r1, 3L) | (!post_contains(r1) & runs >= ahead_first + 1L),
    0.25
  )
  add_rows(
    event == "double" & !is.na(r1),
    "double_first_scores",
    "runner_first_before_id",
    !post_contains(r1) & runs >= ahead_first + 1L,
    0.45
  )
  productive_out <- plate_appearances$is_out &
    !plate_appearances$is_strikeout &
    plate_appearances$outs_on_play == 1L &
    plate_appearances$outs_before <= 1L
  add_rows(
    productive_out & !is.na(r2),
    "second_to_third_on_out",
    "runner_second_before_id",
    post_contains(r2, 3L) | (!post_contains(r2) & runs >= ahead_second + 1L),
    0.25
  )
  add_rows(
    productive_out & !is.na(r1) & is.na(r2),
    "first_to_second_on_out",
    "runner_first_before_id",
    post_contains(r1, 2:3) | (!post_contains(r1) & runs >= 1L),
    0.20
  )
  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

#' Build runner-advancement prevention ratings
#'
#' @param pbp Raw MLB play-by-play.
#' @param fielding_opportunities Optional output from
#'   [build_fielding_opportunity_view()].
#' @param player_reference Optional player table for fielder ID matching.
#'
#' @return A list containing opportunity, player, and team tables.
#' @export
build_runner_advancement_fielding <- function(
  pbp,
  fielding_opportunities = NULL,
  player_reference = NULL
) {
  data <- .fielding_advancement_rows(pbp)
  if (!nrow(data)) {
    return(list(
      opportunities = tibble::tibble(),
      player = tibble::tibble(),
      team = tibble::tibble()
    ))
  }
  if (is.null(fielding_opportunities)) {
    fielding_opportunities <- build_fielding_opportunity_view(
      pbp,
      player_reference = player_reference
    )
  }
  credit_columns <- c(
    "game_pk", "at_bat_index", "fielder_id", "fielder_name",
    "position", "credit_confidence", "fielder_assignment_method"
  )
  credit <- unique(as.data.frame(fielding_opportunities)[
    ,
    intersect(credit_columns, names(fielding_opportunities)),
    drop = FALSE
  ])
  data <- dplyr::left_join(data, credit, by = c("game_pk", "at_bat_index"))
  data$runner_group <- ifelse(
    is.na(data$runner_id),
    "",
    as.character(data$runner_id)
  )
  data$fielder_group <- ifelse(
    is.na(data$fielder_id),
    "",
    as.character(data$fielder_id)
  )
  data$context_group <- paste(
    data$opportunity_type,
    ifelse(is.na(data$position), "UNK", data$position),
    ifelse(is.na(data$hit_location), "UNK", data$hit_location),
    .fielding_trajectory(data),
    pmin(data$outs_before, 2),
    sep = ":"
  )
  model <- .fit_shrunk_logit_effects(
    data,
    "success",
    list(
      runner = list(column = "runner_group", prior = 35),
      fielder = list(column = "fielder_group", prior = 70),
      context = list(column = "context_group", prior = 60)
    )
  )
  data$expected_advancement_rate <- model$probability_without$fielder
  data$adjusted_advancement_rate <- model$probability
  data$advancement_credit <- data$expected_advancement_rate -
    as.numeric(data$success %in% TRUE)
  data$advancement_runs_saved <- data$advancement_credit *
    data$advancement_run_value

  covered <- data[
    !is.na(data$fielder_id) & nzchar(as.character(data$fielder_id)),
    ,
    drop = FALSE
  ]
  make_board <- function(group, player = TRUE) {
    groups <- split(seq_len(nrow(covered)), group)
    if (!length(groups)) return(tibble::tibble())
    board <- lapply(groups, function(indices) {
      row <- data.frame(
        opportunities = length(indices),
        advancements_allowed = sum(covered$success[indices] %in% TRUE),
        observed_advancement_rate = mean(covered$success[indices] %in% TRUE),
        expected_advancement_rate = mean(
          covered$expected_advancement_rate[indices]
        ),
        adjusted_advancement_rate = mean(
          covered$adjusted_advancement_rate[indices]
        ),
        advancements_prevented = sum(covered$advancement_credit[indices]),
        advancement_runs_saved = sum(
          covered$advancement_runs_saved[indices]
        ),
        stringsAsFactors = FALSE
      )
      if (player) {
        row$player_id <- as.character(covered$fielder_id[indices[[1L]]])
        row$player_name <- as.character(covered$fielder_name[indices[[1L]]])
        row$team <- as.character(covered$fielding_team[indices[[1L]]])
        row$position <- as.character(covered$position[indices[[1L]]])
      } else {
        row$team <- as.character(covered$fielding_team[indices[[1L]]])
      }
      row
    })
    tibble::as_tibble(do.call(rbind, board))
  }
  player_group <- paste(
    covered$fielder_id,
    covered$fielding_team,
    covered$position,
    sep = "\034"
  )
  player <- make_board(player_group, TRUE)
  if (nrow(player)) {
    player$reliability <- player$opportunities / (player$opportunities + 70)
    player$runs_saved_per_100 <- 100 * player$advancement_runs_saved /
      pmax(player$opportunities, 1)
    player$advancement_prevention_score <- .fielding_position_score(
      player$runs_saved_per_100,
      player$position,
      player$reliability
    )
    player$model_version <- "sabrhood_runner_advancement_v1"
  }
  team <- make_board(covered$fielding_team, FALSE)
  if (nrow(team)) {
    team$reliability <- team$opportunities / (team$opportunities + 350)
    team$runs_saved_per_100 <- 100 * team$advancement_runs_saved /
      pmax(team$opportunities, 1)
    team$advancement_prevention_score <- .fielding_position_score(
      team$runs_saved_per_100,
      rep("TEAM", nrow(team)),
      team$reliability
    )
    team$model_version <- "sabrhood_runner_advancement_v1"
  }
  list(
    opportunities = tibble::as_tibble(data),
    player = player,
    team = team
  )
}

#' Standardize Baseball Savant Fielding Run Value data
#'
#' @param data Parsed records from the official leaderboard.
#' @param season Season represented by the records.
#'
#' @return A stable Fielding Run Value leaderboard contract.
#' @export
standardize_fielding_run_value <- function(data, season) {
  if (!is.data.frame(data) || !nrow(data)) return(tibble::tibble())
  required <- c(
    "id", "name", "team_id", "team_name", "total_runs", "range_runs",
    "arm_runs", "dp_runs", "catching_runs", "framing_runs",
    "throwing_runs", "blocking_runs", "tot_pa"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Official fielding data are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  numeric_value <- function(name) suppressWarnings(as.numeric(data[[name]]))
  name_parts <- strsplit(as.character(data$name), ",", fixed = TRUE)
  player_name <- vapply(name_parts, function(parts) {
    if (length(parts) >= 2L) {
      trimws(paste(parts[-1L], parts[[1L]]))
    } else {
      trimws(parts[[1L]])
    }
  }, character(1))
  position_columns <- intersect(
    paste0("outs_", 2:9),
    names(data)
  )
  position_codes <- c(
    outs_2 = "C", outs_3 = "1B", outs_4 = "2B", outs_5 = "3B",
    outs_6 = "SS", outs_7 = "LF", outs_8 = "CF", outs_9 = "RF"
  )
  primary_position <- vapply(seq_len(nrow(data)), function(index) {
    values <- suppressWarnings(as.numeric(data[index, position_columns]))
    if (!length(values) || all(!is.finite(values)) || max(values, na.rm = TRUE) <= 0) {
      return(NA_character_)
    }
    unname(position_codes[position_columns[[which.max(values)]]])
  }, character(1))
  tibble::tibble(
    season = as.integer(season),
    player_id = as.character(data$id),
    player_name = player_name,
    team_id = as.character(data$team_id),
    team = .fielding_canonical_team(data$team_id, data$team_name),
    primary_position = primary_position,
    fielding_runs = numeric_value("total_runs"),
    infield_outfield_runs = numeric_value("inf_of_runs"),
    range_runs = numeric_value("range_runs"),
    arm_runs = numeric_value("arm_runs"),
    double_play_runs = numeric_value("dp_runs"),
    catching_runs = numeric_value("catching_runs"),
    framing_runs = numeric_value("framing_runs"),
    throwing_runs = numeric_value("throwing_runs"),
    blocking_runs = numeric_value("blocking_runs"),
    fielding_opportunities = numeric_value("outs_total"),
    innings = numeric_value("tot_pa"),
    source = "Baseball Savant Fielding Run Value Leaderboard",
    model_version = "savant_fielding_run_value_contract_v1"
  )
}

#' Build a position-specific Gold Glove Watch
#'
#' @param official_fielding Standardized official Fielding Run Value data.
#' @param advancement_ratings Optional SABRhood runner-advancement player board.
#' @param minimum_innings Minimum innings for the watch board.
#'
#' @return Position-specific watch rankings.
#' @export
build_gold_glove_watch <- function(
  official_fielding,
  advancement_ratings = NULL,
  minimum_innings = 100
) {
  data <- as.data.frame(official_fielding)
  if (!"league" %in% names(data)) data$league <- "MLB"
  data$league <- as.character(data$league)
  data$league[is.na(data$league) | !nzchar(data$league)] <- "MLB"
  data <- data[
    !is.na(data$primary_position) &
      is.finite(data$innings) &
      data$innings >= minimum_innings,
    ,
    drop = FALSE
  ]
  if (!nrow(data)) return(tibble::tibble())
  data$reliability <- data$innings / (data$innings + 250)
  data$fielding_runs_per_1000 <- 1000 * data$fielding_runs /
    pmax(data$innings, 1)
  data$shrunk_fielding_rate <- data$fielding_runs_per_1000 *
    data$reliability
  data$gold_glove_score <- .fielding_position_score(
    data$shrunk_fielding_rate,
    interaction(data$league, data$primary_position, drop = TRUE),
    data$reliability
  )
  data$sabrhood_advancement_runs <- NA_real_
  data$advancement_prevention_score <- NA_real_
  if (is.data.frame(advancement_ratings) && nrow(advancement_ratings)) {
    match_index <- match(
      as.character(data$player_id),
      as.character(advancement_ratings$player_id)
    )
    data$sabrhood_advancement_runs <- advancement_ratings$advancement_runs_saved[
      match_index
    ]
    data$advancement_prevention_score <-
      advancement_ratings$advancement_prevention_score[match_index]
  }
  data <- data[
    order(
      data$league,
      data$primary_position,
      -data$gold_glove_score,
      -data$fielding_runs
    ),
    ,
    drop = FALSE
  ]
  data$position_rank <- ave(
    seq_len(nrow(data)),
    interaction(data$league, data$primary_position, drop = TRUE),
    FUN = seq_along
  )
  data$watch_status <- ifelse(
    data$position_rank <= 3,
    "front line",
    ifelse(data$position_rank <= 5, "on the watch", "field")
  )
  data$gold_glove_method <- paste(
    "league- and position-standardized, innings-shrunk official Fielding Run Value;",
    "not an award-vote forecast"
  )
  tibble::as_tibble(data)
}

#' Select the most valuable estimated fielding play by day
#'
#' @param fielding_opportunities Batted-ball opportunity view.
#' @param advancement_opportunities Optional runner-advancement opportunities.
#'
#' @return One top estimated play per game date.
#' @export
build_fielding_play_of_day <- function(
  fielding_opportunities,
  advancement_opportunities = NULL
) {
  data <- as.data.frame(fielding_opportunities)
  if (!nrow(data)) return(tibble::tibble())
  advancement <- data.frame()
  if (is.data.frame(advancement_opportunities) &&
      nrow(advancement_opportunities)) {
    advancement <- stats::aggregate(
      advancement_opportunities$advancement_runs_saved,
      by = list(
        game_pk = advancement_opportunities$game_pk,
        at_bat_index = advancement_opportunities$at_bat_index
      ),
      FUN = sum,
      na.rm = TRUE
    )
    names(advancement)[[3L]] <- "advancement_runs_saved"
    data <- dplyr::left_join(
      data,
      advancement,
      by = c("game_pk", "at_bat_index")
    )
  }
  if (!"advancement_runs_saved" %in% names(data)) {
    data$advancement_runs_saved <- 0
  }
  data$advancement_runs_saved[!is.finite(data$advancement_runs_saved)] <- 0
  data$play_runs_saved <- pmax(data$estimated_range_runs, 0) +
    pmax(data$advancement_runs_saved, 0)
  data <- data[
    data$converted_out %in% TRUE |
      data$advancement_runs_saved > 0,
    ,
    drop = FALSE
  ]
  if (!nrow(data)) return(tibble::tibble())
  data <- data[
    order(data$game_date, -data$play_runs_saved, -data$estimated_play_difficulty),
    ,
    drop = FALSE
  ]
  data <- data[!duplicated(data$game_date), , drop = FALSE]
  data$estimated_out_probability <- data$expected_out_probability
  data$play_story <- paste0(
    data$fielder_name,
    " converted a ",
    round(100 * data$expected_out_probability),
    "% estimated-out opportunity",
    ifelse(
      data$advancement_runs_saved > 0,
      paste0(
        " and added ",
        format(round(data$advancement_runs_saved, 2), nsmall = 2),
        " estimated advancement runs saved"
      ),
      ""
    ),
    "."
  )
  data$gameday_url <- paste0("https://www.mlb.com/gameday/", data$game_pk)
  data$publication_status <- paste(
    "development estimate; not official Catch Probability"
  )
  tibble::as_tibble(data[, c(
    "game_date", "game_pk", "play_id", "inning", "half_inning",
    "fielder_id", "fielder_name", "fielding_team", "position",
    "description", "trajectory_group", "launch_speed", "launch_angle",
    "hit_distance", "estimated_out_probability",
    "estimated_play_difficulty", "estimated_range_runs",
    "advancement_runs_saved", "play_runs_saved", "play_story",
    "gameday_url", "publication_status", "opportunity_model"
  )])
}
