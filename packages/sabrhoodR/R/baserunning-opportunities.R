#' Build empirical baserunning opportunities from MLB play-by-play
#'
#' Reconstructs runner identity before and after each plate appearance and
#' returns one row per advancement or stolen-base opportunity. The output is
#' designed for chronological, shrinkage-based modeling rather than as an
#' official scoring record.
#'
#' @param pbp Raw BaseballR MLB play-by-play containing pitch rows and runner
#'   identities in the `matchup.postOn*.id` fields.
#' @param game_venues Optional data frame with `game_pk` plus `venue_id` and/or
#'   `venue_name`.
#'
#' @return A tibble with one row per baserunning opportunity.
#' @export
build_baserunning_opportunity_view <- function(pbp, game_venues = NULL) {
  if (!is.data.frame(pbp) || !nrow(pbp)) {
    stop("`pbp` must be a non-empty raw play-by-play data frame.", call. = FALSE)
  }

  required_identity <- c(
    "matchup.postOnFirst.id", "matchup.postOnSecond.id",
    "matchup.postOnThird.id"
  )
  missing_identity <- setdiff(required_identity, names(pbp))
  if (length(missing_identity)) {
    stop(
      "`pbp` is missing runner identity fields: ",
      paste(missing_identity, collapse = ", "),
      call. = FALSE
    )
  }

  plate_appearances <- build_plate_appearance_view(pbp)
  terminal_raw <- pbp[plate_appearances$source_row, , drop = FALSE]
  as_id <- function(x) {
    output <- as.character(x)
    output[is.na(x) | output %in% c("", "NA")] <- NA_character_
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
  lag_within_half <- function(x) {
    previous <- c(NA_character_, x[-length(x)])
    previous[first_in_half] <- NA_character_
    previous
  }
  plate_appearances$runner_first_before_id <- lag_within_half(
    plate_appearances$runner_first_after_id
  )
  plate_appearances$runner_second_before_id <- lag_within_half(
    plate_appearances$runner_second_after_id
  )
  plate_appearances$runner_third_before_id <- lag_within_half(
    plate_appearances$runner_third_after_id
  )

  batter_lookup <- unique(data.frame(
    runner_id = as_id(.column_or_default(pbp, c("matchup.batter.id", "batter_id"))),
    runner_name = as.character(.column_or_default(
      pbp,
      c("matchup.batter.fullName", "batter_name")
    )),
    stringsAsFactors = FALSE
  ))
  batter_lookup <- batter_lookup[
    !is.na(batter_lookup$runner_id) & nzchar(batter_lookup$runner_id),
    ,
    drop = FALSE
  ]
  batter_lookup <- batter_lookup[!duplicated(batter_lookup$runner_id), , drop = FALSE]
  runner_name <- function(ids) {
    batter_lookup$runner_name[match(as.character(ids), batter_lookup$runner_id)]
  }

  post_contains <- function(pa, runner_id, bases = 1:3) {
    post <- cbind(
      pa$runner_first_after_id,
      pa$runner_second_after_id,
      pa$runner_third_after_id
    )
    vapply(seq_len(nrow(post)), function(index) {
      ids <- post[index, bases, drop = TRUE]
      !is.na(runner_id[[index]]) &&
        any(!is.na(ids) & ids == runner_id[[index]])
    }, logical(1L))
  }

  opportunity_rows <- list()
  add_advancement <- function(rows, opportunity_type, runner_column, success) {
    if (!length(rows) || !any(rows)) return(invisible(NULL))
    selected <- plate_appearances[rows, , drop = FALSE]
    ids <- selected[[runner_column]]
    opportunity_rows[[length(opportunity_rows) + 1L]] <<- tibble::tibble(
      game_pk = selected$game_pk,
      game_date = selected$game_date,
      at_bat_index = selected$at_bat_index,
      inning = selected$inning,
      half_inning = selected$half_inning,
      batting_team = selected$batting_team,
      fielding_team = selected$fielding_team,
      pitcher_id = selected$pitcher_id,
      runner_id = ids,
      runner_name = runner_name(ids),
      batter_id = selected$batter_id,
      batter_name = selected$batter_name,
      opportunity_type = opportunity_type,
      attempted = TRUE,
      success = as.logical(success[rows]),
      event_key = selected$event_key,
      outs_before = selected$outs_before,
      runs_scored_on_play = selected$runs_scored_on_play,
      launch_speed = selected$launch_speed,
      launch_angle = selected$launch_angle,
      hit_location = selected$hit_location,
      batted_ball_type = selected$batted_ball_type,
      opportunity_source = "plate_appearance_transition"
    )
    invisible(NULL)
  }

  event <- plate_appearances$event_key
  runs <- plate_appearances$runs_scored_on_play
  r1 <- plate_appearances$runner_first_before_id
  r2 <- plate_appearances$runner_second_before_id
  r3 <- plate_appearances$runner_third_before_id
  ahead_of_second <- as.integer(!is.na(r3))
  ahead_of_first <- as.integer(!is.na(r2)) + as.integer(!is.na(r3))

  single_r2 <- event == "single" & !is.na(r2)
  single_r2_success <- !post_contains(plate_appearances, r2) &
    runs >= ahead_of_second + 1L
  add_advancement(single_r2, "single_second_scores", "runner_second_before_id", single_r2_success)

  single_r1 <- event == "single" & !is.na(r1)
  single_r1_success <- post_contains(plate_appearances, r1, 3L) |
    (!post_contains(plate_appearances, r1) & runs >= ahead_of_first + 1L)
  add_advancement(single_r1, "single_first_to_third", "runner_first_before_id", single_r1_success)

  double_r1 <- event == "double" & !is.na(r1)
  double_r1_success <- !post_contains(plate_appearances, r1) &
    runs >= ahead_of_first + 1L
  add_advancement(double_r1, "double_first_scores", "runner_first_before_id", double_r1_success)

  sac_r3 <- plate_appearances$is_out &
    !plate_appearances$is_strikeout &
    plate_appearances$outs_on_play == 1L &
    plate_appearances$outs_before <= 1L &
    !is.na(r3)
  add_advancement(
    sac_r3,
    "sac_fly_scores",
    "runner_third_before_id",
    runs >= 1L
  )

  productive_out <- plate_appearances$is_out &
    !plate_appearances$is_strikeout &
    plate_appearances$outs_on_play == 1L &
    plate_appearances$outs_before <= 1L
  out_r2 <- productive_out & !is.na(r2)
  out_r2_success <- post_contains(plate_appearances, r2, 3L) |
    (!post_contains(plate_appearances, r2) & runs >= ahead_of_second + 1L)
  add_advancement(out_r2, "second_to_third_on_out", "runner_second_before_id", out_r2_success)

  out_r1 <- productive_out & !is.na(r1) & is.na(r2)
  out_r1_success <- post_contains(plate_appearances, r1, 2:3) |
    (!post_contains(plate_appearances, r1) & runs >= 1L)
  add_advancement(out_r1, "first_to_second_on_out", "runner_first_before_id", out_r1_success)

  dp_window <- plate_appearances$is_out &
    !plate_appearances$is_strikeout &
    !is.na(r1) &
    plate_appearances$outs_before <= 1L
  add_advancement(
    dp_window,
    "ground_ball_double_play",
    "runner_first_before_id",
    plate_appearances$outs_on_play >= 2L
  )

  action_type <- .normalize_event_key(.column_or_default(
    pbp,
    c("details.eventType", "details.event")
  ))
  action_runner <- as_id(.column_or_default(pbp, c("player.id", "runner_id")))
  action_key <- paste(
    as.character(.column_or_default(pbp, c("game_pk", "gamePk"))),
    .integer_value(.column_or_default(pbp, c("about.atBatIndex", "atBatIndex"))),
    action_runner,
    sep = "\034"
  )
  action_description <- as.character(.column_or_default(
    pbp,
    c("details.description", "description")
  ))
  catcher_name <- sub(
    ".*catcher ([^,.]+).*",
    "\\1",
    action_description,
    ignore.case = TRUE
  )
  catcher_name[!grepl("catcher ", action_description, ignore.case = TRUE)] <- NA_character_

  add_steal_window <- function(
      runner_column,
      blocked_base_column,
      opportunity_type,
      success_event,
      failure_event) {
    runner_ids <- plate_appearances[[runner_column]]
    eligible <- !is.na(runner_ids) & is.na(plate_appearances[[blocked_base_column]])
    if (!any(eligible)) return(invisible(NULL))
    selected <- plate_appearances[eligible, , drop = FALSE]
    ids <- runner_ids[eligible]
    keys <- paste(selected$game_pk, selected$at_bat_index, ids, sep = "\034")
    success_key <- action_key[action_type == success_event]
    failure_key <- action_key[action_type == failure_event]
    success_index <- match(keys, success_key)
    failure_index <- match(keys, failure_key)
    attempted <- !is.na(success_index) | !is.na(failure_index)
    caught_index <- ifelse(!is.na(failure_index), failure_index, success_index)

    opportunity_rows[[length(opportunity_rows) + 1L]] <<- tibble::tibble(
      game_pk = selected$game_pk,
      game_date = selected$game_date,
      at_bat_index = selected$at_bat_index,
      inning = selected$inning,
      half_inning = selected$half_inning,
      batting_team = selected$batting_team,
      fielding_team = selected$fielding_team,
      pitcher_id = selected$pitcher_id,
      runner_id = ids,
      runner_name = runner_name(ids),
      batter_id = selected$batter_id,
      batter_name = selected$batter_name,
      opportunity_type = opportunity_type,
      attempted = attempted,
      success = ifelse(attempted, !is.na(success_index), NA),
      event_key = selected$event_key,
      outs_before = selected$outs_before,
      runs_scored_on_play = selected$runs_scored_on_play,
      launch_speed = selected$launch_speed,
      launch_angle = selected$launch_angle,
      hit_location = selected$hit_location,
      batted_ball_type = selected$batted_ball_type,
      opportunity_source = "stolen_base_window",
      catcher_name_on_out = ifelse(
        !is.na(failure_index),
        catcher_name[caught_index],
        NA_character_
      )
    )
    invisible(NULL)
  }

  add_steal_window(
    "runner_first_before_id",
    "runner_second_before_id",
    "steal_second",
    "stolen_base_2b",
    "caught_stealing_2b"
  )
  add_steal_window(
    "runner_second_before_id",
    "runner_third_before_id",
    "steal_third",
    "stolen_base_3b",
    "caught_stealing_3b"
  )

  if (!length(opportunity_rows)) {
    return(tibble::tibble())
  }
  output <- dplyr::bind_rows(opportunity_rows)
  if (!"catcher_name_on_out" %in% names(output)) {
    output$catcher_name_on_out <- NA_character_
  }

  if (!is.null(game_venues)) {
    if (!is.data.frame(game_venues) || !"game_pk" %in% names(game_venues)) {
      stop("`game_venues` must be a data frame containing `game_pk`.", call. = FALSE)
    }
    venue_columns <- intersect(c("game_pk", "venue_id", "venue_name"), names(game_venues))
    venue_lookup <- unique(game_venues[, venue_columns, drop = FALSE])
    venue_lookup$game_pk <- as.character(venue_lookup$game_pk)
    output <- dplyr::left_join(output, venue_lookup, by = "game_pk")
  }

  output |>
    dplyr::arrange(
      .data$game_date,
      .data$game_pk,
      .data$at_bat_index,
      .data$opportunity_type
    )
}
