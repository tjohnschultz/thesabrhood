#' Classify a pitcher's observed usage role
#'
#' @param games Number of pitching appearances.
#' @param starts Number of starts.
#' @param average_starter_bf Average batters faced when starting.
#' @param average_relief_bf Average batters faced in relief.
#'
#' @return A character vector containing `starter`, `reliever`, `opener`,
#'   `bulk`, `hybrid`, or `unknown`.
#' @export
classify_pitcher_role <- function(games, starts, average_starter_bf = NA_real_, average_relief_bf = NA_real_) {
  games <- .numeric_value(games)
  starts <- .numeric_value(starts)
  starter_share <- ifelse(games > 0, starts / games, NA_real_)

  dplyr::case_when(
    is.na(games) | games < 2 ~ "unknown",
    starts >= 2 & starter_share >= 0.30 & !is.na(average_starter_bf) & average_starter_bf <= 9 ~ "opener",
    (games - starts) >= 2 & starter_share < 0.50 & !is.na(average_relief_bf) & average_relief_bf >= 12 ~ "bulk",
    starter_share >= 0.60 ~ "starter",
    starter_share <= 0.20 ~ "reliever",
    TRUE ~ "hybrid"
  )
}

#' Build a time-aware pitcher registry
#'
#' @param appearances Canonical pitcher-appearance data.
#' @param as_of_date Date through which roles should be classified.
#'
#' @return One row per pitcher with handedness, role, workload, and confidence.
#' @export
build_pitcher_registry <- function(appearances, as_of_date = NULL) {
  required <- c("pitcher_id", "game_date", "is_starter", "batters_faced", "pitches")
  missing <- setdiff(required, names(appearances))
  if (length(missing) > 0L) stop("Appearances are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  if (is.null(as_of_date)) as_of_date <- max(as.Date(appearances$game_date), na.rm = TRUE)
  as_of_date <- as.Date(as_of_date)
  data <- appearances |>
    dplyr::filter(as.Date(.data$game_date) <= as_of_date) |>
    dplyr::arrange(.data$game_date, .data$game_pk)

  registry <- data |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(
      pitcher_name = .last_non_missing(.data$pitcher_name),
      current_team = .last_non_missing(.data$fielding_team),
      throws = .last_non_missing(.data$pitcher_hand),
      first_appearance = min(as.Date(.data$game_date), na.rm = TRUE),
      last_appearance = max(as.Date(.data$game_date), na.rm = TRUE),
      games = dplyr::n(),
      starts = sum(.data$is_starter, na.rm = TRUE),
      relief_appearances = sum(!.data$is_starter, na.rm = TRUE),
      average_pitches = mean(.data$pitches, na.rm = TRUE),
      average_batters_faced = mean(.data$batters_faced, na.rm = TRUE),
      average_starter_bf = ifelse(any(.data$is_starter), mean(.data$batters_faced[.data$is_starter], na.rm = TRUE), NA_real_),
      average_relief_bf = ifelse(any(!.data$is_starter), mean(.data$batters_faced[!.data$is_starter], na.rm = TRUE), NA_real_),
      .groups = "drop"
    )

  registry$pitcher_role <- classify_pitcher_role(
    registry$games, registry$starts, registry$average_starter_bf, registry$average_relief_bf
  )
  registry$starter_probability <- ifelse(registry$games > 0, registry$starts / registry$games, NA_real_)
  registry$reliever_probability <- 1 - registry$starter_probability
  registry$role_sample_reliability <- pmin(registry$games / 20, 1)
  role_separation <- abs(registry$starter_probability - 0.5) * 2
  registry$role_confidence <- round(registry$role_sample_reliability * role_separation, 3)
  registry$as_of_date <- as_of_date
  registry
}

.consecutive_usage_days <- function(dates, as_of_date) {
  used <- sort(unique(as.Date(dates)))
  used <- used[used < as.Date(as_of_date)]
  if (length(used) == 0L) return(0L)
  expected <- as.Date(as_of_date) - 1
  count <- 0L
  while (expected %in% used) {
    count <- count + 1L
    expected <- expected - 1
  }
  count
}

#' Estimate recent bullpen availability
#'
#' @param appearances Canonical pitcher appearances.
#' @param registry Optional pitcher registry used to retain relief-capable roles.
#' @param as_of_date Pregame date. Only appearances before this date are used.
#'
#' @return One row per pitcher with transparent workload and heuristic availability fields.
#' @export
build_bullpen_availability <- function(appearances, registry = NULL, as_of_date = Sys.Date()) {
  as_of_date <- as.Date(as_of_date)
  history <- appearances |>
    dplyr::mutate(game_date = as.Date(.data$game_date)) |>
    dplyr::filter(.data$game_date < as_of_date)
  if (nrow(history) == 0L) return(tibble::tibble())

  output <- history |>
    dplyr::group_by(.data$pitcher_id) |>
    dplyr::summarise(
      pitcher_name = .last_non_missing(.data$pitcher_name),
      team = .last_non_missing(.data$fielding_team),
      throws = .last_non_missing(.data$pitcher_hand),
      last_appearance = max(.data$game_date, na.rm = TRUE),
      pitches_last_1d = sum(.data$pitches[.data$game_date >= as_of_date - 1], na.rm = TRUE),
      pitches_last_3d = sum(.data$pitches[.data$game_date >= as_of_date - 3], na.rm = TRUE),
      pitches_last_7d = sum(.data$pitches[.data$game_date >= as_of_date - 7], na.rm = TRUE),
      appearances_last_3d = dplyr::n_distinct(.data$game_pk[.data$game_date >= as_of_date - 3]),
      consecutive_days_used = .consecutive_usage_days(.data$game_date, as_of_date),
      .groups = "drop"
    )
  output$days_rest <- as.integer(as_of_date - output$last_appearance)

  if (!is.null(registry)) {
    output <- dplyr::left_join(
      output,
      dplyr::select(registry, dplyr::all_of(c("pitcher_id", "pitcher_role", "role_confidence"))),
      by = "pitcher_id"
    ) |>
      dplyr::filter(.data$pitcher_role %in% c("reliever", "opener", "bulk", "hybrid", "unknown"))
  }

  output$availability_score <- 1 -
    pmin(output$pitches_last_1d / 60, 0.60) -
    pmin(output$pitches_last_3d / 180, 0.30) -
    pmin(output$consecutive_days_used * 0.12, 0.36)
  output$availability_score <- pmax(pmin(output$availability_score, 1), 0)
  output$availability_status <- dplyr::case_when(
    output$availability_score >= 0.75 ~ "available",
    output$availability_score >= 0.50 ~ "monitor",
    output$availability_score >= 0.25 ~ "limited",
    TRUE ~ "likely_unavailable"
  )
  output$availability_method <- "sabrhood_workload_heuristic_v1"
  output$as_of_date <- as_of_date
  dplyr::arrange(output, .data$team, dplyr::desc(.data$availability_score))
}

#' Build plate-appearance-level manager pitching decisions
#'
#' @param plate_appearances Canonical plate appearances.
#' @param registry Optional pitcher registry.
#'
#' @return Training rows for pitcher-hook and replacement-selection models.
#' @export
build_manager_pitching_decisions <- function(plate_appearances, registry = NULL) {
  required <- c("game_pk", "at_bat_index", "pitcher_id", "pitches_in_pa")
  missing <- setdiff(required, names(plate_appearances))
  if (length(missing) > 0L) stop("Plate appearances are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  decision_columns <- c(
    "game_pk", "game_date", "at_bat_index", "inning", "half_inning",
    "fielding_team", "pitcher_id", "pitcher_name", "pitcher_hand",
    "batter_id", "batter_name", "batter_side", "pitches_in_pa", "event_key",
    "fielding_score_diff"
  )
  optional_columns <- intersect(
    c(
      "batting_score_diff_before", "base_state_before", "outs_before",
      "is_risp_before", "leverage_proxy", "pressure_index", "game_state_label"
    ),
    names(plate_appearances)
  )
  decisions <- plate_appearances[, c(decision_columns, optional_columns), drop = FALSE]
  order_index <- order(decisions$game_date, decisions$game_pk, decisions$at_bat_index, na.last = TRUE)
  decisions <- decisions[order_index, , drop = FALSE]

  appearance_key <- paste(decisions$game_pk, decisions$pitcher_id, sep = "\034")
  decisions$batters_faced_in_appearance <- as.integer(
    stats::ave(seq_len(nrow(decisions)), appearance_key, FUN = seq_along)
  )
  decisions$pitches_in_appearance <- as.integer(
    stats::ave(decisions$pitches_in_pa, appearance_key, FUN = cumsum)
  )
  decisions$times_through_order_proxy <- ceiling(decisions$batters_faced_in_appearance / 9)

  decision_key <- paste(decisions$game_pk, decisions$fielding_team, sep = "\034")
  group_indices <- split(seq_len(nrow(decisions)), decision_key)
  lead_within_decision_group <- function(x, default = NA) {
    output <- rep(default, length(x))
    for (indices in group_indices) {
      if (length(indices) > 1L) output[indices[-length(indices)]] <- x[indices[-1L]]
    }
    output
  }
  decisions$next_pitcher_id <- lead_within_decision_group(decisions$pitcher_id, NA_character_)
  decisions$next_pitcher_name <- lead_within_decision_group(decisions$pitcher_name, NA_character_)
  decisions$next_batter_id <- lead_within_decision_group(decisions$batter_id, NA_character_)
  decisions$next_batter_name <- lead_within_decision_group(decisions$batter_name, NA_character_)
  decisions$next_batter_side <- lead_within_decision_group(decisions$batter_side, NA_character_)
  decisions$pitcher_pulled_after_pa <- ifelse(
    is.na(decisions$next_pitcher_id),
    NA,
    decisions$next_pitcher_id != decisions$pitcher_id
  )

  if (!is.null(registry)) {
    registry_index <- match(decisions$pitcher_id, registry$pitcher_id)
    for (column in c("pitcher_role", "role_confidence", "average_pitches", "average_batters_faced")) {
      decisions[[column]] <- registry[[column]][registry_index]
    }
  }
  tibble::as_tibble(decisions)
}
