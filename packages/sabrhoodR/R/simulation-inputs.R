#' Validate the daily game projection input contract
#'
#' @param games One row per scheduled game.
#' @param allow_projected_lineups Whether projected, rather than confirmed,
#'   lineups are sufficient for a conditional model run.
#'
#' @return The supplied games with component readiness, completeness, missing
#'   input, and publication-state fields.
#' @export
validate_projection_game_inputs <- function(games, allow_projected_lineups = TRUE) {
  if (!is.data.frame(games) || nrow(games) == 0L) {
    stop("`games` must be a non-empty data frame.", call. = FALSE)
  }
  required <- c(
    "game_id", "game_date", "away_team", "home_team",
    "away_starter_id", "home_starter_id",
    "away_lineup_status", "home_lineup_status",
    "park_factor", "weather_status",
    "away_roster_status", "home_roster_status"
  )
  missing <- setdiff(required, names(games))
  if (length(missing)) {
    stop("Projection games are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.logical(allow_projected_lineups) || length(allow_projected_lineups) != 1L || is.na(allow_projected_lineups)) {
    stop("`allow_projected_lineups` must be TRUE or FALSE.", call. = FALSE)
  }

  nonempty <- function(value) !is.na(value) & nzchar(trimws(as.character(value)))
  dates <- suppressWarnings(as.Date(games$game_date))
  lineup_levels <- if (isTRUE(allow_projected_lineups)) c("projected", "confirmed") else "confirmed"
  weather_levels <- c("available", "indoors", "not_required")

  output <- tibble::as_tibble(games)
  output$schedule_ready <- nonempty(games$game_id) & !is.na(dates) &
    nonempty(games$away_team) & nonempty(games$home_team) & games$away_team != games$home_team
  output$starters_ready <- nonempty(games$away_starter_id) & nonempty(games$home_starter_id)
  output$lineups_ready <- tolower(as.character(games$away_lineup_status)) %in% lineup_levels &
    tolower(as.character(games$home_lineup_status)) %in% lineup_levels
  output$park_ready <- is.finite(.numeric_value(games$park_factor)) & .numeric_value(games$park_factor) > 0
  output$weather_ready <- tolower(as.character(games$weather_status)) %in% weather_levels
  output$rosters_ready <- tolower(as.character(games$away_roster_status)) == "confirmed" &
    tolower(as.character(games$home_roster_status)) == "confirmed"

  readiness_columns <- c(
    "schedule_ready", "starters_ready", "lineups_ready", "park_ready",
    "weather_ready", "rosters_ready"
  )
  output$input_completeness <- rowMeans(as.data.frame(output[readiness_columns]))
  output$missing_inputs <- vapply(seq_len(nrow(output)), function(index) {
    row_readiness <- unlist(output[index, readiness_columns], use.names = FALSE)
    missing_components <- sub("_ready$", "", readiness_columns[!as.logical(row_readiness)])
    if (length(missing_components)) paste(missing_components, collapse = ", ") else "none"
  }, character(1))
  output$projection_ready <- output$input_completeness == 1
  output$readiness_label <- ifelse(
    output$projection_ready,
    "ready",
    ifelse(output$input_completeness >= 4 / 6, "conditional", "blocked")
  )
  output$lineup_gate <- if (isTRUE(allow_projected_lineups)) "projected_or_confirmed" else "confirmed_only"
  output$input_contract_method <- "sabrhood_projection_input_contract_v1"
  output
}

#' Plan a leverage- and handedness-aware bullpen chain
#'
#' @param candidates Bullpen candidates accepted by [rank_reliever_options()].
#' @param batter_pockets Ordered matchup pockets with `pocket_id`, `inning`,
#'   `upcoming_batter_side`, `leverage_index`, and `batters_in_pocket`.
#' @param max_pitches_per_reliever Scenario workload ceiling.
#' @param pitches_per_batter Planning estimate used to update workload between
#'   pockets.
#'
#' @return One row per matchup pocket with the selected reliever, alternatives,
#'   entering availability, and cumulative planned workload.
#' @export
plan_bullpen_chain <- function(
    candidates,
    batter_pockets,
    max_pitches_per_reliever = 30L,
    pitches_per_batter = 4) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0L) {
    stop("`candidates` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.data.frame(batter_pockets) || nrow(batter_pockets) == 0L) {
    stop("`batter_pockets` must be a non-empty data frame.", call. = FALSE)
  }
  pocket_required <- c("pocket_id", "inning", "upcoming_batter_side", "leverage_index", "batters_in_pocket")
  missing <- setdiff(pocket_required, names(batter_pockets))
  if (length(missing)) stop("Bullpen pockets are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  candidate_required <- c("pitcher_id", "pitcher_name", "availability_score", "throws")
  missing <- setdiff(candidate_required, names(candidates))
  if (length(missing)) stop("Reliever candidates are missing: ", paste(missing, collapse = ", "), call. = FALSE)

  max_pitches_per_reliever <- .integer_value(max_pitches_per_reliever)[[1L]]
  pitches_per_batter <- .numeric_value(pitches_per_batter)[[1L]]
  if (is.na(max_pitches_per_reliever) || max_pitches_per_reliever < 1L) {
    stop("`max_pitches_per_reliever` must be a positive integer.", call. = FALSE)
  }
  if (!is.finite(pitches_per_batter) || pitches_per_batter <= 0) {
    stop("`pitches_per_batter` must be positive.", call. = FALSE)
  }
  pocket_batters <- .integer_value(batter_pockets$batters_in_pocket)
  if (anyNA(pocket_batters) || any(pocket_batters < 1L)) {
    stop("Every bullpen pocket must contain at least one batter.", call. = FALSE)
  }

  pool <- tibble::as_tibble(candidates)
  pool$pitcher_id <- as.character(pool$pitcher_id)
  if (anyDuplicated(pool$pitcher_id)) stop("`candidates` must contain one row per pitcher.", call. = FALSE)
  pool$planned_pitches <- 0
  base_availability <- pmax(pmin(.numeric_value(pool$availability_score), 1), 0)
  prior_pitcher <- NA_character_
  output <- vector("list", nrow(batter_pockets))

  for (index in seq_len(nrow(batter_pockets))) {
    eligible <- pool$planned_pitches < max_pitches_per_reliever
    if (!any(eligible)) stop("No reliever remains below the scenario workload ceiling.", call. = FALSE)
    entering <- pool[eligible, , drop = FALSE]
    entering$availability_score <- pmax(
      base_availability[eligible] - 0.55 * entering$planned_pitches / max_pitches_per_reliever,
      0
    )
    ranked <- rank_reliever_options(
      entering,
      upcoming_batter_side = batter_pockets$upcoming_batter_side[[index]],
      leverage_index = batter_pockets$leverage_index[[index]]
    )
    selected <- ranked[1L, , drop = FALSE]
    selected_id <- as.character(selected$pitcher_id[[1L]])
    pool_index <- match(selected_id, pool$pitcher_id)
    estimated_pitches <- as.integer(round(pocket_batters[[index]] * pitches_per_batter))
    pool$planned_pitches[[pool_index]] <- pool$planned_pitches[[pool_index]] + estimated_pitches
    alternatives <- if (nrow(ranked) >= 2L) ranked$pitcher_name[seq.int(2L, min(3L, nrow(ranked)))] else character()
    alternatives <- if (length(alternatives)) paste(alternatives, collapse = " | ") else "none"
    action <- if (!is.na(prior_pitcher) && identical(selected_id, prior_pitcher)) "continue" else "enter"

    output[[index]] <- tibble::tibble(
      chain_step = index,
      pocket_id = as.character(batter_pockets$pocket_id[[index]]),
      inning = .integer_value(batter_pockets$inning[[index]]),
      upcoming_batter_side = toupper(as.character(batter_pockets$upcoming_batter_side[[index]])),
      leverage_index = .numeric_value(batter_pockets$leverage_index[[index]]),
      batters_in_pocket = pocket_batters[[index]],
      action = action,
      pitcher_id = selected_id,
      pitcher_name = as.character(selected$pitcher_name[[1L]]),
      throws = as.character(selected$throws[[1L]]),
      availability_entering = .numeric_value(selected$availability_score[[1L]]),
      matchup_score = .numeric_value(selected$matchup_score[[1L]]),
      performance_score = .numeric_value(selected$performance_score_used[[1L]]),
      selection_score = .numeric_value(selected$selection_score[[1L]]),
      estimated_pitches = estimated_pitches,
      cumulative_planned_pitches = pool$planned_pitches[[pool_index]],
      alternatives = alternatives,
      chain_method = "sabrhood_bullpen_chain_scenario_v1",
      interpretation_warning = "Scenario planner, not a trained manager-choice probability."
    )
    prior_pitcher <- selected_id
  }
  dplyr::bind_rows(output)
}
