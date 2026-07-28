#' Build a called-pitch view for catcher framing analysis
#'
#' @param pbp Raw MLB play-by-play or a canonical pitch table.
#' @param catcher_stints Optional active-catcher intervals.
#' @param starting_catchers Optional starting defense.
#' @param player_reference Optional player-position reference.
#'
#' @return One row per taken pitch with a resolved location.
#' @export
build_called_pitch_view <- function(
  pbp,
  catcher_stints = NULL,
  starting_catchers = NULL,
  player_reference = NULL
) {
  raw_input <- !all(c("pitch_in_pa", "plate_x", "plate_z") %in% names(pbp))
  pitches <- if (raw_input) build_pitch_view(pbp) else pbp
  called <- pitches[
    pitches$is_called_strike %in% TRUE |
      pitches$is_called_ball %in% TRUE,
    ,
    drop = FALSE
  ]
  called <- called[
    is.finite(called$plate_x) &
      is.finite(called$plate_z) &
      is.finite(called$strike_zone_top) &
      is.finite(called$strike_zone_bottom) &
      called$strike_zone_top > called$strike_zone_bottom &
      !called$call_description %in% c("Pitchout", "Hit By Pitch"),
    ,
    drop = FALSE
  ]
  if (!nrow(called)) return(tibble::as_tibble(called))
  called$zone_z <- (
    called$plate_z - called$strike_zone_bottom
  ) / (called$strike_zone_top - called$strike_zone_bottom)
  called$horizontal_bin <- floor((called$plate_x + 2.5) / 0.15)
  called$vertical_bin <- floor((called$zone_z + 0.5) / 0.08)
  called$location_bin <- paste(
    called$horizontal_bin,
    called$vertical_bin,
    sep = ":"
  )
  called$called_strike <- called$is_called_strike %in% TRUE

  if (is.null(catcher_stints) && raw_input) {
    catcher_stints <- build_catcher_stints(
      pbp,
      starting_catchers = starting_catchers,
      player_reference = player_reference
    )
  }
  assign_active_catcher(called, catcher_stints)
}

#' Score catcher framing from pitch location and called outcomes
#'
#' Location cells are shrunk toward the league called-strike rate before
#' catcher and umpire effects are estimated together. The run translation uses
#' a transparent 0.125 runs per additional strike.
#'
#' @param called_pitches Output from [build_called_pitch_view()].
#' @param location_prior Prior taken pitches per location cell.
#' @param strike_run_value Run value assigned to an additional called strike.
#'
#' @return A list containing catcher ratings, location rates, and a model card.
#' @export
score_catcher_framing <- function(
  called_pitches,
  location_prior = 35,
  strike_run_value = 0.125
) {
  required <- c(
    "called_strike", "location_bin", "catcher_id", "umpire_id",
    "catcher_name", "fielding_team"
  )
  missing <- setdiff(required, names(called_pitches))
  if (length(missing)) {
    stop("Called-pitch data are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- as.data.frame(called_pitches)
  data <- data[
    !is.na(data$catcher_id) & nzchar(as.character(data$catcher_id)),
    ,
    drop = FALSE
  ]
  if (!nrow(data)) {
    return(list(
      catcher = tibble::tibble(),
      location = tibble::tibble(),
      model_card = tibble::tibble(
        model_version = "catcher_framing_location_v1",
        called_pitches = 0L,
        catcher_coverage = 0,
        publication_status = "no active-catcher coverage"
      )
    ))
  }
  league_rate <- mean(data$called_strike %in% TRUE)
  location <- data |>
    dplyr::group_by(.data$location_bin) |>
    dplyr::summarise(
      called_pitches = dplyr::n(),
      called_strikes = sum(.data$called_strike %in% TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      expected_called_strike_rate = (
        .data$called_strikes + location_prior * league_rate
      ) / (.data$called_pitches + location_prior),
      reliability = .data$called_pitches /
        (.data$called_pitches + location_prior)
    )
  data$location_expected <- location$expected_called_strike_rate[
    match(data$location_bin, location$location_bin)
  ]
  data$catcher_group <- as.character(data$catcher_id)
  data$umpire_group <- ifelse(
    is.na(data$umpire_id),
    "",
    as.character(data$umpire_id)
  )
  data$location_group <- as.character(data$location_bin)

  model <- .fit_shrunk_logit_effects(
    data,
    "called_strike",
    list(
      location = list(column = "location_group", prior = location_prior),
      catcher = list(column = "catcher_group", prior = 220),
      umpire = list(column = "umpire_group", prior = 500)
    )
  )
  board <- .run_game_effect_board(
    data,
    "called_strike",
    model,
    "catcher",
    "catcher_id",
    "catcher_name",
    "fielding_team"
  )
  board <- dplyr::rename(
    board,
    catcher_id = .data$player_id,
    catcher_name = .data$player_name,
    called_pitches = .data$opportunities,
    called_strikes = .data$outcomes,
    called_strike_rate = .data$observed_rate,
    expected_called_strike_rate = .data$expected_rate_without_player,
    adjusted_called_strike_rate = .data$adjusted_rate,
    extra_strikes = .data$outcomes_above_expected,
    framing_logit_effect = .data$logit_effect
  )
  board$framing_runs_estimate <- strike_run_value * board$extra_strikes
  board$framing_rate_above_expected <- board$extra_strikes / board$called_pitches
  board$framing_score <- pmax(
    50,
    pmin(150, 100 + 500 * board$framing_rate_above_expected)
  )
  board$reliability <- board$called_pitches / (board$called_pitches + 220)
  board$model_version <- "catcher_framing_location_v1"
  board <- board[order(-board$framing_runs_estimate), , drop = FALSE]

  model_card <- tibble::tibble(
    model_version = "catcher_framing_location_v1",
    called_pitches = nrow(data),
    catchers = dplyr::n_distinct(data$catcher_id),
    league_called_strike_rate = league_rate,
    location_prior = location_prior,
    strike_run_value = strike_run_value,
    publication_status = "development; umpire-adjusted location proxy"
  )
  list(catcher = board, location = location, model_card = model_card)
}

#' Standardize an official Baseball Savant ABS challenge leaderboard
#'
#' @param data Parsed `absData` records from the Baseball Savant leaderboard.
#' @param challenge_type Leaderboard type such as `catcher`, `batter`,
#'   `pitcher`, or `team-summary`.
#'
#' @return A stable compact ABS leaderboard contract.
#' @export
standardize_abs_challenge_leaderboard <- function(data, challenge_type) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!nrow(data)) return(tibble::tibble())
  value <- function(candidates, default = NA) {
    .run_game_column(data, candidates, default)
  }
  player_id <- .run_game_id(value(c(
    "id", "fielder_2", "batter", "pitcher", "player_id"
  )))
  player_name <- as.character(value(c(
    "player_name", "name", "team_name"
  )))
  output <- tibble::tibble(
    challenge_type = as.character(challenge_type),
    season = suppressWarnings(as.integer(value(c("year", "season")))),
    player_id = player_id,
    player_name = player_name,
    team = as.character(value(c("team_abbr", "parent_org", "team"))),
    challenge_opportunities = suppressWarnings(as.numeric(value(c(
      "n_total_sample", "n_challenge_opportunities"
    )))),
    reasonable_opportunities = suppressWarnings(as.numeric(value(c(
      "n_chal_reasonable_opps", "reasonable_opportunities"
    )))),
    challenges = suppressWarnings(as.numeric(value(c("n_challenges", "challenges"), 0))),
    overturns = suppressWarnings(as.numeric(value(c("n_overturns", "overturns"), 0))),
    failed_challenges = suppressWarnings(as.numeric(value(c("n_fails", "failed_challenges"), 0))),
    expected_challenges = suppressWarnings(as.numeric(value(c("exp_chal", "expected_challenges")))),
    expected_overturns = suppressWarnings(as.numeric(value(c(
      "exp_chal_gained", "expected_overturns"
    )))),
    overturns_vs_expected = suppressWarnings(as.numeric(value(c(
      "net_net_chal", "overturns_vs_expected"
    )))),
    challenge_runs = suppressWarnings(as.numeric(value(c(
      "n_chal_runs", "challenge_runs"
    )))),
    runs_vs_expected = suppressWarnings(as.numeric(value(c(
      "net_net_runs", "runs_vs_expected"
    )))),
    strikeout_flips = suppressWarnings(as.numeric(value(c("n_strikeouts", "strikeout_flips"), 0))),
    walk_flips = suppressWarnings(as.numeric(value(c("n_walks", "walk_flips"), 0))),
    challenge_rate = suppressWarnings(as.numeric(value(c(
      "rate_challenges", "challenge_rate"
    )))),
    expected_challenge_rate = suppressWarnings(as.numeric(value(c(
      "exp_rate_challenges", "expected_challenge_rate"
    )))),
    overturn_rate = suppressWarnings(as.numeric(value(c(
      "rate_overturns", "overturn_rate"
    ))))
  )
  output$overturn_rate <- ifelse(
    is.finite(output$overturn_rate),
    output$overturn_rate,
    ifelse(output$challenges > 0, output$overturns / output$challenges, NA_real_)
  )
  output$reliability <- output$challenges / (output$challenges + 10)
  output$source <- "Baseball Savant ABS Challenge Leaderboard"
  output$model_version <- "savant_abs_contract_v1"
  output[order(-output$runs_vs_expected, -output$overturns), , drop = FALSE]
}
