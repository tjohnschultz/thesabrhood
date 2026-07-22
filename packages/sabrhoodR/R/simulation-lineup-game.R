.simulate_lineup_game_once <- function(starter_probs, bullpen_probs, starter_bf, total_bf, simulation_id) {
  probability_columns <- c("p_BB", "p_K", "p_1B", "p_2B", "p_3B", "p_HR", "p_OUT")
  event_levels <- c("BB", "K", "1B", "2B", "3B", "HR", "OUT")
  lineup_size <- nrow(starter_probs)
  hitter_index <- ((seq_len(total_bf) - 1L) %% lineup_size) + 1L
  events <- vapply(seq_len(total_bf), function(pa) {
    probabilities <- if (pa <= starter_bf) starter_probs[hitter_index[[pa]], probability_columns] else bullpen_probs[hitter_index[[pa]], probability_columns]
    sample(event_levels, size = 1L, prob = as.numeric(probabilities))
  }, character(1))
  log <- tibble::tibble(
    sim = simulation_id,
    pa_number = seq_len(total_bf),
    lineup_spot = starter_probs$lineup_spot[hitter_index],
    player = starter_probs$player[hitter_index],
    team = starter_probs$team[hitter_index],
    pitcher_phase = ifelse(seq_len(total_bf) <= starter_bf, "starter", "bullpen"),
    event = events,
    H = as.integer(events %in% c("1B", "2B", "3B", "HR")),
    BB = as.integer(events == "BB"),
    K = as.integer(events == "K"),
    HR = as.integer(events == "HR"),
    XBH = as.integer(events %in% c("2B", "3B", "HR")),
    TB = dplyr::case_when(events == "1B" ~ 1, events == "2B" ~ 2, events == "3B" ~ 3, events == "HR" ~ 4, TRUE ~ 0)
  )
  hitter <- log |>
    dplyr::group_by(.data$sim, .data$lineup_spot, .data$player, .data$team) |>
    dplyr::summarise(
      PA = dplyr::n(), H = sum(.data$H), BB = sum(.data$BB), K = sum(.data$K),
      HR = sum(.data$HR), XBH = sum(.data$XBH), TB = sum(.data$TB),
      starter_PA = sum(.data$pitcher_phase == "starter"), bullpen_PA = sum(.data$pitcher_phase == "bullpen"),
      .groups = "drop"
    )
  team <- dplyr::summarise(log, BF = dplyr::n(), H = sum(.data$H), BB = sum(.data$BB), K = sum(.data$K), HR = sum(.data$HR), XBH = sum(.data$XBH), TB = sum(.data$TB))
  list(hitter = hitter, team = team, event_log = log)
}

#' Simulate a batting order through starter and bullpen phases
#'
#' Uses one mutually exclusive plate-appearance event distribution while the
#' probable starter is in the game and a second distribution after the bullpen
#' phase begins. Batters continue through the order instead of restarting when
#' the pitching phase changes.
#'
#' @param starter_lineup Matchup-adjusted lineup for the probable starter.
#' @param bullpen_lineup Matchup-adjusted lineup for the opposing bullpen. If
#'   omitted, starter probabilities are reused.
#' @param n_sims Number of Monte Carlo draws.
#' @param starter_bf Mean starter batters faced.
#' @param total_bf Mean team batters faced in the full game.
#' @param starter_bf_sd,total_bf_sd Standard deviations for opportunity draws.
#' @param player_col,team_col Player and team columns.
#' @param seed Reproducible seed.
#' @param keep_event_log Whether to retain the full plate-appearance log.
#' @return Full-game hitter summaries, team summaries, and optional event logs.
#' @export
simulate_lineup_game <- function(
    starter_lineup,
    bullpen_lineup = NULL,
    n_sims = 5000L,
    starter_bf = 22,
    total_bf = 38,
    starter_bf_sd = 2.5,
    total_bf_sd = 3,
    player_col = "player",
    team_col = "team",
    seed = 123L,
    keep_event_log = FALSE) {
  if (is.null(bullpen_lineup)) bullpen_lineup <- starter_lineup
  if (!is.numeric(n_sims) || length(n_sims) != 1L || n_sims < 1 || n_sims != as.integer(n_sims)) stop("`n_sims` must be a positive whole number.", call. = FALSE)
  if (!is.numeric(starter_bf) || !is.numeric(total_bf) || length(starter_bf) != 1L || length(total_bf) != 1L || starter_bf < 1 || total_bf <= starter_bf) stop("Opportunity means must satisfy 1 <= starter_bf < total_bf.", call. = FALSE)
  starter_probs <- prepare_lineup_event_probs(starter_lineup, player_col = player_col, team_col = team_col, use_features = TRUE)
  bullpen_probs <- prepare_lineup_event_probs(bullpen_lineup, player_col = player_col, team_col = team_col, use_features = TRUE)
  if (!identical(starter_probs$player, bullpen_probs$player)) stop("Starter and bullpen lineups must contain the same ordered players.", call. = FALSE)
  simulate_phase <- function(probabilities, opportunities) {
    opportunities <- as.integer(opportunities)
    remaining <- opportunities
    draw <- list()
    allocated_probability <- 0
    for (event in c("BB", "K", "X1B", "X2B", "X3B", "HR")) {
      column <- switch(event, BB = "p_BB", K = "p_K", X1B = "p_1B", X2B = "p_2B", X3B = "p_3B", HR = "p_HR")
      event_probability <- probabilities[[column]]
      conditional <- if (allocated_probability >= 1) 0 else event_probability / (1 - allocated_probability)
      conditional <- pmin(pmax(conditional, 0), 1)
      draw[[event]] <- stats::rbinom(length(remaining), remaining, conditional)
      remaining <- remaining - draw[[event]]
      allocated_probability <- allocated_probability + event_probability
    }
    draw$OUT <- remaining
    draw
  }
  run <- function() {
    total_draws <- pmin(pmax(round(stats::rnorm(n_sims, total_bf, total_bf_sd)), 27L), 55L)
    starter_draws <- pmin(pmax(round(stats::rnorm(n_sims, starter_bf, starter_bf_sd)), 9L), total_draws - 1L)
    if (isTRUE(keep_event_log)) {
      simulations <- lapply(seq_len(n_sims), function(index) {
        .simulate_lineup_game_once(starter_probs, bullpen_probs, starter_draws[[index]], total_draws[[index]], index)
      })
      return(list(
        hitter = dplyr::bind_rows(lapply(simulations, `[[`, "hitter")),
        team = dplyr::bind_rows(lapply(simulations, `[[`, "team")),
        event_log = dplyr::bind_rows(lapply(simulations, `[[`, "event_log")),
        starter_bf = starter_draws,
        total_bf = total_draws
      ))
    }
    lineup_size <- nrow(starter_probs)
    spot <- seq_len(lineup_size)
    opportunity_matrix <- function(batters_faced) {
      quotient <- batters_faced %/% lineup_size
      remainder <- batters_faced %% lineup_size
      outer(quotient, rep(1, lineup_size)) + outer(remainder, spot, function(value, position) as.integer(position <= value))
    }
    starter_opportunities <- opportunity_matrix(starter_draws)
    total_opportunities <- opportunity_matrix(total_draws)
    bullpen_opportunities <- total_opportunities - starter_opportunities
    hitter_rows <- lapply(seq_len(lineup_size), function(index) {
      starter_events <- simulate_phase(starter_probs[index, , drop = FALSE], starter_opportunities[, index])
      bullpen_events <- simulate_phase(bullpen_probs[index, , drop = FALSE], bullpen_opportunities[, index])
      combine <- function(event) starter_events[[event]] + bullpen_events[[event]]
      x1b <- combine("X1B"); x2b <- combine("X2B"); x3b <- combine("X3B"); hr <- combine("HR")
      tibble::tibble(
        sim = seq_len(n_sims), lineup_spot = index, player = starter_probs$player[[index]], team = starter_probs$team[[index]],
        PA = total_opportunities[, index], H = x1b + x2b + x3b + hr, BB = combine("BB"), K = combine("K"),
        HR = hr, XBH = x2b + x3b + hr, TB = x1b + 2 * x2b + 3 * x3b + 4 * hr,
        starter_PA = starter_opportunities[, index], bullpen_PA = bullpen_opportunities[, index]
      )
    })
    hitter <- dplyr::bind_rows(hitter_rows)
    team <- hitter |>
      dplyr::group_by(.data$sim) |>
      dplyr::summarise(BF = sum(.data$PA), H = sum(.data$H), BB = sum(.data$BB), K = sum(.data$K), HR = sum(.data$HR), XBH = sum(.data$XBH), TB = sum(.data$TB), .groups = "drop")
    list(
      hitter = hitter,
      team = team,
      event_log = NULL,
      starter_bf = starter_draws,
      total_bf = total_draws
    )
  }
  draws <- if (is.null(seed)) run() else withr::with_seed(seed, run())
  hitter_summary <- draws$hitter |>
    dplyr::group_by(.data$lineup_spot, .data$player, .data$team) |>
    dplyr::summarise(
      sims = dplyr::n(), avg_PA = mean(.data$PA), avg_starter_PA = mean(.data$starter_PA), avg_bullpen_PA = mean(.data$bullpen_PA),
      avg_H = mean(.data$H), avg_BB = mean(.data$BB), avg_K = mean(.data$K), avg_HR = mean(.data$HR), avg_XBH = mean(.data$XBH), avg_TB = mean(.data$TB),
      p_1_plus_H = mean(.data$H >= 1), p_2_plus_H = mean(.data$H >= 2), p_1_plus_BB = mean(.data$BB >= 1), p_2_plus_K = mean(.data$K >= 2),
      p_1_plus_HR = mean(.data$HR >= 1), p_1_plus_XBH = mean(.data$XBH >= 1), p_2_plus_TB = mean(.data$TB >= 2), p_3_plus_TB = mean(.data$TB >= 3),
      H_p10 = unname(stats::quantile(.data$H, .10)), H_p90 = unname(stats::quantile(.data$H, .90)),
      TB_p10 = unname(stats::quantile(.data$TB, .10)), TB_p90 = unname(stats::quantile(.data$TB, .90)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$lineup_spot)
  team_summary <- tibble::tibble(
    sims = n_sims,
    avg_BF = mean(draws$total_bf),
    avg_starter_BF = mean(draws$starter_bf),
    avg_H = mean(draws$team$H), avg_BB = mean(draws$team$BB), avg_K = mean(draws$team$K),
    avg_HR = mean(draws$team$HR), avg_XBH = mean(draws$team$XBH), avg_TB = mean(draws$team$TB)
  )
  output <- list(hitter_summary = hitter_summary, team_summary = team_summary, hitter_sim_results = draws$hitter)
  if (isTRUE(keep_event_log)) output$event_log <- draws$event_log
  output
}
