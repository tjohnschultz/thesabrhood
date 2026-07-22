#' Simulate a complete game from team run expectations
#'
#' This is the score-distribution layer used after lineup, starter, park, and
#' bullpen inputs have been translated into expected runs for each club. Tied
#' regulation games are extended one inning at a time so every draw has a
#' winner.
#'
#' @param away_expected_runs Positive expected runs for the away team.
#' @param home_expected_runs Positive expected runs for the home team.
#' @param n_sims Number of Monte Carlo draws.
#' @param seed Reproducible local seed, or `NULL` to use the caller's RNG state.
#' @param extra_inning_run_rate Expected runs per club per extra inning.
#' @param away_team Away-team label.
#' @param home_team Home-team label.
#'
#' @return A list containing draw-level scores, a one-row projection summary,
#'   and margin and exact-score distributions.
#' @export
simulate_game_projection <- function(
    away_expected_runs,
    home_expected_runs,
    n_sims = 20000L,
    seed = 123L,
    extra_inning_run_rate = 0.60,
    away_team = "Away",
    home_team = "Home") {
  validate_positive_scalar <- function(value, name, allow_zero = FALSE) {
    lower_ok <- if (allow_zero) value >= 0 else value > 0
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || !lower_ok) {
      stop("`", name, "` must be one finite ", if (allow_zero) "non-negative" else "positive", " number.", call. = FALSE)
    }
  }
  validate_positive_scalar(away_expected_runs, "away_expected_runs")
  validate_positive_scalar(home_expected_runs, "home_expected_runs")
  validate_positive_scalar(extra_inning_run_rate, "extra_inning_run_rate", allow_zero = TRUE)
  if (length(n_sims) != 1L || is.na(n_sims) || n_sims < 1 || n_sims != as.integer(n_sims)) {
    stop("`n_sims` must be a positive whole number.", call. = FALSE)
  }
  if (length(away_team) != 1L || is.na(away_team) || !nzchar(away_team) ||
      length(home_team) != 1L || is.na(home_team) || !nzchar(home_team)) {
    stop("Team labels must each be one non-empty value.", call. = FALSE)
  }

  run_simulation <- function() {
    away_runs <- stats::rpois(n_sims, away_expected_runs)
    home_runs <- stats::rpois(n_sims, home_expected_runs)
    tied_after_nine <- away_runs == home_runs
    extra_innings <- integer(n_sims)
    unresolved <- which(tied_after_nine)

    for (inning in seq_len(12L)) {
      if (!length(unresolved)) break
      away_runs[unresolved] <- away_runs[unresolved] + stats::rpois(length(unresolved), extra_inning_run_rate)
      home_runs[unresolved] <- home_runs[unresolved] + stats::rpois(length(unresolved), extra_inning_run_rate)
      extra_innings[unresolved] <- inning
      unresolved <- unresolved[away_runs[unresolved] == home_runs[unresolved]]
    }
    if (length(unresolved)) {
      home_wins <- stats::rbinom(length(unresolved), 1L, 0.5)
      home_runs[unresolved] <- home_runs[unresolved] + home_wins
      away_runs[unresolved] <- away_runs[unresolved] + (1L - home_wins)
    }

    tibble::tibble(
      sim = seq_len(n_sims),
      away_team = as.character(away_team),
      home_team = as.character(home_team),
      away_runs = away_runs,
      home_runs = home_runs,
      total_runs = away_runs + home_runs,
      home_margin = home_runs - away_runs,
      winner = ifelse(home_runs > away_runs, as.character(home_team), as.character(away_team)),
      went_extras = tied_after_nine,
      extra_innings = extra_innings
    )
  }

  draws <- if (is.null(seed)) run_simulation() else withr::with_seed(seed, run_simulation())
  summary <- summarize_game_projection(
    draws,
    away_expected_runs = away_expected_runs,
    home_expected_runs = home_expected_runs
  )

  margin_levels <- c("Away by 5+", "Away by 3-4", "Away by 2", "Away by 1", "Home by 1", "Home by 2", "Home by 3-4", "Home by 5+")
  margin_group <- ifelse(draws$home_margin <= -5, "Away by 5+",
    ifelse(draws$home_margin <= -3, "Away by 3-4",
      ifelse(draws$home_margin == -2, "Away by 2",
        ifelse(draws$home_margin == -1, "Away by 1",
          ifelse(draws$home_margin == 1, "Home by 1",
            ifelse(draws$home_margin == 2, "Home by 2",
              ifelse(draws$home_margin <= 4, "Home by 3-4", "Home by 5+")))))))
  margin_distribution <- as.data.frame(table(factor(margin_group, levels = margin_levels)), stringsAsFactors = FALSE)
  names(margin_distribution) <- c("margin_group", "simulations")
  margin_distribution$probability <- margin_distribution$simulations / n_sims
  margin_distribution <- tibble::as_tibble(margin_distribution)

  score_distribution <- draws |>
    dplyr::count(.data$away_runs, .data$home_runs, name = "simulations") |>
    dplyr::mutate(probability = .data$simulations / n_sims) |>
    dplyr::arrange(dplyr::desc(.data$probability), .data$away_runs, .data$home_runs)

  list(
    draws = draws,
    projection_summary = summary,
    margin_distribution = margin_distribution,
    score_distribution = score_distribution
  )
}

#' Summarize draw-level game simulations
#'
#' @param simulation_draws Draws containing `away_runs`, `home_runs`,
#'   `went_extras`, and team labels.
#' @param away_expected_runs Optional input expectation recorded in the output.
#' @param home_expected_runs Optional input expectation recorded in the output.
#'
#' @return A one-row tibble with win probabilities, score estimates,
#'   uncertainty intervals, and close-game probabilities.
#' @export
summarize_game_projection <- function(
    simulation_draws,
    away_expected_runs = NA_real_,
    home_expected_runs = NA_real_) {
  required <- c("away_team", "home_team", "away_runs", "home_runs", "went_extras")
  missing <- setdiff(required, names(simulation_draws))
  if (length(missing)) stop("Missing simulation columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!nrow(simulation_draws)) stop("`simulation_draws` must contain at least one draw.", call. = FALSE)
  total_runs <- simulation_draws$away_runs + simulation_draws$home_runs
  margin <- simulation_draws$home_runs - simulation_draws$away_runs
  quantile_value <- function(value, probability) unname(stats::quantile(value, probability, names = FALSE))

  tibble::tibble(
    away_team = as.character(simulation_draws$away_team[[1L]]),
    home_team = as.character(simulation_draws$home_team[[1L]]),
    simulations = nrow(simulation_draws),
    away_expected_runs_input = as.numeric(away_expected_runs),
    home_expected_runs_input = as.numeric(home_expected_runs),
    away_win_probability = mean(simulation_draws$away_runs > simulation_draws$home_runs),
    home_win_probability = mean(simulation_draws$home_runs > simulation_draws$away_runs),
    away_mean_runs = mean(simulation_draws$away_runs),
    home_mean_runs = mean(simulation_draws$home_runs),
    away_median_runs = stats::median(simulation_draws$away_runs),
    home_median_runs = stats::median(simulation_draws$home_runs),
    away_runs_p10 = quantile_value(simulation_draws$away_runs, 0.10),
    away_runs_p90 = quantile_value(simulation_draws$away_runs, 0.90),
    home_runs_p10 = quantile_value(simulation_draws$home_runs, 0.10),
    home_runs_p90 = quantile_value(simulation_draws$home_runs, 0.90),
    mean_total_runs = mean(total_runs),
    median_total_runs = stats::median(total_runs),
    total_runs_p10 = quantile_value(total_runs, 0.10),
    total_runs_p90 = quantile_value(total_runs, 0.90),
    one_run_probability = mean(abs(margin) == 1),
    extra_innings_probability = mean(simulation_draws$went_extras),
    home_margin_mean = mean(margin),
    projected_winner = if (mean(margin > 0) >= 0.5) as.character(simulation_draws$home_team[[1L]]) else as.character(simulation_draws$away_team[[1L]]),
    simulation_method = "independent_poisson_score_layer_v1"
  )
}
