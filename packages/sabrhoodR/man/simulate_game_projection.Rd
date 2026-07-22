% Generated manually for the development package.
\name{simulate_game_projection}
\alias{simulate_game_projection}
\alias{summarize_game_projection}
\title{Simulate and summarize complete-game score distributions}
\usage{
simulate_game_projection(
  away_expected_runs,
  home_expected_runs,
  n_sims = 20000L,
  seed = 123L,
  extra_inning_run_rate = 0.6,
  away_team = "Away",
  home_team = "Home"
)

summarize_game_projection(
  simulation_draws,
  away_expected_runs = NA_real_,
  home_expected_runs = NA_real_
)
}
\arguments{
\item{away_expected_runs}{Positive expected runs for the away team.}
\item{home_expected_runs}{Positive expected runs for the home team.}
\item{n_sims}{Number of Monte Carlo draws.}
\item{seed}{Reproducible local seed, or \code{NULL}.}
\item{extra_inning_run_rate}{Expected runs per club per extra inning.}
\item{away_team}{Away-team label.}
\item{home_team}{Home-team label.}
\item{simulation_draws}{Draw-level data containing team labels, scores, and an extra-innings indicator.}
}
\value{
\code{simulate_game_projection()} returns a list containing draw-level scores,
a one-row projection summary, and margin and exact-score distributions.
\code{summarize_game_projection()} returns the one-row summary.
}
\description{
Creates a complete-game score distribution after upstream lineup, starter,
park, and bullpen models have supplied team run expectations. Regulation ties
are extended until every draw has a winner.
}
