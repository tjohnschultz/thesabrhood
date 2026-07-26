\name{simulate_game_state}
\alias{simulate_game_state}
\title{Simulate a baseball game plate appearance by plate appearance}
\usage{
simulate_game_state(
  away_starter_probs,
  home_starter_probs,
  away_bullpen_probs,
  home_bullpen_probs,
  n_sims = 1000L,
  away_starter_bf = 21,
  home_starter_bf = 21,
  seed = 1L,
  max_innings = 15L,
  automatic_runner = TRUE
)
}
\arguments{
\item{away_starter_probs}{Away hitters versus the home starter.}
\item{home_starter_probs}{Home hitters versus the away starter.}
\item{away_bullpen_probs}{Away hitters versus the home bullpen.}
\item{home_bullpen_probs}{Home hitters versus the away bullpen.}
\item{n_sims}{Number of simulated games.}
\item{away_starter_bf}{Expected batters faced by the away starter.}
\item{home_starter_bf}{Expected batters faced by the home starter.}
\item{seed}{Random seed.}
\item{max_innings}{Maximum inning before an unresolved game is recorded as a tie.}
\item{automatic_runner}{Whether extra half-innings begin with a runner on second base.}
}
\value{
A list containing game-level results and summaries plus batter counting-stat
and threshold summaries.
}
\description{
Simulates innings, outs, bases, score, batting-order continuity, starter
workload, bullpen transitions, walk-offs, and extra innings from mutually
exclusive plate-appearance event probabilities.
}
