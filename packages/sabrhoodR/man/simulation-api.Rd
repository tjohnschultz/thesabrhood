\name{simulation-api}
\alias{simulation-api}
\alias{validate_lineup_df}
\alias{prepare_lineup_event_probs}
\alias{simulate_starter_matchup}
\alias{make_pitcher_summary}
\alias{make_probability_table}
\alias{make_hitter_summary}
\alias{make_hitter_leaderboards}
\alias{make_pitcher_leaderboards}
\title{The SABRhood Starter Simulation API}
\description{
Validated helpers for converting a matchup-adjusted lineup into mutually
exclusive plate-appearance event probabilities, simulating a starting
pitcher's batters faced, and producing derived projection tables.
}
\usage{
validate_lineup_df(lineup_df, player_col = "player", team_col = "team")

prepare_lineup_event_probs(
  lineup_df,
  player_col = "player",
  team_col = "team",
  triple_share = 0.08,
  use_features = TRUE
)

simulate_starter_matchup(
  lineup_df,
  n_sims = 10000L,
  starter_bf = 22L,
  player_col = "player",
  team_col = "team",
  triple_share = 0.08,
  seed = 123L,
  keep_event_log = FALSE
)

make_pitcher_summary(simulation_results)

make_probability_table(simulation_results)

make_hitter_summary(hitter_sim_results)

make_hitter_leaderboards(batter_df)

make_pitcher_leaderboards(pitcher_df)
}
\arguments{
  \item{lineup_df}{A data frame with one row per hitter, in batting order.}
  \item{player_col}{Column containing player names.}
  \item{team_col}{Column containing team identifiers.}
  \item{triple_share}{Share of non-home-run extra-base hits assigned as triples.}
  \item{use_features}{Whether to use optional engineered feature scores.}
  \item{n_sims}{Number of Monte Carlo simulations.}
  \item{starter_bf}{Batters faced in each simulated starter appearance.}
  \item{seed}{A local reproducible seed, or \code{NULL}.}
  \item{keep_event_log}{Whether to retain plate-appearance event rows.}
  \item{simulation_results}{One row per simulated starter appearance.}
  \item{hitter_sim_results}{One row per hitter within a simulation.}
  \item{batter_df}{Derived hitter projection summaries.}
  \item{pitcher_df}{Derived pitcher projection summaries.}
}
\value{
Validation returns \code{TRUE} invisibly. Probability and summary helpers
return tibbles. \code{simulate_starter_matchup()} returns a named list of
simulation rows and compact derived outputs.
}
\details{
Raw play-by-play data and daily orchestration are intentionally outside this
API. Callers must construct and pass the lineup and matchup data explicitly.
}
