\name{simulate_game_state_phase3}
\alias{simulate_game_state_phase3}
\title{Simulate a detailed plate-appearance game with named relievers}
\usage{
simulate_game_state_phase3(
  away_starter_probs,
  home_starter_probs,
  away_bullpen_probs,
  home_bullpen_probs,
  away_reliever_probs = NULL,
  home_reliever_probs = NULL,
  n_sims = 500L,
  away_starter_bf = 21,
  home_starter_bf = 21,
  seed = 1L,
  max_innings = 15L,
  automatic_runner = TRUE,
  park_baserunning = list(),
  baserunning_rates = list(),
  away_runner_profiles = NULL,
  home_runner_profiles = NULL,
  defense_steal_multipliers = c(away = 1, home = 1)
)
}
\arguments{
\item{away_starter_probs, home_starter_probs}{Ordered hitter event probabilities against the opposing starter.}
\item{away_bullpen_probs, home_bullpen_probs}{Ordered hitter event probabilities against the aggregate opposing bullpen.}
\item{away_reliever_probs, home_reliever_probs}{Ordered hitter probabilities against individual opposing relievers.}
\item{n_sims}{Number of simulated games.}
\item{away_starter_bf, home_starter_bf}{Expected starter batters faced.}
\item{seed}{Random seed.}
\item{max_innings}{Maximum inning before an unresolved tie is retained.}
\item{automatic_runner}{Whether extra half-innings begin with a runner on second.}
\item{park_baserunning}{Named park-sensitive advancement multipliers.}
\item{baserunning_rates}{Optional league baserunning-rate overrides.}
\item{away_runner_profiles, home_runner_profiles}{Long-form empirical runner profiles.}
\item{defense_steal_multipliers}{Named away/home defensive multipliers for
steal-attempt probability. Values below one suppress attempts.}
}
\value{A list with game, hitter, reliever, and detailed-event summaries.}
\description{
Extends the plate-appearance state engine with runner identity, stolen-base
attempts, double plays, productive outs, sacrifice flies, park-sensitive
advancement, and individual reliever selection and workload.
}
