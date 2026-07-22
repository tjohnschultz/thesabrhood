\name{simulate_lineup_game}
\alias{simulate_lineup_game}
\title{Simulate a batting order through starter and bullpen phases}
\usage{
simulate_lineup_game(
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
  keep_event_log = FALSE
)
}
\arguments{
\item{starter_lineup}{Ordered hitter rows adjusted for the probable starter.}
\item{bullpen_lineup}{The same order adjusted for the opposing bullpen.}
\item{n_sims}{Number of Monte Carlo draws.}
\item{starter_bf}{Mean probable-starter batters faced.}
\item{total_bf}{Mean full-game team batters faced.}
\item{starter_bf_sd, total_bf_sd}{Opportunity standard deviations.}
\item{player_col, team_col}{Player and team columns.}
\item{seed}{Reproducible seed.}
\item{keep_event_log}{Whether to retain plate-appearance event rows.}
}
\value{A list with hitter and team simulation summaries.}
\description{Keeps the batting order continuous while switching from starter to bullpen event probabilities.}
