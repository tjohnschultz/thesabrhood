\name{build_player_probability_board}
\alias{build_player_probability_board}
\title{Build daily player performance probability leaderboards}
\usage{
build_player_probability_board(
  hitters,
  pitchers,
  lineups,
  probables,
  expected_pa = 4.2
)
}
\arguments{
\item{hitters}{Compact FanGraphs hitter season table.}
\item{pitchers}{Compact FanGraphs pitcher season table.}
\item{lineups}{Standardized posted batting orders.}
\item{probables}{Standardized probable starters.}
\item{expected_pa}{Expected plate appearances for each listed hitter.}
}
\value{A long-form probability leaderboard.}
\description{
Creates transparent season-rate baselines for hitter home runs, hits, total
bases, and probable-starter strikeout thresholds. Opponent and park adjustments
remain outside this development version.
}
