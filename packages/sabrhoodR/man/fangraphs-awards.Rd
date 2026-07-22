\name{standardize_fangraphs_season}
\alias{standardize_fangraphs_season}
\alias{build_award_race_boards}
\title{Build compact FanGraphs season and award-race products}
\usage{
standardize_fangraphs_season(hitters, pitchers, season)

build_award_race_boards(
  hitters,
  pitchers,
  prior_hitters = NULL,
  prior_pitchers = NULL,
  minimum_pa = 200L,
  minimum_outs = 150L,
  young_age = 26L
)
}
\arguments{
\item{hitters}{FanGraphs hitter source or compact hitter table.}
\item{pitchers}{FanGraphs pitcher source or compact pitcher table.}
\item{season}{Season year.}
\item{prior_hitters}{Optional compact prior-season hitter table.}
\item{prior_pitchers}{Optional compact prior-season pitcher table.}
\item{minimum_pa}{Minimum plate appearances for MVP scoring.}
\item{minimum_outs}{Minimum recorded outs for Cy Young scoring.}
\item{young_age}{Maximum age for the provisional rookie-development pool.}
}
\value{
\code{standardize_fangraphs_season()} returns compact hitter and pitcher tables.
\code{build_award_race_boards()} returns one combined league award-watch table.
}
\description{
Standardizes BaseballR FanGraphs leaderboard outputs and creates transparent
league performance scores for MVP, Cy Young, and a provisional rookie pool.
}
