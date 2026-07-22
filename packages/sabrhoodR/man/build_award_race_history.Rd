\name{build_award_race_history}
\alias{build_award_race_history}
\title{Build award race history from dated FanGraphs checkpoints}
\usage{
build_award_race_history(
  checkpoints,
  prior_hitters = NULL,
  prior_pitchers = NULL,
  award = "MVP",
  top_n = 8L,
  opening_date = NULL
)
}
\arguments{
\item{checkpoints}{A list of lists containing \code{date}, \code{hitters}, and \code{pitchers}.}
\item{prior_hitters}{Optional prior-season compact hitter table.}
\item{prior_pitchers}{Optional prior-season compact pitcher table.}
\item{award}{Award name to retain.}
\item{top_n}{Number of current leaders traced across the season.}
\item{opening_date}{Opening date used to scale qualification thresholds.}
}
\value{A list containing full history, display rows, detected events, and current leaders.}
\description{Recalculates an award board at every checkpoint using only cumulative information available through that date.}
