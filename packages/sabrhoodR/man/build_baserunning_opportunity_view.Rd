\name{build_baserunning_opportunity_view}
\alias{build_baserunning_opportunity_view}
\title{Build empirical baserunning opportunities from MLB play-by-play}
\usage{
build_baserunning_opportunity_view(pbp, game_venues = NULL)
}
\arguments{
\item{pbp}{Raw BaseballR MLB play-by-play containing pitch rows and runner
identities in the \code{matchup.postOn*.id} fields.}

\item{game_venues}{Optional data frame with \code{game_pk} plus
\code{venue_id} and/or \code{venue_name}.}
}
\value{
A tibble with one row per baserunning opportunity.
}
\description{
Reconstructs runner identity before and after each plate appearance and
returns one row per advancement or stolen-base opportunity. The output is
designed for chronological, shrinkage-based modeling rather than as an
official scoring record.
}
