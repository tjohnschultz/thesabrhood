\name{fielding-engine}
\alias{extract_fielding_credit}
\alias{build_fielding_opportunity_view}
\alias{build_fielding_ratings}
\alias{build_runner_advancement_fielding}
\alias{standardize_fielding_run_value}
\alias{build_gold_glove_watch}
\alias{build_fielding_play_of_day}
\title{Build fielding and runner-advancement intelligence}
\usage{
extract_fielding_credit(description)

build_fielding_opportunity_view(
  pbp,
  player_reference = NULL,
  context_prior = 25
)

build_fielding_ratings(opportunities, prior_opportunities = 80)

build_runner_advancement_fielding(
  pbp,
  fielding_opportunities = NULL,
  player_reference = NULL
)

standardize_fielding_run_value(data, season)

build_gold_glove_watch(
  official_fielding,
  advancement_ratings = NULL,
  minimum_innings = 100
)

build_fielding_play_of_day(
  fielding_opportunities,
  advancement_opportunities = NULL
)
}
\arguments{
\item{description}{Official MLB play descriptions.}
\item{pbp}{Raw MLB play-by-play or a canonical pitch table.}
\item{player_reference}{Optional player table used to resolve fielder IDs.}
\item{context_prior}{Prior opportunities used to shrink detailed expected-out cells.}
\item{opportunities}{Rows returned by \code{build_fielding_opportunity_view()}.}
\item{prior_opportunities}{Prior opportunities used to stabilize player fielding ratings.}
\item{fielding_opportunities}{Optional credited batted-ball opportunity table.}
\item{data}{Parsed records from the official Baseball Savant leaderboard.}
\item{season}{Season represented by the official leaderboard.}
\item{official_fielding}{Standardized official Fielding Run Value data.}
\item{advancement_ratings}{Optional SABRhood runner-advancement player ratings.}
\item{minimum_innings}{Minimum innings for Gold Glove Watch.}
\item{advancement_opportunities}{Optional reconstructed runner-advancement rows.}
}
\value{
The credit, opportunity, rating, official-value, award-watch, and play-selection
functions return tibbles. \code{build_runner_advancement_fielding()} returns a
list containing opportunity, player, and team tables.
}
\description{
Builds transparent play-by-play fielding estimates, context-adjusted
runner-advancement prevention, a stable official Fielding Run Value contract,
league- and position-specific Gold Glove Watch rankings, and one labeled development Play
of the Day per date.
}
