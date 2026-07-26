\name{build_matchup_event_probabilities}
\alias{build_matchup_event_probabilities}
\title{Build batter-versus-pitcher event probabilities}
\usage{
build_matchup_event_probabilities(
  matchups,
  hitters,
  pitchers,
  hitter_platoon = NULL,
  pitcher_platoon = NULL,
  hitter_form = NULL,
  pitcher_form = NULL,
  hitter_prior_pa = 200,
  pitcher_prior_pa = 250,
  split_prior_pa = 125,
  form_strength = 1
)
}
\arguments{
\item{matchups}{One row per batter-pitcher matchup.}
\item{hitters, pitchers}{Overall PBP performance summaries.}
\item{hitter_platoon, pitcher_platoon}{Optional summaries by opponent hand.}
\item{hitter_form, pitcher_form}{Optional non-overlapping recent-form tables.}
\item{hitter_prior_pa, pitcher_prior_pa}{League-prior sample sizes for overall rates.}
\item{split_prior_pa}{Player-overall prior size for handedness splits.}
\item{form_strength}{Multiplier for the restrained recent-form adjustment.}
}
\value{
A tibble with one mutually exclusive event distribution per matchup.
}
\description{
Combines shrunk hitter, pitcher, handedness, and recent-form event
distributions with multinomial log5 and normalizes the result to one.
}
