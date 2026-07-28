\name{catcher-intelligence}
\alias{build_called_pitch_view}
\alias{score_catcher_framing}
\alias{standardize_abs_challenge_leaderboard}
\title{Build catcher framing and ABS challenge intelligence}
\usage{
build_called_pitch_view(
  pbp,
  catcher_stints = NULL,
  starting_catchers = NULL,
  player_reference = NULL
)

score_catcher_framing(
  called_pitches,
  location_prior = 35,
  strike_run_value = 0.125
)

standardize_abs_challenge_leaderboard(data, challenge_type)
}
\arguments{
\item{pbp}{Raw MLB play-by-play or a canonical pitch table.}
\item{catcher_stints}{Optional active-catcher intervals.}
\item{starting_catchers}{Optional game-level starting-catcher table.}
\item{player_reference}{Optional player-position reference used for labeled catcher inference.}
\item{called_pitches}{Called pitches returned by \code{build_called_pitch_view()}.}
\item{location_prior}{Prior called pitches per location cell.}
\item{strike_run_value}{Transparent run value assigned to an additional called strike.}
\item{data}{Parsed \code{absData} records from the official Baseball Savant leaderboard.}
\item{challenge_type}{ABS leaderboard role: batter, catcher, pitcher, or team-summary.}
}
\value{
The called-pitch function returns a catcher-assigned pitch table. The framing
function returns catcher ratings, location rates, and a model card. The ABS
function returns a stable standardized leaderboard table.
}
\description{
Creates a location- and context-adjusted development framing score from called
pitches and standardizes the official Baseball Savant ABS challenge
leaderboards.
}
