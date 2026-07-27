\name{run-game-engine}
\alias{build_catcher_stints}
\alias{assign_active_catcher}
\alias{build_run_game_pitch_opportunities}
\alias{build_run_game_ratings}
\alias{build_run_game_count_windows}
\alias{build_run_game_notes}
\title{Build pitch-level running-game intelligence}
\usage{
build_catcher_stints(pbp, starting_catchers = NULL, player_reference = NULL)

assign_active_catcher(data, catcher_stints)

build_run_game_pitch_opportunities(
  pbp,
  catcher_stints = NULL,
  starting_catchers = NULL,
  player_reference = NULL
)

build_run_game_ratings(opportunities)

build_run_game_count_windows(opportunities, prior_pitches = 30)

build_run_game_notes(count_windows, minimum_pitches = 15L)
}
\arguments{
\item{pbp}{Raw MLB play-by-play.}
\item{starting_catchers}{Optional game-level starting-catcher table.}
\item{player_reference}{Optional player-position reference used for labeled catcher inference.}
\item{data}{Pitch or opportunity rows containing game, fielding-team, and at-bat fields.}
\item{catcher_stints}{Active-catcher intervals returned by \code{build_catcher_stints()}.}
\item{opportunities}{Pitch-level rows returned by \code{build_run_game_pitch_opportunities()}.}
\item{prior_pitches}{League-prior pitch count used to shrink count-specific rates.}
\item{count_windows}{Pitcher-by-count summaries returned by \code{build_run_game_count_windows()}.}
\item{minimum_pitches}{Minimum count-specific sample used to publish a note.}
}
\value{
The stint and opportunity functions return tibbles. The rating function returns
a list of runner, pitcher, catcher, battery, and model-card tables. The count
functions return pitcher-by-count summaries and concise scouting notes.
}
\description{
Reconstructs defensive catcher stints, creates one pre-pitch running
opportunity per eligible base, estimates opponent- and context-adjusted runner,
pitcher, catcher, and battery ratings, and identifies pitcher-specific count
windows.
}
