\name{build_insane_baseball_awards}
\alias{build_insane_baseball_awards}
\title{Build the Insane Baseball Awards candidate boards}
\usage{build_insane_baseball_awards(hitter_summary, pitcher_summary, pitches, minimum_pa = 100L, minimum_split_pa = 15L)}
\arguments{
  \item{hitter_summary}{Season hitter summary.}
  \item{pitcher_summary}{Season pitcher summary.}
  \item{pitches}{Canonical pitch view.}
  \item{minimum_pa}{Minimum season PA for profile awards.}
  \item{minimum_split_pa}{Minimum count-specific PA.}
}
\value{A combined award candidate board.}
\description{Creates unusual transparent player leaderboards and scores each race for showcase selection.}
