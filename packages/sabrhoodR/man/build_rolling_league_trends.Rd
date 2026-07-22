\name{build_rolling_league_trends}
\alias{build_rolling_league_trends}
\title{Build rolling MLB-wide production and pitch-usage trends}
\usage{build_rolling_league_trends(pitches, plate_appearances, rolling_days = 14L)}
\arguments{
  \item{pitches}{Canonical pitch view.}
  \item{plate_appearances}{Canonical plate-appearance view.}
  \item{rolling_days}{Positive rolling calendar window.}
}
\value{A list containing pitch-usage and production data frames.}
\description{Aggregates league-wide activity into rolling calendar windows for contextual graphics.}
