\name{fit_probability_calibrator}
\alias{fit_probability_calibrator}
\title{Fit a logistic probability calibration layer}
\usage{
fit_probability_calibrator(probability, actual, min_rows = 300L)
}
\arguments{
\item{probability}{Archived pregame probabilities.}
\item{actual}{Settled binary outcomes.}
\item{min_rows}{Minimum complete rows.}
}
\value{A \code{sabrhood_probability_calibrator} object.}
\description{Fits a Platt-style logistic correction using only settled pregame forecasts.}
