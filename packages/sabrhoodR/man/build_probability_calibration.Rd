\name{build_probability_calibration}
\alias{build_probability_calibration}
\title{Build fixed-width probability calibration bins}
\usage{
build_probability_calibration(probability, actual, bins = 10L)
}
\arguments{
\item{probability}{Numeric probabilities.}
\item{actual}{Binary outcomes.}
\item{bins}{Number of equal-width bins.}
}
\value{A tibble with one row per non-empty bin.}
\description{Compares mean forecast probability with observed outcome rate.}
