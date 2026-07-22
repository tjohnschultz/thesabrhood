\name{fit_run_expectation_calibrator}
\alias{fit_run_expectation_calibrator}
\title{Fit a score-scale calibration model}
\usage{
fit_run_expectation_calibrator(
  training_rows,
  actual_col = "actual_runs",
  expected_col = "raw_expected_runs",
  home_col = "is_home",
  min_rows = 100L
)
}
\arguments{
\item{training_rows}{Data frame containing pregame expectations and observed runs.}
\item{actual_col}{Observed-runs column.}
\item{expected_col}{Raw expected-runs column.}
\item{home_col}{Home-indicator column.}
\item{min_rows}{Minimum complete training rows.}
}
\value{A \code{sabrhood_run_calibrator} object.}
\description{Fits a Poisson calibration layer without changing the upstream feature engine.}
