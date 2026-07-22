\name{predict_run_expectation}
\alias{predict_run_expectation}
\title{Apply a score-scale calibration model}
\usage{
predict_run_expectation(object, raw_expected_runs, is_home, lower = 1.5, upper = 8)
}
\arguments{
\item{object}{A fitted SABRhood run calibrator.}
\item{raw_expected_runs}{Positive upstream run expectations.}
\item{is_home}{Logical or zero-one home indicators.}
\item{lower, upper}{Prediction bounds.}
}
\value{Numeric calibrated expected runs.}
\description{Applies the fitted log-scale and home-field correction.}
