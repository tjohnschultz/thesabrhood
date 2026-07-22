\name{predict_calibrated_probability}
\alias{predict_calibrated_probability}
\title{Apply a logistic probability calibration layer}
\usage{
predict_calibrated_probability(object, probability, epsilon = 1e-06)
}
\arguments{
\item{object}{A fitted SABRhood probability calibrator.}
\item{probability}{Raw probabilities.}
\item{epsilon}{Numerical probability bound.}
}
\value{Calibrated probabilities.}
\description{Applies a fitted Platt-style calibration layer.}
