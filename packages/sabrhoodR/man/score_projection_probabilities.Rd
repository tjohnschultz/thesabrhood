\name{score_projection_probabilities}
\alias{score_projection_probabilities}
\title{Score binary projection probabilities}
\usage{
score_projection_probabilities(probability, actual, epsilon = 1e-06)
}
\arguments{
\item{probability}{Numeric probabilities.}
\item{actual}{Binary outcomes.}
\item{epsilon}{Bound used for log loss.}
}
\value{A one-row tibble of forecast metrics.}
\description{Calculates Brier score, log loss, accuracy, and calibration bias.}
