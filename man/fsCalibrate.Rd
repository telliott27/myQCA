% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate.R
\name{fsCalibrate}
\alias{fsCalibrate}
\title{fuzzy set calibration}
\usage{
fsCalibrate(x, thresholds)
}
\arguments{
\item{x}{a numeric vector of scores to be calibrates}

\item{thresholds}{a vector of length three with the thresholds to use for calibrating in the oder: fully out, crossover, full in}
}
\value{
A vector with the same length of x, with the scores of x transformed into fuzzy set membership scores
}
\description{
fsCalibrate calibrates fuzzy sets using the direct transformation method as described in Charles Ragin's
Redesigning Social Inquiry
}
