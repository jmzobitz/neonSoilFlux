% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_function.R
\name{fit_function}
\alias{fit_function}
\title{Internal function that interpolates a soil measurement to different depths}
\usage{
fit_function(
  input_depth,
  input_value,
  input_value_err,
  input_value_qf,
  interp_depth,
  measurement_special
)
}
\arguments{
\item{input_depth}{Required. Vector of measurement depths.}

\item{input_value}{Required. Vector of measured values (i.e. soil temperature and soil water) measured at depths input_depth}

\item{input_value_err}{Required. Vector or reported measurement errors. Used to compute prediction error (via quadrature) when linear interpolation is used.}

\item{input_value_qf}{Required. Vector of qf values from the smoothing 0 = no smoothing, 1 = mean value used (smoothing), 2 = NA value}

\item{interp_depth}{Depths of the sensors required for interpolation}

\item{measurement_special}{Flag if we want to just do linear interpolation for a given measurement}
}
\value{
A data frame of the depth and the measured column for the measurements and reported error
}
\description{
Definition function. Linearly interpolate a measurement across the different measurement depths
}
\author{
John Zobitz \email{zobitz@augsburg.edu}
based on code developed by Edward Ayres \email{eayres@battelleecology.org}
}
