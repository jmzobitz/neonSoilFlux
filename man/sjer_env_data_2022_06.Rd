% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sjer_env_data_2022_06.R
\docType{data}
\name{sjer_env_data_2022_06}
\alias{sjer_env_data_2022_06}
\title{Measured environmental data at a NEON site}
\format{
A nested 3 item list
}
\usage{
data(sjer_env_data_2022_06)
}
\description{
A nested dataset containing environmental variables at the SJER site from June 2022. A convenience data frame to make processing fluxes easier for testing. Computed with the function acquire_neon_data.  All the variables can be used to compute fluxes (along with megapit data).
}
\details{
\itemize{
  \item measurement: Name of environmental measurement
  \item data: Nested list of data corresponding to a measurement
  \item monthly_mean: Monthly mean of measurement, computed in `compute_monthly_mean`
}
}
\keyword{datasets}
