% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sjer_flux_2022_06.R
\docType{data}
\name{sjer_flux_2022_06}
\alias{sjer_flux_2022_06}
\title{Computed flux values at a NEON site}
\format{
A nested data frame item list with 7200 rows and 8 columns
}
\usage{
data(sjer_flux_2022_06)
}
\description{
A nested dataset containing computed fluxes at the SJER site from June 2022. Fluxes were computed with  using compute_neon_flux(sjer_env_data_2022_06,sjer_megapit_data_2022_06).
}
\details{
\itemize{
\item startDateTime: Time period of measurement (as POSIXct)
\item horizontalPosition: Sensor location where flux is computed
\item flux_compute: A nested tibble with variables (1) flux, flux_err, and method (one of 4 implemented)
\item diffusivity: Computation of surface diffusivity
\item VSWCMeanQF: QF flag for soil water content across all vertical depths at the given horizontal position: 0 = no issues, 1 = monthly mean used in measurement, 2 = QF fail
\item soilTempMeanQF: QF flag for soil temperature across all vertical depths at the given horizontal position: 0 = no issues, 1 = monthly mean used in measurement, 2 = QF fail
\item soilCO2concentrationMeanQF: QF flag for soil CO2 concentration across all vertical depths at the given horizontal position: 0 = no issues, 1 = monthly mean used in measurement, 2 = QF fail
\item staPresMeanQF: QF flag for atmospheric pressure across all vertical depths at the given horizontal position: 0 = no issues, 1 = monthly mean used in measurement, 2 = QF fail
}
}
\keyword{datasets}
