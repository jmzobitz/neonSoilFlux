% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/co2_to_umol.R
\name{co2_to_umol}
\alias{co2_to_umol}
\title{Convert co2 concentration from ppm to µmol m-3 units}
\usage{
co2_to_umol(
  temperature,
  pressure,
  co2,
  temperature_err,
  pressure_err,
  co2_err,
  zOffset
)
}
\arguments{
\item{temperature}{Required. Soil temperature (degrees C)}

\item{pressure}{Required. Barometric air pressure (kilopascal)}

\item{co2}{Carbon dioxide in ppm}

\item{temperature_err}{Required. Reported Soil temperature error (degrees C)}

\item{pressure_err}{Required. Reported Barometric air pressure error (kilopascal)}

\item{co2_err}{Required. Carbon dioxide in ppm error}

\item{zOffset}{Required. Surface depth (m). Reported as a negative number.}
}
\value{
A value of the converted co2
}
\description{
Given a measurement of co2, convert it from ppm to umol m-3 based on temperature and pressure. Also compute associated error via quadrature.
}
\examples{
co2_to_umol(31,96.3,654,.15,.05,9,-.05)
}
\author{
John Zobitz \email{zobitz@augsburg.edu}
}
