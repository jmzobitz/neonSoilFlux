% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tang_2005_flux.R
\name{tang_2005_flux}
\alias{tang_2005_flux}
\title{Internal function to compute surface co2 flux at a given timepoint via Tang et al 2005}
\usage{
tang_2005_flux(zOffset, co2, co2_err, diffusive, diffusive_err)
}
\arguments{
\item{zOffset}{Required. depths below surface - assumed to be positive in value. Important for directionality!}

\item{co2}{Required. co2 at depth (umol m--1)}

\item{co2_err}{Required. Associated errors with that value of co2}

\item{diffusive}{Required. diffusivity at each depth}

\item{diffusive_err}{Required Associated errors with diffusivity}
}
\value{
Data frame of fluxes associated error
}
\description{
Given  zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors, compute the surface flux. This is done by linear extrapolation of surface fluxes. Modified from Tang et al (2005).
}
\references{
Tang, Jianwu, Laurent Misson, Alexander Gershenson, Weixin Cheng, and Allen H. Goldstein. 2005. “Continuous Measurements of Soil Respiration with and without Roots in a Ponderosa Pine Plantation in the Sierra Nevada Mountains.” Agricultural and Forest Meteorology 132 (3): 212–27. https://doi.org/10.1016/j.agrformet.2005.07.011.

Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July):78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.
}
\seealso{
[dejong_shappert_flux()], [hirano_flux()], [tang_2003_flux()] for other ways to compute surface fluxes.
}
\author{
John Zobitz \email{zobitz@augsburg.edu}
}
