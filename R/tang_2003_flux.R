#' @title Compute surface flux at a given timepoint using a surface gradient via Tang et al 2003

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given  zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors, compute the surface flux. This is done by computing the gradient of co2 (slope from linear regression) and linear extrapolation of surface diffusivity. Modified from Tang et al (2003).

#' @param zOffset Required. depths below surface - assumed to be positive in value. Important for directionality!
#' @param co2 Required. co2 at depth (umol m--1)
#' @param co2_err Required. Associated errors with that value of co2
#' @param diffusive Required. diffusivity at each depth
#' @param diffusive_err Required Associated errors with diffusivity
#'
#' @import tibble
#' @import dplyr

#' @return Data frame of fluxes associated error

# changelog and author contributions / copyrights
#   John Zobitz (2023-01-20)
#     original creation



tang_2003_flux <- function(zOffset,co2,co2_err,diffusive,diffusive_err) {

  # Assume zOffset is positive, so we want the flux to be negative (opposite the direction of the flux)
  # Tang03: estimate slope from linear regression of z and C, also then Da0

  # Define the output vector:
  out_flux <- tibble(flux = NA,flux_err = NA,method = 'tang_2003')

  if(any(is.na(co2)) | any(is.na(co2_err)) | any(is.na(diffusive)) | any(is.na(diffusive_err))) {
    return(out_flux)
  } else {


  n_obs <- length(co2)
  mean_z <- mean(zOffset)

  mean_co2 <- mean(co2)  # pd: 1/n_obs
  pd_mean_co2 <- 1/n_obs

  slope_co2 <- sum((co2-mean_co2)*(zOffset-mean_z))/(sum((zOffset-mean_z)^2))
  pd_slope_co2 <- (1-pd_mean_co2)*(zOffset-mean_z)/(sum((zOffset-mean_z)^2))

  #Compute the quadrature error
  slope_err <- quadrature_error(pd_slope_co2,co2_err)

  # Now do the same for the intercept of Diffusivity
  mean_diffus <- mean(diffusive)  # pd: 1/n_obs
  pd_mean_diffus <- 1/n_obs

  slope_diffus <- sum((diffusive-mean_diffus)*(zOffset-mean_z))/(sum((zOffset-mean_z)^2))
  pd_slope_diffus <- (1-pd_mean_diffus)*(zOffset-mean_z)/(sum((zOffset-mean_z)^2))


  slope_diffus_err <- quadrature_error(pd_slope_diffus,diffusive_err)

  d0 <- mean_diffus - slope_diffus*mean_z   # intercept of linear regression
  pd_d0 <- pd_mean_diffus - pd_slope_diffus*mean_z

  d0_err <- quadrature_error(pd_d0,diffusive_err)

  surface_flux <- -d0*slope_co2  # To get partial derivative we use the product rule

  surface_flux_pd <- c(-d0,-slope_co2)
  surface_flux_tot_err <- c(slope_err,d0_err)

  surface_flux_err <- quadrature_error(surface_flux_pd,surface_flux_tot_err)


  if(surface_flux < 0) {
    out_flux$flux = -surface_flux
    out_flux$flux_err  = surface_flux_err
  }

  return(out_flux)
}

}
