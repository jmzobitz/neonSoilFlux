#' @title Internal function to compute surface co2 flux at a given timepoint via De Jong and Schappert (1972)

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given  zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors, compute the surface flux. This is done by estimating the surface concentration (through linear regression) and doing a gradient calculation. Modified from De Jong and Schappert (1972).

#' @param zOffset Required. depths below surface - assumed to be positive in value. Important for directionality!
#' @param co2 Required. co2 at depth (umol m--1)
#' @param co2_err Required. Associated errors with that value of co2
#' @param diffusive Required. diffusivity at each depth
#' @param diffusive_err Required Associated errors with diffusivity
#'
#' @return Data frame of fluxes associated error
#' @seealso [hirano_flux()], [tang_2003_flux()], [tang_2005_flux()] for other ways to compute surface fluxes.
#'
#' @references
#' Jong, E. De, and H. J. V. Schappert. 1972. “Calculation of Soil Respiration and Activity from CO2 Profiles in the Soil.” Soil Science 113 (5): 328.
#'
#' Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July):78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.

# changelog and author contributions / copyrights
#   John Zobitz (2023-01-20)
#     original creation
#     2024-04-08: update to get namespaces correct


dejong_shappert_flux <- function(zOffset, co2, co2_err, diffusive, diffusive_err) {
  # Define the output vector:
  out_flux <- tibble::tibble(flux = NA, flux_err = NA, method = "dejong_shappert_1972")

  if (any(is.na(co2)) | any(is.na(co2_err)) | any(is.na(diffusive)) | any(is.na(diffusive_err))) {
    return(out_flux)
  } else {
    # Assume zOffset is positive, so we want the flux to be negative (opposite the direction of the flux)
    # DeJong & Shappert: # estimate C0 (intercept of linear regression of z and C), then do a gradient

    n_obs <- length(co2)
    mean_z <- mean(zOffset)

    mean_co2 <- mean(co2) # pd: 1/n_obs
    pd_mean_co2 <- 1 / n_obs

    slope <- sum((co2 - mean_co2) * (zOffset - mean_z)) / (sum((zOffset - mean_z)^2))
    pd_slope <- (1 - pd_mean_co2) * (zOffset - mean_z) / (sum((zOffset - mean_z)^2))

    c0 <- mean_co2 - slope * mean_z # intercept of linear regression
    pd_c0 <- pd_mean_co2 - pd_slope * mean_z

    surface_flux <- -diffusive[1] * (c0 - co2[1]) / (-zOffset[1]) # Since we use the first value, we need to account for that in the partial derivative
    first_meas <- array(0, dim = n_obs)
    first_meas[1] <- 1

    flux_pd <- -diffusive[1] * (pd_c0 - first_meas) / (-zOffset[1])
    diffusive_pd <- -1 * (c0 - co2[1]) / (-zOffset[1])

    tot_pd <- c(flux_pd, diffusive_pd)
    tot_err <- c(co2_err, diffusive_err[1])

    surface_flux_err <- quadrature_error(tot_pd, tot_err)

    out_flux <- tibble::tibble(flux = NA, flux_err = NA, method = "dejong_shappert_1972")
    if (surface_flux < 0) {
      out_flux$flux <- -surface_flux
      out_flux$flux_err <- surface_flux_err
    }

    return(out_flux)
  }
}
