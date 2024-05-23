#' @title Internal function to compute surface co2 flux at a given timepoint via Tang et al 2005

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given  zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors, compute the surface flux. This is done by linear extrapolation of surface fluxes. Modified from Tang et al (2005).

#' @param zOffset Required. depths below surface - assumed to be positive in value. Important for directionality!
#' @param co2 Required. co2 at depth (umol m--1)
#' @param co2_err Required. Associated errors with that value of co2
#' @param diffusive Required. diffusivity at each depth
#' @param diffusive_err Required Associated errors with diffusivity
#'

#' @return Data frame of fluxes associated error
#' @seealso [dejong_shappert_flux()], [hirano_flux()], [tang_2003_flux()] for other ways to compute surface fluxes.

#' @references
#' Tang, Jianwu, Laurent Misson, Alexander Gershenson, Weixin Cheng, and Allen H. Goldstein. 2005. “Continuous Measurements of Soil Respiration with and without Roots in a Ponderosa Pine Plantation in the Sierra Nevada Mountains.” Agricultural and Forest Meteorology 132 (3): 212–27. https://doi.org/10.1016/j.agrformet.2005.07.011.
#'
#' Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July):78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.



tang_2005_flux <- function(zOffset, co2, co2_err, diffusive, diffusive_err) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2023-01-20)
  #     original creation
  #     2024-04-08: update to get namespaces correct




  # Assume zOffset is positive, so we want the flux to be negative (opposite the direction of the flux)
  # Tang05: linearly extrapolate flux from the bottom two measurement leavels
  # Calculate diffusivty between measurement levels (use weighted harmonic mean based on Turcu et al. (2005) Vadose Zone J 4, 1161-1169).

  # Define the output vector:
  out_flux <- tibble::tibble(flux = NA, flux_err = NA, method = "tang_2005")

  if (any(is.na(co2)) | any(is.na(co2_err)) | any(is.na(diffusive)) | any(is.na(diffusive_err))) {
    return(out_flux)
  } else {
    diff_z_all <- diff(c(0, zOffset)) # Layer thickness (of ALL three layers)

    denom1 <- diff_z_all[1] / diffusive[1] + diff_z_all[2] / diffusive[2]
    denom2 <- diff_z_all[2] / diffusive[2] + diff_z_all[3] / diffusive[3]

    diffusive_mean <- c(
      sum(diff_z_all[1:2]) / sum(denom1),
      sum(diff_z_all[2:3]) / sum(denom2)
    )


    # The formula for an entry is:  (dz1 + dz2) / (dz1 / x1 + dz2 / x2 ) --> AWESOME USE OF THE CHAIN RULE!

    # so then
    # f_x1 = (dz1 + dz2) * (-1)* ( (dz1 / x1 + dz2 / x2 ) )^(-2) * (-1) * dz1 * (x1)^(-2)  --> Let's take advantage of some calculations
    # f_x1 = 1 / (dz1 + dz2) * diffusive_mean[1]^2 * dz1 * (x1)^(-2)

    # f_x2 = (dz1 + dz2) * (-1)* ( (dz1 / x1 + dz2 / x2 ) )^(-2) * (-1) * dz2 * (x2)^(-2)  --> Let's take advantage of some calculations
    # f_x2 = 1 / (dz1 + dz2) * diffusive_mean[1]^2 * dz2 * (x2)^(-2)

    diffusive_mean_pd_1 <- c(
      diff_z_all[1] * diffusive_mean[1]^2 / (sum(diff_z_all) * diffusive[1]^(2)),
      diff_z_all[2] * diffusive_mean[1]^2 / (sum(diff_z_all) * diffusive[2]^(2))
    )

    diffusive_mean_pd_2 <- c(
      diff_z_all[1] * diffusive_mean[2]^2 / (sum(diff_z_all) * diffusive[2]^(2)),
      diff_z_all[2] * diffusive_mean[2]^2 / (sum(diff_z_all) * diffusive[3]^(2))
    )


    diffusive_mean_err <- c(
      quadrature_error(diffusive_mean_pd_1, diffusive_err[1:2]),
      quadrature_error(diffusive_mean_pd_2, diffusive_err[2:3])
    )


    # Calculate diffusive flux between CO2 measurement levels (µmol m-2 s-1).
    # This is Fick's law, so Flux = -Da *(gradient)
    # for positive z, then this means co2 increases as z increases, so the negative sign in the equation means that flux is negative (opposite the direction of the gradient), so out of the soil.
    # This then means that we will force the flux to be positive after the calculation is done (see below)  ** sigh Calculus **
    # we use the the thickness of the bottom two layers
    diff_z <- diff(zOffset) # Layer thickness (bottom 2)
    diffusive_flux <- -diff(co2) / diff_z * diffusive_mean

    # This will be a two element vector

    # For the first element the calculation is -(x2-x1)/(z2-z1)*y1.  So then the pd consists of three things:
    # f_x1 <- 1/(z2-z1)*y1
    # f_x2 <- -1/(z2-z1)*y1
    # f_y1 <- -(x2-x1)/(z2-z1)
    f1_x1 <- 1 / diff_z[1] * diffusive_mean[1]
    f1_x2 <- -1 / diff_z[1] * diffusive_mean[1]
    f1_y1 <- -(co2[2] - co2[1]) / diff_z[1]

    # For the second element the calculation is -(x3-x2)/(z3-z2)*y2.  So then the pd consists of three things:
    # f_x2 <- 1/(z3-z2)*y1
    # f_x3 <- -1/(z3-z2)*y1
    # f_y2 <- -(x3-x2)/(z3-z2)
    f2_x2 <- 1 / diff_z[2] * diffusive_mean[2]
    f2_x3 <- -1 / diff_z[2] * diffusive_mean[2]
    f2_y2 <- -(co2[3] - co2[2]) / diff_z[2]

    diffusive_flux_err <- c(
      quadrature_error(c(f1_x1, f1_x2, f1_y1), c(co2_err[1], co2_err[2], diffusive_mean_err[1])),
      quadrature_error(c(f2_x2, f2_x3, f2_y2), c(co2_err[2], co2_err[3], diffusive_mean_err[2]))
    )


    # Determine the average depth (m) between the bottom 2 measurement levels so that this can be assigned to the flux measurement
    diffusive_flux_depth <- diff_z / 2 + zOffset[1:2]

    # Linear extrapolation
    # (x0,y0)  (x1,y1)
    # slope  --> (y1 - y0)/(x1-x0)
    # equation of line: y = y0 + slope*(x-x0)
    # here x = 0, so y at x = 0 is y0 + slope * -x0

    # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).


    surface_flux <- diffusive_flux[1] + (diffusive_flux[1] - diffusive_flux[2]) / (diffusive_flux_depth[1] - diffusive_flux_depth[2]) * -diffusive_flux_depth[1]



    # pd of surface flux - there are 2 measurements (fluxes at two depths)
    # y0 + ((y1 - y0)/(x1-x0)) * -x0

    surface_flux_pd_f1 <- 1 + (-1 / (diffusive_flux_depth[1] - diffusive_flux_depth[2])) * -diffusive_flux_depth[1]
    surface_flux_pd_f2 <- (1 / (diffusive_flux_depth[1] - diffusive_flux_depth[2])) * -diffusive_flux_depth[1]

    surface_flux_pd <- c(surface_flux_pd_f1, surface_flux_pd_f2)

    surface_flux_err <- quadrature_error(surface_flux_pd, diffusive_flux_err)

    # Now when we have a negative surface flux, that means CO2 is moving out of the soil. If the flux is positive, the gradient method assumptions are violated, so we return NA



    if (surface_flux < 0) {
      out_flux$flux <- -surface_flux
      out_flux$flux_err <- surface_flux_err
    }

    return(out_flux)
  }
}
