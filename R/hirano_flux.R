#' @title Internal function to compute surface co2 flux at a given timepoint via Hirano et al 2005

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given  zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors, compute the surface flux. This is done by estimating the surface concentration through linear regression and linear extrapolation of the bottom and top surface fluxes. Modified from Hirano et al (2005).

#' @param zOffset Required. depths below surface - assumed to be positive in value. Important for directionality!
#' @param co2 Required. co2 at depth (umol m--1)
#' @param co2_err Required. Associated errors with that value of co2
#' @param diffusive Required. diffusivity at each depth
#' @param diffusive_err Required Associated errors with diffusivity
#'

#' @return Data frame of fluxes associated error

#' @seealso [dejong_shappert_flux()], [tang_2003_flux()], [tang_2005_flux()] for other ways to compute surface fluxes.

#' @references
#' Hirano, Takashi, Honghyun Kim, and Yumiko Tanaka. 2003. “Long-Term Half-Hourly Measurement of Soil CO2 Concentration and Soil Respiration in a Temperate Deciduous Forest.” Journal of Geophysical Research: Atmospheres 108 (D20). https://doi.org/10.1029/2003JD003766.
#'
#' Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July):78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.



hirano_flux <- function(zOffset, co2, co2_err, diffusive, diffusive_err) {


  # changelog and author contributions / copyrights
  #   John Zobitz (2024-01-20)
  #     original creation
  #     2024-04-08: update to get namespaces correct




  # Assume zOffset is positive, so we want the flux to be negative (opposite the direction of the flux)
  # Hirano et al 2005: linearly extrapolate flux from the bottom and top measurement levels
  # We first calculate the concentration at the surface and estimate the diffusive flux (like DeJong and Schappert 1972)

  # DeJong & Shappert: # estimate C0 (intercept of linear regression of z and C), then do a gradient

  # Define the output vector:
  out_flux <- tibble::tibble(flux = NA, flux_err = NA, method = "hirano_2005")

  if (any(is.na(co2)) | any(is.na(co2_err)) | any(is.na(diffusive)) | any(is.na(diffusive_err))) {
    return(out_flux)
  } else {
    n_obs <- length(co2)
    mean_z <- mean(zOffset)

    mean_co2 <- mean(co2) # pd: 1/n_obs
    pd_mean_co2 <- 1 / n_obs

    slope <- sum((co2 - mean_co2) * (zOffset - mean_z)) / (sum((zOffset - mean_z)^2))
    pd_slope <- (1 - pd_mean_co2) * (zOffset - mean_z) / (sum((zOffset - mean_z)^2))

    c0 <- mean_co2 - slope * mean_z # intercept of linear regression
    pd_c0 <- pd_mean_co2 - pd_slope * mean_z

    # Compute the c0 error
    c0_err <- quadrature_error(pd_c0, co2_err)
    # Set the depths of all
    zOffset_all <- c(0, zOffset)
    co2_all <- c(c0, co2)
    co2_err_all <- c(c0_err, co2_err)

    # Calculate diffusive flux between CO2 measurement levels (µmol m-2 s-1).
    # This is Fick's law, so Flux = -Da *(gradient)
    # for positive z, then this means co2 increases as z increases, so the negative sign in the equation means that flux is negative (opposite the direction of the gradient), so out of the soil.
    # This then means that we will force the flux to be positive after the calculation is done (see below)  ** sigh Calculus **
    # we use the the thickness of the bottom two layers
    diff_z_all <- diff(zOffset_all) # Layer thickness (all depths)
    diffusive_flux <- -diff(co2_all) / diff_z_all * diffusive

    # Ok, so the partial derivatives need some computation
    # This will be a two element vector

    # For the first element the calculation is -(c1-c0)/(z1-0)*y1.  So then the pd consists of three things:
    # f_c0 <- 1*/(z1-0)*y1
    # f_c1 <- -1/(z1-0)*y1
    # f_y1 <- -(c1-c0)/(z1-0)
    f1_x1 <- 1 / diff_z_all[1] * diffusive[1]
    f1_x2 <- -1 / diff_z_all[1] * diffusive[1]
    f1_y1 <- -(co2_all[2] - co2_all[1]) / diff_z_all[1]

    # For the second element the calculation is -(c3-c2)/(z3-z2)*y3.  So then the pd consists of three things:
    # f_c2 <- 1/(z3-z2)*y3
    # f_c3 <- -1/(z3-z2)*y3
    # f_y2 <- -(c3-c2)/(z3-z2)
    f2_x2 <- 1 / diff_z_all[3] * diffusive[3]
    f2_x3 <- -1 / diff_z_all[3] * diffusive[3]
    f2_y3 <- -(co2_all[4] - co2_all[3]) / diff_z_all[3]

    diffusive_flux_err <- c(
      quadrature_error(c(f1_x1, f1_x2, f1_y1), c(co2_err_all[1], co2_err_all[2], diffusive_err[1])),
      quadrature_error(c(f2_x2, f2_x3, f2_y3), c(co2_err_all[3], co2_err_all[4], diffusive_err[3]))
    )


    # Determine the average depth (m) between all measurement levels so that this can be assigned to the flux measurement
    diffusive_flux_depth <- diff_z_all / 2 + c(0, zOffset[1:2])

    # Linear extrapolation
    # (x0,y0)  (x1,y1)
    # slope  --> (y1 - y0)/(x1-x0)
    # equation of line: y = y0 + slope*(x-x0)
    # here x = 0, so y at x = 0 is y0 + slope * -x0

    # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).


    surface_flux <- diffusive_flux[1] + (diffusive_flux[1] - diffusive_flux[3]) / (diffusive_flux_depth[1] - diffusive_flux_depth[3]) * -diffusive_flux_depth[1]


    # pd of surface flux - there are 2 measurements (fluxes at two depths)
    # y0 + ((y1 - y0)/(x1-x0)) * -x0

    surface_flux_pd_f1 <- 1 + (-1 / (diffusive_flux_depth[1] - diffusive_flux_depth[3])) * -diffusive_flux_depth[1]
    surface_flux_pd_f2 <- (1 / (diffusive_flux_depth[1] - diffusive_flux_depth[3])) * -diffusive_flux_depth[1]

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
