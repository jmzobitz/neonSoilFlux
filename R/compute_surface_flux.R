#' @title Compute CO2 surface flux

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Computation function. Given a measurement of the co2 and diffusive flux at different levels, return the surface flux

#' @import dplyr

#' @param input_data Required. A data frame containing zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors

#' @return A value of the surface CO2 flux (umol m-2 s-1)

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation
#   John Zobitz (2022-06-21)
#     revision to account for correct directionality when computing fluxes
#   John Zobitz (2023-07-14)
#     revision to include value of gradient at surface and computation of quadrature errors


compute_surface_flux <- function(input_data) {

  # Select the first three levels (closest to the surface)
  input_data_rev <- input_data %>%
    arrange(desc(zOffset)) %>%
    slice(1:3)


  # Select the first three levels (closest to the surface)
  zOffset_pos <- input_data_rev |> pull(zOffset) |> abs() # Make these positive, just to avoid problems down the road.
  diff_z <- diff(zOffset_pos)  # forward difference quotient (z2 - z1, z3-z2)


  diffus <- input_data_rev |> pull(diffusivity)
  co2umol <- input_data_rev |> pull(co2_umol)

  co2_err <- input_data_rev |> pull(co2ExpUncert)
  diffusive_err <- input_data_rev |> pull(diffusExpUncert)

  # Calculate diffusivty between measurement levels (use weighted harmonic mean based on Turcu et al. (2005) Vadose Zone J 4, 1161-1169).



  denom1 <- diff_z[1] / diffus[1] + diff_z[2] / diffus[2]
  denom2 <- diff_z[1] / diffus[2] + diff_z[2] / diffus[3]

  diffusive_mean <- c(sum(diff_z)/sum(denom1),
                      sum(diff_z)/sum(denom2))

  # The formula for an entry is:  (dz1 + dz2) / (dz1 / x1 + dz2 / x2 ) --> AWESOME USE OF THE CHAIN RULE!

  # so then
  # f_x1 = (dz1 + dz2) * (-1)* ( (dz1 / x1 + dz2 / x2 ) )^(-2) * (-1) * dz1 * (x1)^(-2)  --> Let's take advantage of some calculations
  # f_x1 = 1 / (dz1 + dz2) * diffusive_mean[1]^2 * dz1 * (x1)^(-2)

  # f_x2 = (dz1 + dz2) * (-1)* ( (dz1 / x1 + dz2 / x2 ) )^(-2) * (-1) * dz2 * (x2)^(-2)  --> Let's take advantage of some calculations
  # f_x2 = 1 / (dz1 + dz2) * diffusive_mean[1]^2 * dz2 * (x2)^(-2)

  diffusive_mean_pd_1 <- c(
    diff_z[1]* diffusive_mean[1]^2 / (sum(diff_z) * diffus[1]^(2) ),
    diff_z[2]* diffusive_mean[1]^2 / (sum(diff_z) * diffus[2]^(2) )  )

  diffusive_mean_pd_2 <- c(
    diff_z[1]* diffusive_mean[2]^2 / (sum(diff_z) * diffus[2]^(2) ),
    diff_z[2]* diffusive_mean[2]^2 / (sum(diff_z) * diffus[3]^(2) )  )


  diffusive_mean_err <- c(
    quadrature_error(diffusive_mean_pd_1,diffusive_err[1:2]),
    quadrature_error(diffusive_mean_pd_2,diffusive_err[2:3])
  )


  # Calculate diffusive flux between CO2 measurement levels (µmol m-2 s-1).
  # This is Fick's law, so Flux = -Da *(gradient)
  # for positive z, then this means co2 increases as z increases, so the negative sign in the equation means that flux is negative (opposite the direction of the gradient), so out of the soil.
  # This then means that we will force the flux to be positive after the calculation is done (see below)  ** sigh Calculus **
  diffusive_flux <- -diff(co2umol)/diff_z * diffusive_mean

  # This will be a two element vector

  # For the first element the calculation is -(x2-x1)/(z2-z1)*y1.  So then the pd consists of three things:
  # f_x1 <- 1/(z2-z1)*y1
  # f_x2 <- -1/(z2-z1)*y1
  # f_y1 <- -(x2-x1)/(z2-z1)
  f_x1 <- 1/diff_z[1]*diffusive_mean[1]
  f_x2 <- -1/diff_z[1]*diffusive_mean[1]
  f_y1 <- -(co2umol[2]-co2umol[1])/diff_z[1]

  # For the second element the calculation is -(x3-x2)/(z3-z2)*y2.  So then the pd consists of three things:
  # f_x2 <- 1/(z3-z2)*y1
  # f_x3 <- -1/(z3-z2)*y1
  # f_y2 <- -(x3-x2)/(z3-z2)
  f_x2 <- 1/diff_z[2]*diffusive_mean[2]
  f_x3 <- -1/diff_z[2]*diffusive_mean[2]
  f_y2 <- -(co2umol[3]-co2umol[2])/diff_z[2]

  diffusive_flux_err <- c(

    quadrature_error(c(f_x1,f_x2,f_y1), c(co2_err[1],co2_err[2],diffusive_mean_err[1]) ),

    quadrature_error(c(f_x2,f_x3,f_y2), c(co2_err[2],co2_err[3],diffusive_mean_err[2]) )

  )



  gradient <- diff(co2umol)/diff_z
  # Determine the average depth (m) between measurement levels so that this can be assigned to the flux measurement
  diffusive_flux_depth <- diff_z/2 + zOffset_pos[1:2]

 # Linear extrapolation
 # (x0,y0)  (x1,y1)
 # slope  --> (y1 - y0)/(x1-x0)
 # equation of line: y = y0 + slope*(x-x0)
 # here x = 0, so y at x = 0 is y0 + slope * -x0

  # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).


  surface_flux <- diffusive_flux[1] + (diffusive_flux[1] - diffusive_flux[2]) / (diffusive_flux_depth[1] - diffusive_flux_depth[2]) * -diffusive_flux_depth[1]

  # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).
#  data$diffuFluxD0 <- data$diffuFluxD12 - (((data$diffuFluxD12 - data$diffuFluxD23) / #(abs(diffuFluxD12Depth) - abs(diffuFluxD23Depth))) * abs(diffuFluxD12Depth))


 # pd of surface flux - there are 2 measurements (fluxes at two depths)
  # y0 + ((y1 - y0)/(x1-x0)) * -x0

  surface_flux_pd_f1 <- 1 + (-1/(diffusive_flux_depth[1] - diffusive_flux_depth[2])) * -diffusive_flux_depth[1]
  surface_flux_pd_f2 <-  (1/(diffusive_flux_depth[1] - diffusive_flux_depth[2])) * -diffusive_flux_depth[1]

  surface_flux_pd <- c(surface_flux_pd_f1, surface_flux_pd_f2)

  surface_flux_err <- quadrature_error(surface_flux_pd,diffusive_flux_err)



  surface_gradient <- gradient[1] - (((gradient[1] - gradient[2]) / (diffusive_flux_depth[1] - diffusive_flux_depth[2])) * diffusive_flux_depth[1])


  # Now when we have a negative surface flux, that means CO2 is moving out of the soil. If the flux is positive, the gradient method assumptions are violated, so we return NA

  if(surface_flux < 0) {

    out_values <- tibble(gradient = surface_gradient,
                         flux = -surface_flux,
                         ExpUncert = surface_flux_err,
                         diffusivity = diffusive_mean[1],
                         diffusivity_err = diffusive_mean_err[1],
                         zOffset = -zOffset_pos[1])


    return(out_values)
    #return(NA)

  } else {

    out_values <- tibble(gradient = NA,
                         flux = NA,
                         ExpUncert = NA,
                         diffusivity = NA,
                         diffusivity_err = NA,
                         zOffset = -zOffset_pos[1])

    return(out_values)
    #return(-surface_flux) # Make the surface flux positive for consistency * sigh * minus signs
  }



}




