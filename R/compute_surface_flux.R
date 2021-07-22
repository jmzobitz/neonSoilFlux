#' @title Compute CO2 surface flux

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Computation function. Given a measurement of the co2 and diffusive flux at different levels, return the surface flux

#' @import dplyr

#' @param input_data Required. A data frame containing zOffsets, diffusivity, and co2 (umol mol-1)

#' @return A value of the surface CO2 flux (umol m-2 s-1)

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation

compute_surface_flux <- function(input_data) {

  # Select the first three levels (closest to the surface)
  input_data_rev <- input_data %>%
    arrange(desc(zOffset)) %>%
    slice(1:3) %>%
    mutate(zOffset = abs(zOffset))

  # Calculate diffusivity of the first two levels as harmonic mean
  diffusive_mean <- c( (sum(input_data_rev$diffus[1:2]^(-1))/2)^(-1), (sum(input_data_rev$diffus[2:3]^(-1))/2)^(-1) )

  # Calculate diffusive flux between CO2 measurement levels (µmol m-2 s-1)
  diffusive_flux <- c((input_data_rev$co2mol[2] - input_data_rev$co2mol[1]) / (input_data_rev$zOffset[1] - input_data_rev$zOffset[2] ) * diffusive_mean[1],
                      (input_data_rev$co2mol[3] - input_data_rev$co2mol[2]) / (input_data_rev$zOffset[2] - input_data_rev$zOffset[3] ) * diffusive_mean[2] )

  # Determine the average depth (m) between measurement levels so that this can be assigned to the flux measurement
  diffusive_flux_depth <- c ( (((input_data_rev$zOffset[2]-input_data_rev$zOffset[1])/2)+input_data_rev$zOffset[1]), (((input_data_rev$zOffset[3]-input_data_rev$zOffset[2])/2)+input_data_rev$zOffset[2]) )


  # Now compute the fluxes, adding on the harmonic mean, fluxes, and depths
  flux_depth <- input_data_rev %>%
    slice(1:2) %>%
    mutate(diff_hm = diffusive_mean,
           flux = diffusive_flux,
           diff_depth = diffusive_flux_depth)






  # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).
  surface_flux <- flux_depth$flux[1] - (((flux_depth$flux[1] - flux_depth$flux[2]) / (abs(flux_depth$diff_depth[1]) - abs(flux_depth$diff_depth[2]))) * abs(flux_depth$diff_depth[1]))

  return(surface_flux)


}




