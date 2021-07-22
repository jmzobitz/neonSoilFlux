#' @title Compute CO2 surface flux at a given NEON site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Computation function. Given a measurement of the co2 and diffusive flux at different levels, return the surface flux

#' @import dplyr

#' @param site_data Required. A data frame containing zOffsets, positions, megapit data, depths, measurement data (computed in interpolate

#' @return A value of the surface CO2 flux (umol m-2 s-1)

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation


neon_site_flux <- function(site_data,positions) {
  # First we need to filter the data at the layers where the CO2 measurements are made.

  filtered_site <- site_data %>%
    group_by(horizontalPosition,zOffset) %>%  # Group by horiz position & nest
    nest() %>%
    filter(horizontalPosition %in% positions$horizontalPosition &
             zOffset %in% positions$zOffset) %>%
    unnest(cols=c(data)) %>%  # Unnest & pivot
    pivot_wider(names_from="measurement",values_from = "value") %>% # Next compute diffusivity and co2
    mutate(diffus = diffusivity(temperature,soil_water,pressure,
                                coarseFrag2To5,coarseFrag5To20,
                                bulkDensExclCoarseFrag),
           co2mol = co2_to_umol(temperature,pressure,co2))




  # Nest the filtered data by horizontal position and time
  filtered_site_nest <- filtered_site %>%
    select(horizontalPosition,startDateTime,zOffset,diffus,co2mol) %>%
    group_by(horizontalPosition,startDateTime) %>%
    nest()

  # Compute the fluxes
  out_flux <- filtered_site_nest %>%
    mutate(flux = map(.x=data,.f=~compute_surface_flux(.x))) %>%
    select(-data) %>%
    unnest(cols=c(flux)) %>%
    ungroup()

  return(out_flux)


}

