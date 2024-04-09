#' @title Return NEON environmental data

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given a site filename (from acquire_neon_data), process and compute fluxes.
#' This file takes a saved data file from acquire:
#' 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame
#' 2) Interpolates across the measurements
#' 3) Merges air pressure data into this data frame
#' 4) Does a final QF check so we should have only timeperiods where all measurements exist
#' 5) Adds in the megapit data so we have bulk density, porosity measurements at the interpolated depth.
#' 6) Saves the data

#' @param input_file_name Required. Path of location for the save file from acquire_neon_data. Must end in .Rda (a string)
#' @param save_file Optional. Logical. Do we save the file or return it to console?
#' @param output_file_name Optional. Path of location for file to save when save_file is TRUE. Must end in .Rda (a string)
#'
#' @example extract_env_data(input_file_name = "my-file.Rda",save_file = TRUE,output_file_name = "env-my-file.Rda")
#'


#' @return Data frame of environmental measurements for flux computation

# changelog and author contributions / copyrights
#   John Zobitz (2024-03-30)
#     2024-04-08: update to get namespaces correct





extract_env_data <- function(input_file_name,
                             save_file = FALSE,
                             output_file_name = NULL) {

  load(input_file_name)

  ################

  ################
  # 2) Interpolates across the measurements
  site_data2 <- site_data |>
    dplyr::mutate(data = purrr::pmap(.l=list(data,monthly_mean,measurement),.f=~insert_mean(..1,..2,..3)))

  # Filters out measurements that don't have enough QF flags
  site_filtered <- measurement_detect(site_data2)

  # Exit gracefully if no values get returned
  if(any(site_filtered$n_obs ==0)) {
    msg = paste0("No valid environmental timeperiod measurements for ", input_file_name)
    stop(msg)
  }



  # Interpolate all the measurements together in one nested function
  # We *need* to interpolate the errors - assume the errors interpolate as well?

  site_interp <- depth_interpolate(
    input_measurements = site_filtered,
    measurement_name = c("VSWC","soilTemp"),  # Measurements we are interpolating
    measurement_interpolate = "soilCO2concentration")  # We always want to interpolate to this depth

  # Add in the pressure measurements
  pressure_measurement <- site_filtered |>
    dplyr::filter(measurement =="staPres") |>
    dplyr::select(-monthly_mean) |>
    tidyr::unnest(cols=c("data")) |>
    dplyr::group_by(startDateTime) |>
    tidyr::nest() |>
    dplyr::rename(press_data = data)


  ### Then take each of the measurements to associate them with errors
  all_measures <- site_interp |>
    dplyr::inner_join(pressure_measurement, by=c("startDateTime")) |>
    dplyr::mutate(staPresMeanQF = purrr::map_int(.x=press_data,.f=~dplyr::pull(.x,staPresFinalQF))) |>
    dplyr::relocate(press_data,.after="env_data")

  if(save_file) {
    save(all_measures,file=output_file_name)
  } else{
    return(all_measures)
  }

}
