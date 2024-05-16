#' @title Return NEON environmental data

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' This file takes data frame from acquire_neon_data and:
#' 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame
#' 2) Interpolates across the measurements
#' 3) Merges air pressure data into this data frame


#' @param input_data Required. Nested data frame from acquire_neon_data.
#'
#' @example correct_env_data(site_data)
#'


#' @return List of all QF flags over time period and Data frame of environmental measurements for flux computation

# changelog and author contributions / copyrights
#   John Zobitz (2024-05-07)
#     original creation





correct_env_data <- function(input_data) {

  ################

  ################
  # 2) Interpolates across the measurements
  site_data2 <- input_data |>
    dplyr::mutate(data = purrr::pmap(.l=list(data,monthly_mean,measurement),.f=~insert_mean(..1,..2,..3)))

  site_data3 <- site_data2 |>
    dplyr::mutate(qf_check = purrr::map2(measurement,data,check_qf_flags)) |>
    dplyr::select(measurement,qf_check) |>
    tidyr::pivot_wider(names_from = "measurement",values_from = "qf_check")

  # Define an output vector that we will then join to at the end of the different flags
  qf_flags <- site_data3$soilCO2concentration[[1]] |>
    dplyr::inner_join(site_data3$VSWC[[1]],by=c("startDateTime","horizontalPosition")) |>
    dplyr::inner_join(site_data3$soilTemp[[1]],by=c("startDateTime","horizontalPosition")) |>
    dplyr::inner_join(dplyr::select(site_data3$staPres[[1]],-horizontalPosition),by=c("startDateTime"))

  # Remove values where we don't have measurements - we will use this for joining
  qf_flags_small <- qf_flags |>
    dplyr::filter(dplyr::if_all(.cols=tidyselect::ends_with("MeanQF"),.fns=~(.x!=2)) ) |>
    dplyr::select(startDateTime,horizontalPosition)

  site_filtered <- site_data2 |>
    dplyr::mutate(data = purrr::map2(.x=data,.y=measurement,.f=~(

      if(.y != "staPres") {
        dplyr::semi_join(.x,qf_flags_small,by=c("startDateTime","horizontalPosition"))
      } else {
        dplyr::semi_join(.x,qf_flags_small,by=c("startDateTime"))
      }


      )
                                    )
                  )




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
    dplyr::group_by(startDateTime,horizontalPosition) |>
    tidyr::nest() |>
    dplyr::rename(press_data = data) |>
    dplyr::ungroup()  |>
    dplyr::select(-horizontalPosition) # This isn't needed for pressure (only one measurement at a site)



  ### Then take each of the measurements to associate them with errors
  all_measures <- site_interp |>
    dplyr::inner_join(pressure_measurement, by=c("startDateTime")) |>
    dplyr::mutate(staPresMeanQF = purrr::map_int(.x=press_data,.f=~dplyr::pull(.x,staPresFinalQF))) |>
    dplyr::relocate(press_data,.after="env_data") |>
    dplyr::select(horizontalPosition,startDateTime,env_data,press_data)

    return(list(all_flags = qf_flags,site_filtered = all_measures) )


}