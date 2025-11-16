#' @title Internal function that prepares downloaded NEON data for flux processing

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' This file takes data frame from acquire_neon_data and:
#' 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame
#' 2) Interpolates across the measurements
#' 3) Merges air pressure data into this data frame


#' @param input_data Required. Nested data frame from acquire_neon_data.
#'
#' @examples
#' \donttest{
#' # Note: you may need to first aqcuire the NEON data using acquire_neon_data
#' # Now correct existing environmental data:
#' corrected_data <- correct_env_data(sjer_env_data_2022_06)
#' }



#' @return List of all QF flags over time period and Data frame of environmental measurements for flux computation



correct_env_data <- function(input_data) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2024-05-07)
  #     original creation
  #   2024-11-24: Update to improve efficiencies in interpolation


  .data = NULL  # Appease R CMD Check
  ################

  ################
  # 2) Interpolates across the measurements
  site_data2 <- input_data |>
    dplyr::mutate(data = purrr::pmap(.l=list(.data[["data"]],.data[["monthly_mean"]],.data[["measurement"]]),.f=~insert_mean(..1,..2,..3)))

  site_data3 <- site_data2 |>
    dplyr::mutate(qf_check = purrr::map2(.data[["measurement"]],.data[["data"]],check_qf_flags)) |>
    dplyr::select(tidyselect::all_of(c("measurement","qf_check"))) |>
    tidyr::pivot_wider(names_from = "measurement",values_from = "qf_check")

  # Define an output vector that we will then join to at the end of the different flags
  qf_flags <- site_data3$soilCO2concentration[[1]] |>
    dplyr::inner_join(site_data3$VSWC[[1]],by=c("startDateTime","horizontalPosition")) |>
    dplyr::inner_join(site_data3$soilTemp[[1]],by=c("startDateTime","horizontalPosition")) |>
    dplyr::inner_join(dplyr::select(site_data3$staPres[[1]],-.data[["horizontalPosition"]]),by=c("startDateTime"))

  # Remove values where we don't have measurements - we will use this for joining
  qf_flags_small <- qf_flags |>
    dplyr::filter(dplyr::if_all(.cols=tidyselect::ends_with("MeanQF"),.fns=~(.x!=2)) ) |>
    dplyr::select(.data[["startDateTime"]],.data[["horizontalPosition"]])

  site_filtered <- site_data2 |>
    dplyr::mutate(data = purrr::map2(.x=.data[["data"]],.y=.data[["measurement"]],.f=~(

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

  # Now we want to join this all up
  co2_measurement <- site_interp |>
    dplyr::filter(.data[["measurement"]] =="soilCO2concentration") |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    tidyr::unnest(cols=c("data")) |>
    select(-.data[["measurement"]])

  # Now we want to join this all up
  VSWC_measurement <- site_interp |>
    dplyr::filter(.data[["measurement"]] =="VSWC") |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    tidyr::unnest(cols=c("data")) |>
    select(-.data[["measurement"]])

  # Now we want to join this all up
  soilTemp_measurement <- site_interp |>
    dplyr::filter(.data[["measurement"]] =="soilTemp") |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    tidyr::unnest(cols=c("data")) |>
    select(-.data[["measurement"]])

  env_data_all <- co2_measurement |>
    inner_join(VSWC_measurement,by=c("horizontalPosition","startDateTime","zOffset")) |>
    inner_join(soilTemp_measurement,by=c("horizontalPosition","startDateTime","zOffset")) |>
    dplyr::group_by(.data[["startDateTime"]],.data[["horizontalPosition"]]) |>
    tidyr::nest() |>
    dplyr::rename(env_data = .data[["data"]])

  # Add in the pressure measurements
  pressure_measurement <- site_interp |>
    dplyr::filter(.data[["measurement"]] =="staPres") |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    tidyr::unnest(cols=c("data")) |>
    select(-.data[["measurement"]]) |>
    dplyr::group_by(.data[["startDateTime"]],.data[["horizontalPosition"]]) |>
    tidyr::nest() |>
    dplyr::rename(press_data = .data[["data"]]) |>
    dplyr::ungroup()  |>
    dplyr::select(-.data[["horizontalPosition"]]) # This isn't needed for pressure (only one measurement at a site)



  ### Then take each of the measurements to associate them with errors
  all_measures <- env_data_all |>
    dplyr::inner_join(pressure_measurement, by=c("startDateTime"))



    return(list(all_flags = qf_flags,site_filtered = all_measures) )


}
