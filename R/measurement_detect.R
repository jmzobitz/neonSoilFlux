#' @title Internal function that makes sure for each time, position, and depth we have at least two data points for soil temp and soil h20, 3 for soil co2

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Given a merged data frame of co2, water, and temperature:
#' 1) Filters on QF measurement flags
#' 2) Filters if we have at least 2 soil h20 and temperature measurements, 3 co2 measurements at each time, horizontal position, and vertical depth
#' 3) Filters if we have at least 3 distinct measurements at each time and horizontal position
#' 4) Returns the resulting data frame.
#'
#' This internal function is created to speed up data processing.

#' @param input_data Required. Nested data frame of merged soil water, temperature, co2, and pressure across different NEON locations and depths
#'

#' @return Data frame of fluxes from the timeperiod


measurement_detect <- function(input_data) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2022-06-11)
  #     original creation
  #   John Zobitz (2023-10-23)
  #     modified to apply drop_na rather in map rather than doing a filter.
  #     drop_na() checks for complete rows.
  #     2024-04-08: update to get namespaces correct




  .data = NULL  # Appease R CMD Check

  # Do a single pass across each of the measurements to see if we have all of the measurements (MeanQF) and if there are no NA values for interpolation

  # if the monthly mean is a NA, then we use a 2
  # meanQF = 0 --> no smoothed mean
  # meanQF = 1 --> smoothed mean used
  # meanQF = 2 --> NA flag, so we can't use measurement


  input_data_rev <- input_data |>
    dplyr::mutate(data = purrr::map(.x = .data[["data"]], .f = ~ dplyr::filter(.x, if_any(ends_with("FinalQF"), ~ (.x != 2))) |>
      tidyr::drop_na()))

  # Each column selector, time, and horizontal position needs at least three measurements, then we should also do the same for the pressure measurements by time point
  # filter out co2 data with more than 3 measurements at a given timepoint

  input_data_interp <- input_data_rev |>
    dplyr::mutate(data = purrr::map2(.x = .data[["data"]], .y =.data[["measurement"]], .f = ~ (
      if (.y == "soilCO2concentration") {
        .x |>
          dplyr::group_by(.data[["horizontalPosition"]], .data[["startDateTime"]]) |>
          tidyr::nest() |>
          dplyr::mutate(tot = purrr::map_dbl(.data[["data"]], nrow)) |>
          dplyr::filter(tot > 2) |>
          tidyr::unnest(cols = c("data"))
      } else if (.y %in% c("VSWC", "soilTemp")) {
        .x |>
          dplyr::group_by(.data[["horizontalPosition"]],.data[["startDateTime"]]) |>
          tidyr::nest() |>
          dplyr::mutate(tot = purrr::map_dbl(.data[["data"]], nrow)) |>
          dplyr::filter(tot >= 2) |>
          tidyr::unnest(cols = c("data"))
      } else {
        .x
      } # This is the pressure column
    )))

  # Now we need to see (for the first three measurements), how many we have at a given time point

  have_three_measurement <- input_data_interp |>
    dplyr::filter(.data[["measurement"]] != "staPres") |>
    tidyr::unnest(cols = c("data")) |>
    dplyr::group_by(.data[["horizontalPosition"]], .data[["startDateTime"]]) |>
    tidyr::nest() |>
    dplyr::mutate(tot_meas = purrr::map_dbl(.x = .data[["data"]], .f = ~ (length(unique(.x$measurement))))) |>
    dplyr::filter(.data[["tot_meas"]] == 3) |> # Keep only when we have co2, temp, and water
    dplyr::select(.data[["horizontalPosition"]], .data[["startDateTime"]])

  # Next we go down and do filtering on across the data ready for interpolation - semi join!

  input_data_interp_ready <- input_data_interp |>
    dplyr::mutate(data = purrr::map2(.x = .data[["data"]], .y =.data[["measurement"]], .f = ~ (
      if (.y != "staPres") {
        .x |> dplyr::semi_join(have_three_measurement, by = c("horizontalPosition", "startDateTime"))
      } else { ## For the column with pressure, we just want a starting time.
        .x |> dplyr::semi_join(have_three_measurement, by = c("startDateTime"))
      }))) |>
    dplyr::mutate(n_obs = purrr::map2_dbl(.x = .data[["data"]], .y = .data[["measurement"]], .f = ~ (
      if (.y != "staPres") {
        .x |>
          dplyr::group_by(.data[["horizontalPosition"]], .data[["startDateTime"]]) |>
          nrow()
      } else { ## For the column with pressure, we just want a starting time.
        .x |>
          dplyr::group_by(startDateTime) |>
          nrow()
      })))




  return(input_data_interp_ready)
}
