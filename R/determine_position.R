#' @title Determine the depth of a measurement

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a NEON measurement data frame and measurement depth, pull out the measurement depth for a measurement - because it may vary in time based on if a sensor is replaced.
#'
#' @param input_positions Required. Input vector of measurements depths
#' @param input_measurement Required. Input vector of measurements.

#' @return A data frame that reports the measurement depth for and associated environmental measurement.

#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr



#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2024-01-17)
#     original creation



determine_position <- function(input_positions, input_measurement) {
  position_data <- input_positions |>
    dplyr::mutate(dplyr::across(.cols = positionStartDateTime:positionEndDateTime, .fns = ~ as.POSIXct(.x,
      format = "%Y-%m-%dT%H:%M:%S", # format time
      tz = "UTC"
    ))) |>
    dplyr::mutate(dplyr::across(.cols = positionStartDateTime:positionEndDateTime, .fns = ~ dplyr::if_else(is.na(.x), Sys.time(), .x))) |>
    dplyr::mutate(measurement_interval = lubridate::interval(positionStartDateTime, positionEndDateTime)) |>
    tidyr::separate_wider_delim(cols = HOR.VER, names = c("horizontalPosition", "verticalPosition"), delim = ".") |>
    dplyr::group_by(horizontalPosition, verticalPosition) |>
    tidyr::nest() |>
    dplyr::rename(position_info = data) |>
    dplyr::mutate(n_levels = map_dbl(.x = position_info, .f = ~ nrow(.x))) # Test to see if we have multiple measurement levels - then we need to check


  out_measurement <- input_measurement |>
    dplyr::group_by(horizontalPosition, verticalPosition, startDateTime) |>
    tidyr::nest() |>
    dplyr::rename(measurement_info = data) |>
    dplyr::inner_join(position_data, by = c("horizontalPosition", "verticalPosition")) |>
    dplyr::mutate(position_info = purrr::pmap(
      .l = list(n_levels, startDateTime, position_info),
      .f = function(levels, startDateTime, position_info) {
        (


          if (levels == 1) {
            return(position_info)
          } else {
            new_info <- position_info |>
              dplyr::filter(startDateTime %within% measurement_interval)

            return(new_info)
          })
      }
    )) |>
    dplyr::mutate(zOffset = purrr::map_dbl(.x = position_info, .f = ~ purrr::pluck(.x, "zOffset"))) |>
    dplyr::select(-n_levels, -position_info) |>
    tidyr::unnest(cols = c(measurement_info))

  return(out_measurement)
}
