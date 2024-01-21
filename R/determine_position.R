#' @title Determine the depth of a measurement

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a NEON measurement data frame and measurement depth, pull out the measurement depth for a measurement - because it may vary in time based on if a sensor is replaced.
#'
#' @param input_positions Required. Input vector of measurements depths
#' @param input_measurement Required. Input vector of measurements.

#' @return A data frame that reports the measurement depth for and associated environmental measurement.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples
#' # Currently none


#' @import dplyr
#' @import purrr
#' @import lubridate


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2024-01-17)
#     original creation



determine_position <- function(input_positions,input_measurement) {

  position_data <- input_positions |>
    mutate(across(.cols=positionStartDateTime:positionEndDateTime,.fns = ~as.POSIXct(.x,
                                                                                     format="%Y-%m-%dT%H:%M:%S", #format time
                                                                                     tz = "UTC"
    ))) |>
    mutate(across(.cols=positionStartDateTime:positionEndDateTime,.fns=~if_else(is.na(.x),Sys.time(),.x))) |>
    mutate(measurement_interval = lubridate::interval(positionStartDateTime,positionEndDateTime)) |>
    separate_wider_delim(cols=HOR.VER,names=c("horizontalPosition","verticalPosition"),delim=".") |>
    group_by(horizontalPosition,verticalPosition) |>
    nest()


  out_measurement <- input_measurement |>
    inner_join(position_data,by=c("horizontalPosition","verticalPosition")) |>
    mutate(data = map2(.x=startDateTime,.y=data,.f=~(.y |> mutate(which_interval = .x %within% measurement_interval) |>
                                                       filter(which_interval)))) |>
    mutate(zOffset = map_dbl(.x=data,.f=~pluck(.x,"zOffset"))) |>
    select(-data)


  return(out_measurement)

}
