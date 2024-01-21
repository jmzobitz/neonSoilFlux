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
    nest() |>
    rename(position_info = data) |>
    mutate(n_levels = map_dbl(.x=position_info,.f=~nrow(.x)))  # Test to see if we have multiple measurement levels - then we need to check


  out_measurement <- input_measurement |>
    group_by(horizontalPosition,verticalPosition,startDateTime) |>
    nest() |>
    rename(measurement_info=data) |>
    inner_join(position_data,by=c("horizontalPosition","verticalPosition")) |>
    mutate(position_info = pmap(.l=list(n_levels,startDateTime,position_info),
                                .f=function(levels,startDateTime,position_info) (


                                  if(levels ==1) {
                                    return(position_info)
                                  } else {
                                    new_info <- position_info |>
                                      filter(startDateTime %within% measurement_interval)

                                    return(new_info)
                                  }
                                )
    )
    )  |>
    mutate(zOffset = map_dbl(.x=position_info,.f=~pluck(.x,"zOffset"))) |>
    select(-n_levels,-position_info) |>
    unnest(cols=c(measurement_info))

  return(out_measurement)

}
