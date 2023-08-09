#' @title Makes sure for each time, position, and depth we have at least two data points for soil temp and soil h20, 3 for soil co2

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
#' This as created to speed up data processing.

#' @param input_data Required. Nested data frame of merged soil water, temperature, co2, and pressure across different NEON locations and depths
#'
#' @example TBD
#'
#' @import dplyr

#' @return Data frame of fluxes from the timeperiod

# changelog and author contributions / copyrights
#   John Zobitz (2022-06-11)
#     original creation



measurement_detect <- function(input_data) {


  # Do a single pass across each of the measurements to see if we have all of the measurements (FinalQF) and if there are no NA values for interpolation

  input_data_rev <- input_data |>
    mutate(data = map(.x=data,.f=~filter(.x,if_any(ends_with("FinalQF"), ~ (.x  == 0))))) |>
    mutate(data = map(.x=data,.f=~filter(.x,if_any(!matches(c("domainID","siteID","horizontalPosition","verticalPosition")) | !ends_with("FinalQF"), ~ (!is.na(.x))))))

# Each column selector, time, and horizontal position needs at least three measurements, then we should also do the same for the pressure measurements by time point
    # filter out co2 data with more than 3 measurements at a given timepoint

  input_data_interp <- input_data_rev |>
    mutate(data = map2(.x=data,.y=measurement,.f=~(
      if(.y=="soilCO2concentration") {
        .x |> group_by(horizontalPosition, startDateTime) %>%
          nest() |>
          mutate(tot = map_dbl(data, nrow)) |>
          filter(tot>2) |>
          unnest(cols=c("data"))

      } else if(.y %in% c("VSWC", "soilTemp") ) {
        .x |> group_by(horizontalPosition, startDateTime) %>%
          nest() |>
          mutate(tot = map_dbl(data, nrow)) |>
          filter(tot>=2) |>
          unnest(cols=c("data"))

      } else { .x }  # This is the pressure column
    )
    ))

    # Now we need to see (for the first three measurements), how many we have at a given time point

  have_three_measurement <- input_data_interp |>
    filter(measurement !="staPres") |>
    unnest(cols=c("data")) %>%
    group_by(horizontalPosition, startDateTime) %>%
    nest() %>%
    mutate(tot_meas = map_dbl(.x=data, .f=~(length(unique(.x$measurement))) )) %>%
    filter(tot_meas == 3) %>%  # Keep only when we have co2, temp, and water
    select(horizontalPosition,startDateTime)

  # Next we go down and do filtering on across the data ready for interpolation - semi join!

  input_data_interp_ready <- input_data_interp |>
    mutate(data = map2(.x=data,.y=measurement,.f=~(
      if(.y!="staPres") {
        .x |> semi_join(have_three_measurement,by=c("horizontalPosition","startDateTime"))

      } else { ## For the column with pressure, we just want a starting time.
        .x |> semi_join(have_three_measurement,by=c("startDateTime"))
      }
    )
    )) |>
    mutate(n_obs = map2_dbl(.x=data,.y=measurement,.f=~(
      if(.y!="staPres") {
        .x |> group_by(horizontalPosition,startDateTime) |> nrow()

      } else { ## For the column with pressure, we just want a starting time.
        .x |> group_by(startDateTime) |> nrow()
      }
    )
    ))




  return( input_data_interp_ready )
}

