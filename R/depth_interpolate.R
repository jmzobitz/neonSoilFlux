#' @title Interpolate different depth measurements

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_data Required. Vector of NEON data and measurements from the soil
#' @param col_name Required. Names of column we are interpolating. Currently only does one column at a time.

#' @return A data frame of the depth and the measured column

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none
#' @import dplyr


#' @examples


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-20)
#     original creation
#   2022-06-06: Modification of the interpolation to accelerate computation
##############################################################################################


depth_interpolate <- function(input_measurements) {



  # Make a data frame of the sensor depths
  sensor_depths <- input_measurements %>%
    select(zOffset,measurement) %>%
    group_by(measurement) %>%
    summarize(min = min(zOffset),
              max = max(zOffset))



  # Define top layer for interpolation
  if(max(sensor_depths$max) > -0.05){
    from_depth <- 0 # Extrapolate to soil surface is sensor is less than 5 cm from soil surface
  }else{
    from_depth <- round(max(sensor_depths$max), digits=2)
  }


  # Define bottom layer for interpolation
  if(max(sensor_depths$min) < -0.2){
    to_depth <- ceiling(max(sensor_depths$min)*10) / 10 # Extrapolate to nearest 10 cm increment below deepest sensor if that sensor is below 20 cm (i.e., not experiencing very high diurnal variability)
  }else{
    to_depth <- round(max(sensor_depths$min), digits=2)
  }

  # Next filter our measurements so that we take these only to save processing time
  input_revised <- input_measurements %>%
    filter(between(zOffset,to_depth,from_depth))

  # Create sequence of depths in 1 cm intervals up to the depth of the deepest sensor producing good data
  depths <- seq(from=from_depth, to=to_depth, by=-0.01)

  # Internal function that ensures we have more than 2 observations for each measurement type

  measurement_detect <- function(input_data) {

    # Can we interpolate data?
    have_interpolate <- input_data %>%
      group_by(measurement) %>%
      summarize(tot=n()) %>%
      pull(tot) %>%
      all(.>2)

    # Do we have three different measurements (co2, pressure, temp) to compute flux?
    have_measurement <- input_data %>%
      pull(measurement) %>%
      n_distinct(.)

    return(have_interpolate & (have_measurement == 3) )
  }


  input_nest_try <- input_revised %>%
    filter(finalQF ==0) %>%  # Remove anywhere the QF flag is bad
    group_by(horizontalPosition,startDateTime) %>%
    nest() %>%  # Nest each measurement together
    mutate(can_flux = map(.x=data,.f=~measurement_detect(.x)) ) %>% # Can we compute the flux?
    unnest(cols=c(can_flux)) %>%
    filter(can_flux)   # Only select places where computing the flux is possible

  if (nrow(input_nest_try) == 0) {
    stop("There are no valid timeperiods during this month.")
  }
  input_nest <- input_nest_try %>%
    unnest(cols=c(data)) %>%
    select(-can_flux) %>%
    group_by(measurement,horizontalPosition,startDateTime) %>%
    nest()

  # Then apply the fitting function
  input_interp <- input_nest %>%
    mutate(interp = map(.x=data,.f=~fit_function(.x,zOffset,value,depths)))

  # Now we should save these out:
  input_interp_out <- input_interp %>%
    select(-data) %>%
    unnest(cols=c(interp))

  return(input_interp_out)

}

