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

  # Filter out the bad measurements and then nest them together so we can interpolate
  input_nest <- input_revised %>%
    filter(finalQF ==0) %>%
    group_by(measurement,horizontalPosition,startDateTime) %>%
    nest()

  # Interpolate the values
  input_interp <- input_nest %>%
    mutate(interp = map(.x=data,.f=~fit_function(.x,zOffset,value,depths)))


  # Now we should save these out:
  input_interp_out <- input_interp %>%
    select(-data) %>%
    unnest(cols=c(interp))

  return(input_interp_out)

}

