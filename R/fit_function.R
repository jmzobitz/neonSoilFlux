#' @title Interpolate a soil measurement to different depths

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_data Required. Vector of NEON data and measurements from the soil (soil temperature and soil water)
#' @param interp_values Depths of the soil co2 sensors

#' @return A data frame of the depth and the measured column for the measurements

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none
#' @import dplyr

#' @examples TBD


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-15)
#     original creation
#   2022-06-11: revision to include spline fits of temperature and water,
# better functionality
##############################################################################################


fit_function <- function(input_data,co2_values) {

  # Data is a data frame of soil h20 and soil temperature, interp values from from co2
 interp_values <- co2_values$zOffset
  # Define top layer for interpolation
  if(max(input_data$zOffset) > -0.05){
    from_depth <- 0 # Extrapolate to soil surface is sensor is less than 5 cm from soil surface
  }else{
    from_depth <- round(max(input_data$zOffset), digits=2)
  }


  # Define bottom layer for interpolation
  if(min(input_data$zOffset) < -0.2){
    to_depth <- floor(min(input_data$zOffset)*10) / 10 # Extrapolate to nearest 10 cm increment below deepest sensor if that sensor is below 20 cm (i.e., not experiencing very high diurnal variability)
  }else{
    to_depth <- round(min(input_data$zOffset), digits=2)
  }

  # Create sequence of depths in 1 cm intervals up to the depth of the deepest sensor producing good data
  depths <- seq(from=from_depth, to=to_depth, by=-0.01)

  independent_data <- input_data$zOffset
  dependent_data <- input_data$value


  # Make sure there are more than two data points to interpolate
  if(length(independent_data) >3) {

    measurement_spline <- smooth.spline(x = independent_data,y= dependent_data)
    # Predict
    measurement_interp <- predict(measurement_spline, x=interp_values) %>%
      as_tibble() %>%
      rename(zOffset = x,
             value = y )

  } else {


    # Just use linear interpolation here
    measurement_interp <- approx(x=independent_data, y=dependent_data, xout = interp_values, rule=2) %>%
      as_tibble() %>%
      rename(zOffset = x,
             value = y )

  }


  # Define the output data frame, do a join so this makes sense:
  measurement_out <- co2_values %>%
    select(-value) %>%
    inner_join(measurement_interp,by="zOffset")

  return(measurement_out)
}

