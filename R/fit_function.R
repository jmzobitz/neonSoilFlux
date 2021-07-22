#' @title Interpolate a soil measurement to different depths

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
#   John Zobitz (2021-07-15)
#     original creation
##############################################################################################


fit_function <- function(input_data,indep_name,dep_name,interp_values) {


  independent_data <- input_data %>%
    pull({{indep_name}})


  dependent_data <- input_data %>%
    pull({{dep_name}})


  # Make sure there are more than two data points to interpolate
  if(length(independent_data)>2 & sum(!is.na(dependent_data))>2) {
    # Just use linear interpolation here
    measurement_interp <- approx(x=independent_data, y=dependent_data, xout = interp_values, rule=2) %>%
      as_tibble() %>%
      rename("{{indep_name}}" := x,
             "{{dep_name}}" := y )



  } else {

    measurement_interp <- tibble(x=interp_values,y=NA) %>%
      rename("{{indep_name}}" := x,
             "{{dep_name}}" := y )
  }

  return(measurement_interp)
}

