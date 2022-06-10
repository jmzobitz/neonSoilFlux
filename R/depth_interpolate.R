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
#   2022-06-11: Modification to improve fitting
##############################################################################################


depth_interpolate <- function(input_measurements) {

  input_nest <- input_measurements %>%
    group_by(horizontalPosition, startDateTime,measurement) %>%
    nest() %>%
    pivot_wider(names_from = "measurement",values_from = "data") %>%
    pivot_longer(cols=c("temperature","soil_water"),names_to = "measurement",values_to="data")



  # Then apply the fitting function
  input_interp <- input_nest %>%
    mutate(interp = map2(.x=data,.y=co2,.f=~fit_function(.x,.y)))

  # Now we should save these out:
  input_interp_out <- input_interp %>%
    select(-data) %>%
    pivot_wider(names_from = "measurement",values_from = "interp") %>%
    pivot_longer(cols=c("temperature","soil_water","co2"),names_to = "measurement",values_to="data") %>%
    unnest(cols=c(data))

  return(input_interp_out)

}

