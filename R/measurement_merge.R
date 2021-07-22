#' @title Acquire data from a NEON site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Definition function. Given a measurement return the necessary components we need to compute flux calculations (and nothing more). Currently only works with 30 minute data


#' @param neon_data Required. A list of NEON data downloaded from the utilities
#' @param data_code Required. Names of data product we are interpolating. (SWS = soil water, ST = soil temperature, SCO2C = soil CO2)
#' @param data_product_id Name of the data product 00094 = soil water, 00041 = soil temperature, 00095 = soil CO2
#' @param measurement_name Required. Names of column we are grabbing. (VSWCMean = soil water, soilTempMean = soil temperature, soilCO2concentrationMean = soil CO2)
#' @param qf_name Required. Names of qf column we are grabbing. (VSWCFinalQF = soil water, finalQF = soil temperature, finalQF = soil CO2)
#'
#' @return A data frame of the requested data.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples
#' @import dplyr

#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-19)
#     original creation
##############################################################################################

measurement_merge <- function(neon_data,data_code,data_product_id,measurement_name,qf_name) {

  # Create data frame for soil temperature data after interpolating across depths with columns from 0 m to 2 m


  # Determine a data frame of the different horizontal and vertial positions
  positions_name <- paste0("sensor_positions_",data_product_id)
  positions <- neon_data[[positions_name]] %>%
    separate(HOR.VER,into=c("HOR","VER")) %>%
    select(siteID,HOR,VER,zOffset)


  # Identify good (i.e., unflagged) soil temperature data, join the positions, and group by and nest

  data_name <- paste0(data_code,"_30_minute")

  good_data <- neon_data[[data_name]] %>%
    select(c("domainID","siteID","horizontalPosition","verticalPosition","startDateTime","endDateTime") | matches(paste0("^",measurement_name,"$")) | matches(paste0("^",qf_name,"$")) ) %>%
    left_join(positions,by=c("siteID"="siteID","horizontalPosition"="HOR","verticalPosition"="VER")) %>%

  return(good_data)

}


