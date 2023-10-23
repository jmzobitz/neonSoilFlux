#' @title Save the location of sensor positions for a given data product

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Definition function. Given a measurement return the necessary components we need to compute flux calculations (and nothing more). Currently only works with 30 minute data


#' @param neon_data Required. A list of NEON data downloaded from the utilities
#' @param data_code Required. Names of data product we are interpolating. (SWS = soil water, ST = soil temperature, SCO2C = soil CO2)
#' @param data_product_id Name of the data product 00094 = soil water, 00041 = soil temperature, 00095 = soil CO2
#'
#' @return A data frame of the requested data.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples sensor_positions(sjer_co2,"SCO2C","00095")
#' @import dplyr

#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation
##############################################################################################

sensor_positions <- function(neon_data,data_code,data_product_id,save_filename) {

  # Determine a data frame of the different horizontal and vertial positions
  positions_name <- paste0("sensor_positions_",data_product_id)
  positions <- neon_data[[positions_name]] %>%
    separate(HOR.VER,into=c("HOR","VER")) %>%
    select(siteID,HOR,VER,zOffset) %>%
    rename(horizontalPosition = HOR,
           verticalPosition = VER)

  return(positions)

}


