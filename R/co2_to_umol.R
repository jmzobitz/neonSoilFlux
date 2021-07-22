#' @title Convert co2 concentration from ppm to µmol m-3 units

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a measurement of co2, convert it from ppm to umol m-3 based on temperature and pressure


#' @param temperature Required. Soil temperature (degrees C)
#' @param pressure Required. Barometric air pressure ?
#' @param co2 Carbon dioxide in ppm

#' @return A value of the converted co2

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples TBD
#'

#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation


co2_to_umol <- function(temperature,pressure,co2) {

  # Assign values to constants
  R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
  absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)


  co2_convert <- (co2 * pressure) / (R * (temperature - absZero) )



  return(co2_convert)




}

