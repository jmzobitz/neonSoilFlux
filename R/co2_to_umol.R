#' @title Convert co2 concentration from ppm to µmol m-3 units

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a measurement of co2, convert it from ppm to umol m-3 based on temperature and pressure


#' @param temperature Required. Soil temperature (degrees C)
#' @param pressure Required. Barometric air pressure (kilopascal)
#' @param co2 Carbon dioxide in ppm
#' @param temperature_err Required. Reported Soil temperature error (degrees C)
#' @param pressure Required. Reported Barometric air pressure error (kilopascal)
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
#   2023-07-18: Included experimental uncertainty in the mix.


co2_to_umol <- function(temperature,pressure,co2,temperature_err,pressure_err,co2_err,zOffset) {

  # Assign values to constants
  R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
  absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)


  co2_convert <- (co2 * pressure) / (R * (temperature - absZero) )

  # Vector of partial derivatives (temp, pressure, co2)
  pd_errs <- c( -(co2 * pressure) / (R * (temperature - absZero)^2 ), (pressure) / (R * (temperature - absZero) ), (pressure) / (R * (temperature - absZero) ) )

  errs <- c(temperature_err,pressure_err,co2_err)

  calc_err <- quadrature_error(pd_errs,errs)

  out_tibble <- tibble(zOffset = zOffset,co2_umol = co2_convert,co2ExpUncert = calc_err)
  return(out_tibble)




}

