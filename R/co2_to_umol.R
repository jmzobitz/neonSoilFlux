#' @title Convert co2 concentration from ppm to µmol m-3 units

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a measurement of co2, convert it from ppm to umol m-3 based on temperature and pressure. Also compute associated error via quadrature.


#' @param temperature Required. Soil temperature (degrees C)
#' @param pressure Required. Barometric air pressure (kilopascal)
#' @param co2 Carbon dioxide in ppm
#' @param temperature_err Required. Reported Soil temperature error (degrees C)
#' @param pressure_err Required. Reported Barometric air pressure error (kilopascal)
#' @param co2_err Required. Carbon dioxide in ppm error
#' @param zOffset Required. Surface depth (m). Reported as a negative number.

#' @return A value of the converted co2
#' @examples co2_to_umol(31,96.3,654,.15,.05,9,-.05)

#' @export




co2_to_umol <- function(temperature, pressure, co2, temperature_err, pressure_err, co2_err, zOffset) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2021-07-21)
  #     original creation
  #   2023-07-18: Included experimental uncertainty in the mix.
  #     2024-04-08: update to get namespaces correct
  #   2024-04-15: update to fix error in calculation of partial derivative of pressure (from Yang song)


  # Since pressure is a single value, make it equal to all the other lengths
  pressure <- rep(pressure, length(temperature))
  pressure_err <- rep(pressure_err, length(temperature_err))

  # Assign values to constants
  R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
  absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)


  co2_convert <- (co2 * pressure) / (R * (temperature - absZero))

  # Vector of partial derivatives (temp, pressure, co2)
  pd_errs <- c(-(co2 * pressure) / (R * (temperature - absZero)^2),
               (co2) / (R * (temperature - absZero)),
               (pressure) / (R * (temperature - absZero))
               )

  errs <- c(temperature_err, pressure_err, co2_err)

  calc_err <- quadrature_error(pd_errs, errs)

  out_tibble <- tibble::tibble(zOffset = zOffset,
                               co2_umol = co2_convert,
                               co2ExpUncert = calc_err)
  return(out_tibble)
}
