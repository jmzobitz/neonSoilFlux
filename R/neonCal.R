#' Soil water content calibrations for belowground sensor data
#'
#' A datatble of SWC corrections to remove step changes in soil water content measureing depths
#'
#' \itemize{
#'   \item domain: NEON sampling domain
#'   \item site: NEON site
#'   \item plot: Which particular NEON plot at the site
#'   \item measurementLevel: Which particular measurement depth at that plot
#'   \item sensorDepths: Corrected depth of sensor measurement
#'   \item CVALA1: calibration coefficient a
#'   \item CVALA2: calibration coefficient b
#'   \item CVALA3: calibration coefficient c
#'   \item CVALR1: revision control
#'   \item CVALR2: failed file
#'   \item U_CVALA1: soil moisture combined uncertainty
#'   \item U_CVALA2: soil moisture repeatability
#'   \item U_CVALA3: soil moisture combined uncertainty (truth and trueness only)
#'   \item CVALO1: coefficients manually overridden (Y/N)
#'   \item CVALS1: start date for the soil specific calibration
#'   \item CVALS2: end date for the soil specific calibration, NA if no end date is defined
#'   \item remarks: any comments about the soil specific calibration information
#'   \item CVALC1: information build date

#' }
#'
#' @docType data
#' @keywords datasets
#' @name neonCal
#' @format A data table with 1881 observations and 18 variables
#' @source <https://data.neonscience.org/data-products/DP1.00094.001> and <https://doi.org/10.6084/m9.figshare.24424762.v1>

"neonCal"
