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
#'   \item CVALR1: (?) unused calibration coefficient
#'   \item CVALR2: (?) unused calibration coefficient
#'   \item U_CVALA1: calibration coefficient for expanded uncertainty a
#'   \item U_CVALA2: calibration coefficient for expanded uncertainty b
#'   \item U_CVALA3: calibration coefficient for expanded uncertainty c
#'   \item CVALO1: categorical variable, only value "n"
#'   \item CVALS1: (?) start and ending date?
#'   \item CVALS2: (?) start and ending date?
#'   \item remarks: any comments about the site
#'   \item CVALC1: (?) processing date

#' }
#'
#' @docType data
#' @keywords datasets
#' @name neonBoulded60Cal
#' @format A data table with 1881 observations and 18 variables
#' @source <https://data.neonscience.org/data-products/DP1.00094.001> and <https://doi.org/10.6084/m9.figshare.24424762.v1>

"neonBoulded60Cal"
