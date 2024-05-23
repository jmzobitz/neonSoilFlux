#' Measured environmental data at a NEON site
#'
#' A nested dataset containing environmental variables at the SJER site from June 2022. A convenience data frame to make processing fluxes easier for testing. Computed with the function acquire_neon_data.  All the variables can be used to compute fluxes (along with megapit data).
#'
#' \itemize{
#'   \item measurement: Name of environmental measurement
#'   \item data: Nested list of data corresponding to a measurement
#'   \item monthly_mean: Monthly mean of measurement, computed in `compute_monthly_mean`
#' }
#'
#' @docType data
#' @keywords datasets
#' @name sjer_env_data_2022_06
#' @usage data(sjer_env_data_2022_06)
#' @format A nested 3 item list
NULL
