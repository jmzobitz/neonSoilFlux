#' Computes Fick's law of diffusion between two values
#'
#' @param depths Required. Depths that the compute the flux (2 values)
#' @param co2 Required. CO2 molar concentration (2 values, corresponding to depths)
#' @param co2_err Required. Error in co2 (2 values, corresponding to depths)
#' @param diffus Required. Diffusivity (a single value)
#' @param diffus_err Required. Diffusive error (a single value)
#'
#' @return Tibble of depth, flux, and error.  The depth is the mean between the depths
#' @export
#'
#' @examples
#' compute_flux_gradient_layer(depths = c(-0.03,-0.09),
#' co2 = c(28030, 66834.66),
#' co2_err = c(1327,1246),
#' diffus = 7.13e-06,
#' diffus_err = 3.44e-6
#' )
#'
compute_flux_gradient_layer <- function(depths,co2,co2_err,diffus,diffus_err) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2024-06-13)
  #     original creation


  # Assume the depths and co2 correspond correctly
   # Will just compute F = -Da*dC/dz
  # NOTE: if depths are measured with negative numbers, that means as the depths decrease (get more negative), co2 should increase, so dC/dz should be negative. Then F will be a positive value.  When reported F is negative, that means dC/dz > 0, or opposite of what we would expect.
  # When depths are measured with positive numbers, that means as the depths increase, co2 should increase, so dC/dz should be positive.  Then F will be a negative value.  When reported F is positive, that means dC/dz < 0, or opposite of what we would expect.
  # For consistency with NEON reporting we will assume depths are negatives.

  # diff is y2-y1, so when we do the partial derivatives for (diffus, y1, y2)
  # flux_pd <-  (pd_diffus,pd_y1,pd_y2)

  flux <- -diffus*diff(co2)/diff(depths)

  errs <- c(diffus_err,co2_err)
  flux_pd <- c(-diff(co2)/diff(depths), diffus/diff(depths),-diffus/diff(depths) )

  flux_err <- quadrature_error(flux_pd, errs)
  zOffset <- mean(depths)

  gradient <- diff(co2)/diff(depths)
  gradient_pd <- c(-1,1)/diff(depths)
  gradient_err <- quadrature_error(gradient_pd,co2_err)

  return(tibble::tibble(zOffset,flux,flux_err,gradient,gradient_err))
}
