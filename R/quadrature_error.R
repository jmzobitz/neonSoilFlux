#' @title Helper function to quickly compute the quadrature error

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Assume a derived quantity y is a function of inputs x_i: y = f(x_1,x_2,x_3, ...)
#'
#' Given uncertainties (x_err) for each x_i, then this function will compute the corresponding y_err via quadrature.

#' Inputs are the vector of partial derivaties df/dx_i, evaluated at (x_1,x_2,x_3,...).
#'
#' Resulting y_err is the square root of the sum of (df/dx_1)^2 * (x_err)^2 + (df/dx_2)^2 * (x_err)^2 + (df/dx_3)^2 * (x_err)^2 ...

#' @param x_pd Required. Input vector of partial derivatives for y = f(x), evaluated at x_i
#' @param x_err Required. Error vector of measurements

#' @return A value of quadrature error
#'
#' @examples
#' # Let's say we have 5 temperature measurements w/ error::
#' temperature <- c(31.108, 30.689, 30.463, 30.381, 30.250)
#' temperature_error <- c(0.1508,0.1507,0.1497,0.1496,0.1497)
#'
#' # The sample mean is the sum of all measurements divided by the average:
#' sum(temperature)/5  # (Can also be computed with mean(temperature))
#'
#' # The vector of partial derivatives is just 1/n for each measurement:
#' temperature_pd <- c(1/5,1/5,1/5,1/5,1/5)
#' quadrature_error(temperature_pd,temperature_error)
#' # Note: quadrature_error(1/5,temperature_error) is also allowed.



#' @export



quadrature_error <- function(x_pd,x_err) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2023-07-19)
  #     original creation
  #     2024-05-21: documentation updated




  out_val <- sqrt( sum( x_pd^2*x_err^2 ) )

  return(out_val)




}

