#' Compute linear regression coefficients and errors.
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given  x and y data compute simple linear regression slope and intercept, with errors by quadrature.
#'
#' @param x Required. vector of independent variables.
#' @param x_err Required. vector of independent variable errors.
#' @param y Required. Vector of dependent variables.
#' @param y_err Required. Vector of dependent variable errors.
#'
#' @return A data frame of linear regression slope, intercept, and associated errors
#' @export
#'
#' @examples
#' y<- c(345,432,233)
#' y_err <- c(1,5,6)
#' x <- c(0.01,0.04,0.17)
#' x_err <- c(0.1,.3,.6)
#' linear_regression(x,x_err,y,y_err)
linear_regression <- function(x,x_err,y,y_err) {

  .data = NULL  # Appease R CMD Check

  ### Code that can do a linear regression and uncertainty for both the slope and intercept.  Only assume error in y

  # Calculations are easy, it is just the error on the slope / intercept

  # measurements x and y will have three elements

  # see pages 47-52 of lab notebook #current

  # intercept = ybar - slope*xbar
  # slope = sum[(x-xbar)*(y-ybar)]/sum[(x-xbar)^2]



  ybar <- mean(y)
  ny <- length(y)
  xbar <- mean(x)
  nx <- length(x)

  ybar_err <- quadrature_error(1/ny,y_err)
  xbar_err <- quadrature_error(1/nx,x_err)

  numerator <- sum((x-xbar)*(y-ybar))
  denominator <- sum((x-xbar)^2)
  slope <- numerator/denominator
  intercept <- ybar - slope*xbar

  ### Now for the partial derivatives - slope = f(x,y,xbar,ybar) (makes things easier!)
  slope_pdx <- ((y-ybar)*denominator - numerator*2*(x-xbar))/denominator^2
  slope_pdy <- (x-xbar)/denominator
  slope_err <- quadrature_error(c(slope_pdx,slope_pdy),c(x_err,y_err))

  ### Intercept is easier to work with!
  intercept_pdslope <- -xbar
  intercept_pdybar <- -1
  intercept_pdxbar <- slope

  intercept_err <- quadrature_error(c(intercept_pdslope,intercept_pdybar,intercept_pdxbar), c(slope_err,ybar_err,xbar_err))

  yfit <- intercept + slope*x
  ss_res <- sum((y-yfit)^2)
  ss_tot <- sum((y-ybar)^2)
  r2 <- 1-ss_res/ss_tot

  out_values <- tidyr::tibble(slope,
                       slope_err,
                       intercept,
                       intercept_err,
                       r2)

  return(out_values)

}
