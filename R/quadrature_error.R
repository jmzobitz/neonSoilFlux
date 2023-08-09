#' @title Helper function to quickly compute the quadrature error

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a vector (x) and its errors (x_err), compute the quadrature error

#' @param x Required. Input vector of partial derivatives
#' @param x_err Required. Error vector of measurements

#' @return A value of quadrature error

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples TBD
#'

#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2023-07-19)
#     original creation



quadrature_error <- function(x,x_err) {

  out_val <- sqrt( sum( x^2*x_err^2 ) )

  return(out_val)




}

