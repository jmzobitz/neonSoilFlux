#' Compute all possible combinations of a vector - ignoring the empty set
#'
#' @param x Required. Input vector
#' @param size_start First power set input size
#'
#' @return A list of possible options
#' @export
#'
#' @examples
#' # All possible combinations
#' powerset(1:4)
#'
#' # Now try without ignoring the single sets:
#' powerset(1:4,2)
powerset <- function(x,size_start=1) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2024-06-13)
  #     original creation

  .data = NULL  # Appease R CMD Check
  # Now we go through all possible combinations of the
  # power set of all levels:
  #https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r


  # Note: we should do an error check if size_start > length(x)
  sets <- lapply(size_start:(length(x)), function(i) utils::combn(x, i, simplify = F))
  unlist(sets, recursive = F)
}
