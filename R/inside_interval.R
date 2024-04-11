#' @title Determine if a YYYY-MM string is inside a interval

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#'
#' @param start starting interval time
#' @param end  ending interval time
#' @param reference_time time we are comparing to - YYYY-MM string
#'
#' @return Logical indicating whether or not the reference time is inside the interval
#' @export

inside_interval <- function(start,end,reference_time) {
  # First evaluate if the input vector times are valid time

  new_reference <- as.POSIXct(lubridate::ym(reference_time))

  new_start <- dplyr::if_else(!is.na(start),as.POSIXct(start),as.POSIXct(lubridate::today()) )
  new_end <- dplyr::if_else(!is.na(end),as.POSIXct(end),as.POSIXct(lubridate::today()) )

  out_val <- dplyr::between(new_reference,new_start,new_end)
  return(out_val)


}

# changelog and author contributions / copyrights
#   John Zobitz (2024-04-10)
#     original creation
