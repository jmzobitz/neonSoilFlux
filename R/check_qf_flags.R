#' @title Internal helper function to determine availability of data within a time interval
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a data measurement, determine when a monthly mean is used and the bulk QF flags. Helps to determine when the environmental measurements produced a QF value and to be used in subsequent flux calculations.
#'
#' @param measurement_name The name of the measurement (staPres, soilTemp, VSWC, soilCO2Concentration)
#' @param data Data used to check the qf flags
#'
#' @return A data frame of startDateTime, horizontalPosition, and the associated QF flag.




check_qf_flags <- function(measurement_name,data) {

  #'
  #' # changelog and author contributions / copyrights
  #   John Zobitz (2024-05-08)
  #     original creation

  .data = NULL  # Appease R CMD Check
  ## Function takes data and the measurement, checks to see if there are more than 2 measurements for swc, temperature, and co2 at a given spatial location and time.
  ## For atmospheric pressure, just checks to see if we have a measurement

  # Pull out the unique timeperiods and spatial locations
  startDateTime <- data$startDateTime |> unique()
  horizontalPosition <- data$horizontalPosition |> unique()


  if(measurement_name != "staPres") {

    # Filter if there are two valid (QF neq 2) measurements at each timepoint and horizontalPosition.
    # Also check to see if there any measurements use the mean

    data_revised <- data |>
      dplyr::filter(dplyr::if_any(tidyselect::ends_with("FinalQF"), ~ (.x != 2)) ) |>
      dplyr::group_by(horizontalPosition,startDateTime) |>
      tidyr::nest() |>
      dplyr::mutate(n_valid = purrr::map_dbl(data,nrow)) |>
      dplyr::filter(.data[["n_valid"]] > 2) |>
      dplyr::mutate(mean_used = purrr::map_dbl(.x=data,.f=~(.x |> dplyr::summarize(dplyr::if_any(tidyselect::ends_with("FinalQF"),~any(.x ==1) |> as.numeric()) ) |> dplyr::pull()))
      ) |>
      dplyr::select(-data,-.data[["n_valid"]])
  } else {

    # Filter if there is a valid (QF neq 2) measurements at each timepoint and horizontalPosition.
    # Also check to see if there any measurements use the mean

    data_revised <- data |>
      dplyr::filter(dplyr::if_any(tidyselect::ends_with("FinalQF"), ~ (.x != 2)) ) |>
      dplyr::group_by(horizontalPosition,startDateTime) |>
      tidyr::nest() |>
      dplyr::mutate(mean_used = purrr::map_dbl(.x=data,.f=~(.x |> dplyr::summarize(dplyr::if_any(tidyselect::ends_with("FinalQF"),~any(.x ==1) |> as.numeric()) ) |> dplyr::pull()))
      ) |>
      dplyr::select(-data)

  }

  # Create a data frame of times and positions possible in a given month.
  # Join it to the measurements
  my_join <- tidyr::expand_grid(startDateTime,horizontalPosition) |>
    dplyr::left_join(data_revised,by=c("startDateTime","horizontalPosition")) |>
    dplyr::mutate(mean_used = dplyr::if_else(is.na(.data[["mean_used"]]),2,.data[["mean_used"]]) )

  # rename final QF frame
  names(my_join)[names(my_join) == "mean_used"] <- paste0(measurement_name,"MeanQF")

  return(my_join)
}
