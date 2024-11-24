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
  #   John Zobitz (2024-11-24)
  #     adjusted to single function to compute QF flags for half hour, based on proportion of measurements within a half hour (70% threshold for both good and mean)


  .data = NULL  # Appease R CMD Check
  ## Function takes data and the measurement, checks to see if there are more than 2 measurements for swc, temperature, and co2 at a given spatial location and time.
  ## For atmospheric pressure, just checks to see if we have a measurement


  data_revised <- data |>
    select(c("horizontalPosition","startDateTime") | tidyselect::ends_with("MeanQF")) |>
    dplyr::group_by(startDateTime,horizontalPosition) |>
    tidyr::nest() |>
    dplyr::mutate(n_vals = purrr::map_dbl(data,nrow),
                  n_good = purrr::map_dbl(.x=data,.f=~( ((dplyr::pull(.x) ==(0 )) |> sum() ))),
                  n_mean = purrr::map_dbl(.x=data,.f=~( ((dplyr::pull(.x) ==(1 )) |> sum() )))
    ) |>
    dplyr::mutate(mean_used = dplyr::if_else(n_good / n_vals > 0.7,0,2),
                  mean_used = dplyr::if_else((n_good + n_mean)/n_vals > 0.7 & mean_used ==2,1,mean_used)) |>
    dplyr::select(-data,-.data[["n_good"]],-.data[["n_vals"]],-.data[["n_mean"]]) |>
    dplyr::ungroup()


  # rename final QF frame
  names(data_revised)[names(data_revised) == "mean_used"] <- paste0(measurement_name,"MeanQF")

  return(data_revised)
}
