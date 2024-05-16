#' @title Helper function to plot QF results.
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a flux measurement data frame, show when the environmental measurements produced a QF value
#' @param input_fluxes data frame of computed fluxes
#'
#' @return A ggplot graph where we have ordered factors showing the QA values a given environmental measurement
#' @export
#'
#' @examples
#' # Say you have a file name of computed fluxes:
#' load("my-fluxes.Rda") # Loads up out_fluxes
#' env_fingerprint_plot(out_fluxes)
# changelog and author contributions / copyrights
#   John Zobitz (2024-04-07)
#     original creation
#     2024-04-08: update to get namespaces correct
#     2024-05-07: colorscheme updates

env_fingerprint_plot <- function(input_fluxes) {
  prep_env <- out_fluxes |>
    dplyr::select(-flux_compute, -diffusivity) |>
    dplyr::mutate(
      week_day = lubridate::wday(startDateTime),
      decimal_hour = lubridate::hour(startDateTime) + lubridate::minute(startDateTime) / 60,
      day = lubridate::floor_date(startDateTime, unit = "day"),
    ) |>
    tidyr::pivot_longer(cols = c("soilCO2concentrationMeanQF":"staPresMeanQF")) |>
    dplyr::mutate(name = stringr::str_extract(name,pattern=".+(?=MeanQF)"))

  prep_env |>
    dplyr::mutate(value = as.factor(value)) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = decimal_hour, y = day, fill = value)) +
    ggplot2::facet_grid(horizontalPosition ~ name) +
    ggplot2::labs(fill = "QF Check:", x = "Hour of Day", y = "Date") +
    ggplot2::scale_y_datetime(breaks = "7 day") +
    ggplot2::theme(legend.position = "bottom")  +
    ggplot2::scale_fill_manual(
      values=c("#33CC00","#FFC333", "#FF3333"),
      labels = c("0"="Pass", "1"="Monthly Mean", "2"="Fail"),
      drop = FALSE
    )



}
