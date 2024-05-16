#' @title Helper function to plot QF results for fluxes.
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a flux measurement data frame, show when the flux and diffusivity measurements produced a QF value
#' @param input_fluxes data frame of computed fluxes
#'
#' @return A ggplot graph where we have ordered factors showing the QA values a given flux computation
#' @export
#'
#' @examples
#' # Say you have a file name of computed fluxes:
#' load("my-fluxes.Rda") # Loads up out_fluxes
#' flux_fingerprint_plot(out_fluxes)
#'
# changelog and author contributions / copyrights
#   John Zobitz (2025-05-07)
#     original creation

flux_fingerprint_plot <- function(input_fluxes) {

  prep_flux <- input_fluxes |>
    dplyr::select(startDateTime,horizontalPosition,flux_compute,diffusivity) |>
    tidyr::unnest(cols=c(flux_compute)) |>
    dplyr::mutate(fluxMeanQF = purrr::map_lgl(flux,anyNA),
           fluxMeanQF = dplyr::if_else(fluxMeanQF,2,0),
           ) |>
    dplyr::select(startDateTime,horizontalPosition,method,fluxMeanQF,diffusivity) |>
    tidyr::pivot_wider(names_from=method,values_from = fluxMeanQF) |>
    tidyr::unnest(cols=c(diffusivity)) |>
    mutate(diffusivity = purrr::map_lgl(diffusivity,is.na),
           diffusivity = dplyr::if_else(diffusivity,2,0)
    ) |>
    dplyr::select(-zOffset,-diffusExpUncert)

  prep_flux |>
    dplyr::mutate(
      week_day = lubridate::wday(startDateTime),
      decimal_hour = lubridate::hour(startDateTime) + lubridate::minute(startDateTime) / 60,
      day = lubridate::floor_date(startDateTime, unit = "day")
    ) |>
    tidyr::pivot_longer(cols = c("diffusivity":"tang_2005")) |>
    dplyr::mutate(name = factor(name,levels=c("diffusivity","dejong_shappert_1972","hirano_2005","tang_2003","tang_2005"))) |>
    dplyr::mutate(value = as.factor(value)) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = decimal_hour, y = day, fill = value)) +
    ggplot2::facet_grid(horizontalPosition ~ name) +
    ggplot2::labs(fill = "QF Check:", x = "Hour of Day", y = "Date") +
    ggplot2::scale_y_datetime(breaks = "7 day") +
    ggplot2::theme(legend.position = "bottom")  +
    ggplot2::scale_fill_manual(values=c("#33CC00", "#FF3333"),
                               labels = c("0"="Pass","2"="Fail"),
                               drop = FALSE)
}
