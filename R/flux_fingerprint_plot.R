#' @title Helper function to plot QF results for fluxes.
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a flux measurement data frame, show when the flux and diffusivity measurements produced a QF value
#' @param input_fluxes data frame of computed fluxes (may be Marshall or Millington-Quirk)
#'
#' @return A ggplot graph where we have ordered factors showing the QA values a given flux computation
#' @export
#'
#' @examples
#' # Make a fingerprint plot for computed flux values:
#' flux_fingerprint_plot(sjer_flux_2022_06$millington_quirk)
#' # Can also use
#' flux_fingerprint_plot(sjer_flux_2022_06$marshall)

# changelog and author contributions / copyrights
#   John Zobitz (2024-05-07)
#     original creation
#   2025-08-23 - modification for revisions for package update

flux_fingerprint_plot <- function(input_fluxes) {

  .data = NULL  # Appease R CMD Check

  prep_flux <- input_fluxes |>
    dplyr::select(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["flux_compute"]],.data[["diffusivity"]]) |>
    tidyr::unnest(cols=c("flux_compute")) |>
    dplyr::mutate(fluxMeanQF = purrr::map_lgl(.data[["flux"]],anyNA),
           fluxMeanQF = dplyr::if_else(.data[["fluxMeanQF"]],2,0),
           ) |>
    dplyr::select(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["method"]],.data[["fluxMeanQF"]],.data[["diffusivity"]]) |>
    tidyr::pivot_wider(names_from=.data[["method"]],values_from = .data[["fluxMeanQF"]]) |>
    tidyr::unnest(cols=c(diffusivity)) |>
    dplyr::mutate(diffusivity = purrr::map_lgl(.data[["diffusivity"]],is.na),
           diffusivity = dplyr::if_else(.data[["diffusivity"]],2,0)
    ) |>
    dplyr::select(-.data[["zOffset"]],-.data[["diffusExpUncert"]])

  prep_flux |>
    dplyr::mutate(
      week_day = lubridate::wday(.data[["startDateTime"]]),
      decimal_hour = lubridate::hour(.data[["startDateTime"]]) + lubridate::minute(.data[["startDateTime"]]) / 60,
      day = lubridate::floor_date(.data[["startDateTime"]], unit = "day")
    ) |>
    tidyr::pivot_longer(cols = c(-"startDateTime",-"horizontalPosition",-"week_day",-"decimal_hour",-"day")) |>
    dplyr::mutate(value = as.factor(.data[["value"]])) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = .data[["decimal_hour"]], y = .data[["day"]], fill = .data[["value"]])) +
    ggplot2::facet_grid(horizontalPosition ~ name) +
    ggplot2::labs(fill = "QF Check:", x = "Hour of Day", y = "Date") +
    ggplot2::scale_y_datetime(breaks = "7 day") +
    ggplot2::theme(legend.position = "bottom")  +
    ggplot2::scale_fill_manual(values=c("#33CC00", "#FF3333"),
                               labels = c("0"="Pass","2"="Fail"),
                               drop = FALSE)
}
