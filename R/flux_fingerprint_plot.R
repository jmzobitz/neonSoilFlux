#' @title Helper function to plot QF results for fluxes.
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a flux measurement data frame, show when the flux and diffusivity measurements produced a QF value
#' @param input_fluxes data frame of computed fluxes
#' @param input_diffus_method character string of diffusivity method used to compute fluxes. Can be one of "Marshall" or "Millington-Quirk".
#'
#' @return A ggplot graph where we have ordered factors showing the QA values a given flux computation
#' @export
#'
#' @examples
#' # Make a fingerprint plot for computed flux values:
#' flux_fingerprint_plot(sjer_flux_2022_06)

# changelog and author contributions / copyrights
#   John Zobitz (2025-05-07)
#     original creation
#   John Zobitz (2025-11-17)
#     updates to specify diffusivity methods

flux_fingerprint_plot <- function(input_fluxes,input_diffus_method = "Marshall") {

  .data = NULL  # Appease R CMD Check

  input_fluxes |>
    dplyr::select(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["flux_compute"]]) |>
    tidyr::unnest(cols=c("flux_compute")) |>
    dplyr::mutate(fluxMeanQF = purrr::map_lgl(.data[["flux"]],anyNA),
                  fluxMeanQF = dplyr::if_else(.data[["fluxMeanQF"]],2,0),
   ) |>
    dplyr::select(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["diffus_method"]],.data[["method"]],.data[["fluxMeanQF"]]) |>
    dplyr::filter(diffus_method == input_diffus_method) |>
    dplyr::mutate(
      week_day = lubridate::wday(.data[["startDateTime"]]),
      decimal_hour = lubridate::hour(.data[["startDateTime"]]) + lubridate::minute(.data[["startDateTime"]]) / 60,
      day = lubridate::floor_date(.data[["startDateTime"]], unit = "day")
    ) |>
    dplyr::mutate(value = as.factor(.data[["fluxMeanQF"]])) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = .data[["decimal_hour"]], y = .data[["day"]], fill = .data[["value"]])) +
    ggplot2::facet_grid(horizontalPosition ~ method) +
    ggplot2::labs(fill = "QF Check:", x = "Hour of Day", y = "Date") +
    ggplot2::scale_y_datetime(breaks = "7 day") +
    ggplot2::theme(legend.position = "bottom")  +
    ggplot2::scale_fill_manual(values=c("#33CC00", "#FF3333"),
                               labels = c("0"="Pass","2"="Fail"),
                               drop = FALSE) +
    ggplot2::ggtitle(paste0("Diffusivity method: ",input_diffus_method))


}
