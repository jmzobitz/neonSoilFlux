#' @title Internal function to compute CO2 surface flux

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Computation function. Given a measurement of the co2 and diffusive flux at different levels, return the surface flux

#' @param input_data Required. A data frame containing zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors


#' @return A value of the surface CO2 flux (umol m-2 s-1)


compute_surface_flux <- function(input_data) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2021-07-21)
  #     original creation
  #   John Zobitz (2022-06-21)
  #     revision to account for correct directionality when computing fluxes
  #   John Zobitz (2023-07-14)
  #     revision to include value of gradient at surface and computation of quadrature errors
  #   John Zobitz (2024-01-20)
  #     revision to simplify code - fluxes are calculated via different approaches with the measurements




   .data = NULL  # Appease R CMD Check

  # Compute the flux between the two different layers

  # Select the first three levels (closest to the surface)
  input_data_rev <- input_data |>
    dplyr::arrange(dplyr::desc(.data[["zOffset"]])) |>
    dplyr::slice(1:3)


  # Select the first three levels (closest to the surface)
  zOffset_pos <- input_data_rev |>
    dplyr::pull(.data[["zOffset"]]) |>
    abs() # Make these positive, just to avoid problems down the road.


  diffus <- input_data_rev |> dplyr::pull(.data[["diffusivity"]])
  co2umol <- input_data_rev |> dplyr::pull(.data[["co2_umol"]])

  co2_err <- input_data_rev |> dplyr::pull(.data[["co2StdErMean"]])
  diffusive_err <- input_data_rev |> dplyr::pull(.data[["diffusStdErMean"]])

  # Collect the names of all the flux calculations
  function_names <- c(
    dejong_shappert_flux,
    hirano_flux,
    tang_2003_flux,
    tang_2005_flux
  )


  # Then map across all of them
  output <- purrr::map_df(
    .x = function_names,
    .f = ~ .x(
      zOffset = zOffset_pos,
      co2 = co2umol,
      co2_err = co2_err,
      diffusive = diffus,
      diffusive_err = diffusive_err
    )
  )





  return(output)
}
