#' Title Determine the surface flux from linear regression at layers
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Computation function. Given a measurement of the co2 and diffusive flux at different levels, return the surface flux

#' @param input_data Required. A data frame containing zOffsets, diffusivity, and co2 (umol mol-1) and their associated errors
#'
#'
#' @return A value of the surface CO2 flux (umol m-2 s-1)

#' @seealso [compute_surface_flux()] for other ways to compute surface fluxes.
#'
compute_surface_flux_layer <- function(input_data) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2024-06-13)
  #     original creation
  #   John Zobitz (2024-11-20)
  #     modified to report out co2 gradient and error


  .data = NULL  # Appease R CMD Check


  # Select the first three levels (closest to the surface)
  input_data_rev <- input_data |>
    dplyr::arrange(dplyr::desc(.data[["zOffset"]])) |>
    dplyr::slice(1:3)


  # Select the first three levels (closest to the surface)
  zOffset <- input_data_rev |> dplyr::pull(.data[["zOffset"]])

  diffus <- input_data_rev |> dplyr::pull(.data[["diffusivity"]])
  diffus_err <- input_data_rev |> dplyr::pull(.data[["diffusExpUncert"]])

  co2 <- input_data_rev |> dplyr::pull(.data[["co2_umol"]])
  co2_err <- input_data_rev |> dplyr::pull(.data[["co2ExpUncert"]])




  ### Step 1: linearly interpolate the diffusivity and co2 through the profile



  # Assume there is no error on the depth measurements
  zOffset_err <- array(0,dim=length(zOffset))

  # First do co2 - we need both the slope and the intercept from that regression
  co2_regression <- linear_regression(x = zOffset,
                                      x_err = zOffset_err,
                                      y = co2,
                                      y_err = co2_err
  )

  # Define the surface fit and error
  co2_0_fit <- co2_regression$intercept
  co2_0_fit_err <- co2_regression$intercept_err

  # Compute Da0 - surface diffusivity
  diffus_regression <- linear_regression(x = zOffset,
                                  x_err = zOffset_err,
                                  y = diffus,
                                  y_err = diffus_err
  )

  diffus_0_fit <- diffus_regression$intercept
  diffus_0_fit_err <- diffus_regression$intercept_err



  # The first method is using the diffusivity linear regression intercept  (Da) + the co2 linear regression slope (dC/dz).  F = -Da*dC/dz

  flux_000 <- tibble::tibble(
    flux = -diffus_0_fit*co2_regression$slope,
    flux_err = quadrature_error(
      x_pd = c(-co2_regression$slope,-diffus_0_fit),
      x_err = c(diffus_0_fit_err,co2_regression$slope_err)
      ),
    r2 = co2_regression$r2,
    gradient = co2_regression$slope,
    gradient_err = co2_regression$slope_err,
    method = "000")

  # Set up a data frame to compute fluxes at each layer
  flux_data_pre_revised <-  tibble::tibble(
    zOffset = c(zOffset),
    zOffset_err = 0,
    co2 = c(co2),
    co2_err = c(co2_err),
    diffus = c(diffus),
    diffus_err = c(diffus_err)
  ) |>
    dplyr::arrange(dplyr::desc(zOffset))

  # Now we need to get ready to map!
  results <- vector(mode = "list",length=(nrow(flux_data_pre_revised)))

  # Take all possible combinations of levels
  index_combination <- combn(1:nrow(flux_data_pre_revised),2,simplify=FALSE)
  # Remember that the top layer is the surface, so for the diffusivity we always use the computed measurements at the lower layer
  for(i in seq_along(results)) {

    out_test_flux <- compute_flux_gradient_layer(depths = flux_data_pre_revised$zOffset[index_combination[[i]]],
                                co2 = flux_data_pre_revised$co2[index_combination[[i]]],
                                co2_err = flux_data_pre_revised$co2_err[index_combination[[i]]],
                                diffus = flux_data_pre_revised$diffus[index_combination[[i]][2]],
                                diffus_err = flux_data_pre_revised$diffus_err[index_combination[[i]][2]])

    out_char <- c("0","0","0")
    out_char[index_combination[[i]]] <- "1"  # Report a 1 at a given measurement level


    results[[i]] <- out_test_flux |>
      dplyr::mutate(method = stringr::str_c(out_char,collapse=""),
                    r2 = NA)

  }


  dplyr::bind_rows(results) |>
    dplyr::select(flux,flux_err,gradient,gradient_err,method,r2) |>
    rbind(flux_000)


}





