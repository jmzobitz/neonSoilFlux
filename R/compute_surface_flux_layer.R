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
    method = "000")

  # Set up a data frame to compute fluxes at each layer
  flux_data_pre_revised <-  tibble::tibble(
    zOffset = c(0,zOffset),
    zOffset_err = 0,
    co2 = c(co2_0_fit, co2),
    co2_err = c(co2_0_fit_err,co2_err),
    diffus = c(diffus_0_fit,diffus),
    diffus_err = c(diffus_0_fit_err,diffus_err)
  ) |>
    dplyr::arrange(dplyr::desc(zOffset))

  # Now we need to get ready to map!
  results <- vector(mode = "list",length=(nrow(flux_data_pre_revised)-1))


  # Remember that the top layer is the surface, so for the diffusivity we always use the computed measurements
  for(i in seq_along(results)) {
    results[[i]] <- compute_flux_gradient_layer(depths = flux_data_pre_revised$zOffset[i:(i+1)],
                                                co2 = flux_data_pre_revised$co2[i:(i+1)],
                                                co2_err = flux_data_pre_revised$co2_err[i:(i+1)],
                                                diffus = flux_data_pre_revised$diffus[i+1],
                                                diffus_err = flux_data_pre_revised$diffus_err[i+1])


  }


  flux_levels <- dplyr::bind_rows(results) |>
    dplyr::arrange(dplyr::desc(zOffset)) |>
    dplyr::mutate(zOffset_err = 0,
           method = c("100","010","001"),
           r2 = NA)


  flux_indices <- powerset(1:3,size_start=2)

  surface_flux <- purrr::map(.x=flux_indices,.f=function(.x) {
    new_vec <- dplyr::slice(flux_levels,.x)

    out_vec <- linear_regression(x = new_vec$zOffset,
                                 y = new_vec$flux,
                                 x_err = new_vec$zOffset_err,
                                 y_err = new_vec$flux_err )
    out_char <- c("0","0","0")
    out_char[.x] <- "1"

    return(tibble::tibble(flux = out_vec$intercept,
                  flux_err = out_vec$intercept_err,
                  r2 = out_vec$r2,
                  method = stringr::str_c(out_char,collapse="")))

  }
  )


  dplyr::bind_rows(surface_flux) |>
    rbind(dplyr::select(flux_levels,flux,flux_err,method,r2),
          flux_000)


}





