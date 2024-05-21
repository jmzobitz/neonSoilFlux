#' @title Internal function that interpolates a soil measurement to different depths

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_depth Required. Vector of measurement depths.
#' @param input_value Required. Vector of measured values (i.e. soil temperature and soil water) measured at depths input_depth
#' @param input_value_err Required. Vector or reported measurement errors. Used to compute prediction error (via quadrature) when linear interpolation is used.
#' @param input_value_qf Required. Vector of qf values from the smoothing 0 = no smoothing, 1 = mean value used (smoothing), 2 = NA value
#' @param interp_depth Depths of the sensors required for interpolation
#' @param measurement_special Flag if we want to just do linear interpolation for a given measurement

#' @return A data frame of the depth and the measured column for the measurements and reported error



# changelog and author contributions / copyrights
#   John Zobitz (2021-07-15)
#     original creation
#   2022-06-11: revision to include spline fits of temperature and water, better functionality
#   2023-07-23: revision to includes prediction error for linear interpolation - for splines (n measurements > 3) this is done by estimating the prediction error by quadrature using formulas for simple linear regression.  Also included is the addition of measurement_special, a flag to just do linear interpolation and any other positivity measures.
#   2024-01-19: revision to include the qf flags
#     2024-04-08: update to get namespaces correct
##############################################################################################


fit_function <- function(input_depth,input_value,input_value_err,input_value_qf,interp_depth,measurement_special) {

  # The default value if we have nothing
  out_value <- tibble::tibble(zOffset = interp_depth, value = NA, ExpUncert = NA)

  # do a quick NA filtering on the measurement
  test_data <- tibble::tibble(
    depth = input_depth,
    value = input_value,
    err = input_value_err,
    qf = input_value_qf
  ) |>
    tidyr::drop_na() |>
    dplyr::filter(qf !=2)  # Can use mean qf values

  if (nrow(test_data) > 2) {
    input_depth <- test_data |> dplyr::pull(depth)
    input_value <- test_data |> dplyr::pull(value)
    input_value_err <- test_data |> dplyr::pull(err)
    # Define top layer for interpolation
    if (max(input_depth) > -0.05) {
      from_depth <- 0 # Extrapolate to soil surface is sensor is less than 5 cm from soil surface
    } else {
      from_depth <- round(max(input_depth), digits = 2)
    }


    # Define bottom layer for interpolation
    if (min(input_depth) < -0.2) {
      to_depth <- floor(min(input_depth) * 10) / 10 # Extrapolate to nearest 10 cm increment below deepest sensor if that sensor is below 20 cm (i.e., not experiencing very high diurnal variability)
    } else {
      to_depth <- round(min(input_depth), digits = 2)
    }

    # Create sequence of depths in 1 cm intervals up to the depth of the deepest sensor producing good data
    depths <- seq(from = from_depth, to = to_depth, by = -0.01)


    # Make sure there are more than two data points to interpolate
    if (length(input_depth) > 3 & !measurement_special) {
      measurement_spline <- stats::smooth.spline(x = input_depth, y = input_value)


      xbar <- mean(input_depth)
      n <- length(input_depth)
      pd_yi <- 1/n -(input_depth-xbar)*(1-1/n)/sum((input_depth-xbar)^2)

      predict_err<-quadrature_error(pd_yi,input_value_err)

      # Predict
     out_value <- stats::predict(measurement_spline, x = interp_depth) |>
       tibble::as_tibble() |>
      dplyr::rename(zOffset = x,
               value = y) |>
       dplyr::mutate(ExpUncert = predict_err)





    } else if(dplyr::between(nrow(test_data),2,3) | measurement_special) {

      # Just use linear interpolation here and compute the error by quadrature using the first two data points
      input_depth <- test_data |> dplyr::pull(depth)
      input_value <- test_data |> dplyr::pull(value)
      input_value_err <- test_data |> dplyr::pull(err)

      xbar <- mean(input_depth)
      n <- length(input_depth)
      pd_yi <- 1/n -(input_depth-xbar)*(1-1/n)/sum((input_depth-xbar)^2)

      predict_err<-quadrature_error(pd_yi,input_value_err)


      out_value <- stats::approx(x = input_depth,y=input_value,xout= interp_depth,rule = 2) |>
        tibble::as_tibble() |>
        dplyr::rename(zOffset = x,
               value = y) |>
        dplyr::mutate(ExpUncert = predict_err)

      if(measurement_special & any(out_value$value < 0)) {
        out_value$value = NA
        out_value$ExpUncert = NA
      }
    }


  }


  return(out_value)
}

