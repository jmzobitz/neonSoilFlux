#' @title Interpolate different depth measurements

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_measurements Required. Nested data frame (tibble of a months worth of data of co2, temperature, swc, pressure)
#' @param measurement_name Required. Names of measurements we are interpolating. Currently only does one column at a time.
#' @param measurement_interpolate Required. Names of measurement whose depth is used to interpolate (typically co2)


#' @return A nested data frame with interpolated measurements.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr


#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-20)
#     original creation
#   2022-06-06: Modification of the interpolation to accelerate computation
#   2022-06-11: Modification to improve fitting
#   2023-07-14: Extensive modification to improve interpolation of multiple columns of data.
#               This now includes all the input data, but only interpolates measurements
#     2024-04-08: update to get namespaces correct
##############################################################################################


depth_interpolate <- function(input_measurements,
                              measurement_name,
                              measurement_interpolate) {
  # Get out the depths to which we will interpolate, make it into a nested data frame
  interp_positions_co2 <- input_measurements |>
    dplyr::filter(measurement == measurement_interpolate) |>
    dplyr::select(-monthly_mean, -n_obs) |>
    tidyr::unnest(cols = c("data")) |>
    dplyr::group_by(horizontalPosition, startDateTime) |>
    tidyr::nest() |>
    dplyr::rename(interp_data = data)



  # This takes the measurements we want to interpolate and creates a nested data frame for them
  input_env_values <- input_measurements |>
    dplyr::filter(measurement %in% c(measurement_name)) |>
    dplyr::select(-n_obs, -monthly_mean) |>
    dplyr::mutate(data = purrr::map(.x = data, .f = ~ (.x |> dplyr::group_by(horizontalPosition, startDateTime) |> tidyr::nest()))) |>
    # unnest(cols=c("data")) |>
    dplyr::rename(measurement_data = data) |>
    dplyr::mutate(measurement_data = purrr::map(.x = measurement_data, .f = ~ (.x |> dplyr::inner_join(interp_positions_co2, by = c("horizontalPosition", "startDateTime")))))



  ### Now do the interpolation - it is fastest to fill up a list and then join it onto our data frame.
  env_data_interp <- input_env_values |>
    dplyr::mutate(results = purrr::map2(
      .x = measurement_data,
      .y = measurement,
      .f = function(x, y) {
        env_data <- x
        curr_measurement <- y

        out_interp <- vector(mode = "list", length = nrow(env_data))
        out_qf <- vector(mode = "integer", length = nrow(env_data))

        for (i in 1:nrow(env_data)) {
          measurement_sp <- FALSE
          if (curr_measurement == "VSWC") {
            measurement_sp <- TRUE
          }

          current_data <- env_data$data[[i]]
          interpolate_depth <- env_data$interp_data[[i]]$zOffset
          col_names <- names(current_data)

          var_mean <- dplyr::pull(current_data, var = which(stringr::str_detect(col_names, "[^StdEr]Mean$"))) # 30-min means
          var_uncert <- dplyr::pull(current_data, var = which(stringr::str_detect(col_names, "ExpUncert$"))) # expanded measurement uncertainty at 95% confidence
          var_qf <- dplyr::pull(current_data, var = which(stringr::str_detect(col_names, "FinalQF$")))


          out_fitted_vals <- fit_function(
            input_depth = current_data$zOffset,
            input_value = var_mean,
            input_value_err = var_uncert,
            input_value_qf = var_qf,
            interp_depth = interpolate_depth,
            measurement_special = measurement_sp
          ) |>
            dplyr::rename(Mean = value)

          bad_measures <- any(is.na(out_fitted_vals$Mean) | is.na(out_fitted_vals$ExpUncert)) # Check if any are NA
          # Adjust the names so we don't do it later
          curr_names <- names(out_fitted_vals)
          new_names <- stringr::str_c(curr_measurement, curr_names) |>
            stringr::str_replace(pattern = paste0(curr_measurement, "zOffset"), "zOffset")
          names(out_fitted_vals) <- new_names

          out_interp[[i]] <- out_fitted_vals

          # Assign a QF value
          MeanQF_val <- 0

          if (bad_measures) {
            MeanQF_val <- 2
          } else {
            if (any(var_qf == 1)) {
              MeanQF_val <- 1
            }
          }

          out_qf[[i]] <- MeanQF_val
        }

        ### Can we make this a map?
        env_data_out <- env_data |>
          cbind(tibble::tibble(out_interp, out_qf))


        return(env_data_out)
      }
    ))




  ### UGH, this is a deeply nested list
  env_co2_data <- env_data_interp |>
    dplyr::select(-measurement_data) |> # remove original env data
    tidyr::unnest(cols = c("results")) |> # unnest
    dplyr::select(-data) |>
    dplyr::mutate(measurement = paste0(measurement, "MeanQF")) |> # Add in the mean value to the measurement name and then nest by the half hour
    dplyr::group_by(horizontalPosition, startDateTime) |>
    tidyr::nest()

  # Next join all the soil measurements together
  env_co2_data_2 <- env_co2_data |>
    dplyr::mutate(
      env_data = purrr::map(.x = data, .f = ~ (.x$out_interp[[1]] |> dplyr::inner_join(.x$out_interp[[2]], by = "zOffset") |>
        dplyr::inner_join(.x$interp_data[[1]], by = "zOffset"))),
      qf_flags = purrr::map(.x = data, .f = ~ (.x |> dplyr::select(-out_interp) |> tidyr::pivot_wider(names_from = "measurement", values_from = "out_qf")))
    )

  # Almost there!
  out_fitted <- env_co2_data_2 |>
    dplyr::mutate(soilCO2concentrationMeanQF = purrr::map_dbl(.x = env_data, .f = function(x) {
      col_names <- names(x)
      var_qf <- dplyr::pull(x, var = which(stringr::str_detect(col_names, "FinalQF$")))

      out_qf <- 0
      if (any(var_qf == 2)) {
        out_qf <- 2
      }
      if (any(var_qf == 1)) {
        out_qf <- 1
      }

      return(out_qf)
    })) |>
    dplyr::select(-data) |>
    tidyr::unnest(cols = c(qf_flags)) |>
    dplyr::select(-interp_data)



  return(out_fitted)
}
