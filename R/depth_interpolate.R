#' @title Internal function to interpolate different depth measurements

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_measurements Required. Nested data frame (tibble of a months worth of data of co2, temperature, swc, pressure)
#' @param measurement_name Required. Names of measurements we are interpolating. Currently only does one column at a time.
#' @param measurement_interpolate Required. Names of measurement whose depth is used to interpolate (typically co2)


#' @return A nested data frame with interpolated measurements.



depth_interpolate <- function(input_measurements,
                              measurement_name,
                              measurement_interpolate) {



  # changelog and author contributions / copyrights
  #   John Zobitz (2021-07-20)
  #     original creation
  #   2022-06-06: Modification of the interpolation to accelerate computation
  #   2022-06-11: Modification to improve fitting
  #   2023-07-14: Extensive modification to improve interpolation of multiple columns of data.
  #               This now includes all the input data, but only interpolates measurements
  #   2024-04-08: update to get namespaces correct
  #   2024-11-24: update to improve efficiency and interpolation
  ##############################################################################################


  .data = NULL  # Appease R CMD Check

  # Get out the depths to which we will interpolate, make it into a nested data frame
  interp_positions_co2 <- input_measurements |>
    dplyr::filter(.data[["measurement"]] == measurement_interpolate) |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    tidyr::unnest(cols = c("data")) |>
    #dplyr::select(.data[["horizontalPosition"]], .data[["startDateTime"]],.data[["zOffset"]]) |>
    dplyr::group_by(.data[["horizontalPosition"]], .data[["startDateTime"]]) |>
    tidyr::nest() |>
    dplyr::rename(interp_data = .data[["data"]])



  # This takes the measurements we want to interpolate and creates a nested data frame for them

  ### qf_val is assigned :
  #
  # QF = 0 if more than 70% of QF values are 0 and at least 3 values QF = 0 or 1
  # QF = 1 if more than 70% of QF values are 0 or 1 and at least 3 values QF = 0 or 1
  # QF = 2 otherwise

  input_env_values <- input_measurements |>
    dplyr::filter(.data[["measurement"]] %in% c(measurement_name)) |>
    dplyr::select(-.data[["monthly_mean"]]) |>
    dplyr::mutate(data = purrr::map(.x = .data[["data"]], .f = ~ (.x |> dplyr::group_by(horizontalPosition, startDateTime) |> tidyr::nest()))) |>
    dplyr::rename(measurement_data = .data[["data"]]) |>
    dplyr::mutate(measurement_data = purrr::map(.x=.data[["measurement_data"]],.f=~(.x |>
                                                                    dplyr::mutate(n_vals = purrr::map_dbl(data,nrow),
                                                                                  n_good = purrr::map_dbl(.x=data,.f=~( ((dplyr::pull(.x) ==(0 )) |> sum() ))),
                                                                                  n_mean = purrr::map_dbl(.x=data,.f=~( ((dplyr::pull(.x) ==(1 )) |> sum() )))
                                                                    ) |>
                                                                    dplyr::mutate(qf_val = dplyr::if_else(n_good / n_vals > 0.7 & ((n_good+n_mean)>=3),0,2),
                                                                                  qf_val = dplyr::if_else((n_good + n_mean)/n_vals > 0.7 & qf_val ==2 & ((n_good+n_mean)>=3),1,qf_val)) |>
                                                                    dplyr::select(-n_vals,-n_good,-n_mean)))) |>
    dplyr::mutate(measurement_data = purrr::map(.x = .data[["measurement_data"]], .f = ~ (.x |> dplyr::inner_join(interp_positions_co2, by = c("horizontalPosition", "startDateTime")))))


  # We will be doing a doubly nesting map - yikes!

  env_data_interp <- input_env_values |>
    tidyr::unnest(cols=c("measurement_data")) |>
    dplyr::mutate(results = purrr::map2(.x=.data[["data"]],.y=.data[["interp_data"]],
                                 .f=function(.x,.y) {
                                   interpolate_depth <- .y$zOffset
                                   col_names <- names(.x)

                                   var_depth <- .x$zOffset
                                   var_mean <- dplyr::pull(.x, var = which(stringr::str_detect(col_names, "[^StdEr]Mean$"))) # 30-min means
                                   var_uncert <- dplyr::pull(.x, var = which(stringr::str_detect(col_names, "ExpUncert$"))) # expanded measurement uncertainty at 95% confidence / 2 so 1 SD
                                   var_qf <- dplyr::pull(.x, var = which(stringr::str_detect(col_names, "MeanQF$")))
                                  out_vals <- fit_function(
                                     input_depth = var_depth,
                                     input_value = var_mean,
                                     input_value_err = var_uncert,
                                     input_value_qf = var_qf,
                                     interp_depth = interpolate_depth,
                                     measurement_special = FALSE  # Do regular interpretation
                                   ) |>
                                     dplyr::rename(Mean = .data[["value"]])

                                  return(out_vals)
                                 }
    )
    )


  ### Now do the interpolation - it is fastest to fill up a list and then join it onto our data frame.


# Next the qf_val that is assigned needs to go with the different measurements for that point


  ### UGH, this is a deeply nested list
  env_interpolated_data <- env_data_interp |>
    dplyr::mutate(results = purrr::pmap(.l=list(.data[["results"]],.data[["qf_val"]],.data[["measurement"]]),.f=function(x,y,z) {
      x |> dplyr::mutate(MeanQF=y) |>
      dplyr::rename_with(~ ifelse(. == "zOffset", ., paste0(z, .)))

      })) |>
    dplyr::select(-.data[["data"]],-.data[["interp_data"]],-.data[["qf_val"]]) |>
    dplyr::group_by(.data[["measurement"]]) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(.x=data,.f=~(.x |> tidyr::unnest(cols=c(.data[["results"]])) |>
                                                   dplyr::ungroup()
                                                 )
                                    )
                  )


  ## Now start to join these all up to pitch out

  out_fitted <- input_measurements |>
    dplyr::filter((.data[["measurement"]] %in% measurement_name)) |>
    dplyr::inner_join(env_interpolated_data,by=c("measurement")) |>
    dplyr::select(-.data[["data.x"]]) |>
    dplyr::rename(data = .data[["data.y"]]) |>
    rbind( dplyr::filter(input_measurements,!(.data[["measurement"]] %in% measurement_name)))



  return(out_fitted)
}
