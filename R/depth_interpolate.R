#' @title Interpolate different depth measurements

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Linearly interpolate a measurement across the different measurement depths


#' @param input_measurements Required. Nested data frame (tibble of a months worth of data of co2, temperature, swc, pressure)
#' @param measurement_name Required. Names of measurements we are interpolating. Currently only does one column at a time.
#' @param measurement_interpolate Required. Names of measurement whose depth is used to interpolate (typically co2)
#' @param column_selectors Required. Names of columns (e.g column_selectors = c("Mean","ExpUncert")) that are used in the interpolation. These are matched for each NEON measurement, i.e SWC is VSWCMean for the Mean SWC.

#' @return A nested data frame with interpolated measurements.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none
#' @import dplyr


#' @examples


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-20)
#     original creation
#   2022-06-06: Modification of the interpolation to accelerate computation
#   2022-06-11: Modification to improve fitting
#   2023-07-14: Extensive modification to improve interpolation of multiple columns of data.
#               This now includes all the input data, but only interpolates measurements
##############################################################################################


depth_interpolate <- function(input_measurements,
                              measurement_name,
                              measurement_interpolate,
                              column_selectors) {


  # Get out the depths to which we will interpolate, make it into a nested data frame
  interp_positions <- input_measurements |>
    filter(measurement == measurement_interpolate) |>
    select(positions) |>
    unnest(cols=c("positions")) |>
    group_by(HOR) |>
    nest() |>
    rename(interp_positions = data)

  # join in the vertical positions names - we will use this for adding onto the final data frame later
  interp_positions_long <- interp_positions |>
    unnest(cols=c("interp_positions"))

  # Define a string for renaming the data
  rename_string <- "horizontalPosition|verticalPosition|startDateTime|Mean|ExpUncert|zOffset"

  # This takes the measurements we want to interpolate and creates a nested data frame to interpolate
  input_values <- input_measurements |>
    filter(measurement %in% c(measurement_name)) |>
    mutate(data = map2(.x=data,.y=positions,.f=~( .x |> inner_join(.y,by=c("horizontalPosition"="HOR","verticalPosition"="VER","siteID")) ) ) ) |>
    mutate(data = map2(.x=data,.y=measurement,.f= function(.x,.y) {
                                                   .x <- .x |> select(horizontalPosition,verticalPosition,startDateTime,str_c(.y,column_selectors),zOffset);
                                                   new_names <- names(.x) |> str_extract(pattern=rename_string);
                                                   names(.x) <- new_names;
                                                   return(.x)
                                                   } ) ) |>
    select(-n_obs,-positions) |>
    unnest(cols=c("data")) |>
    group_by(measurement,horizontalPosition,startDateTime) |>
    nest() |>
    rename(measurement_data = data) |>
    inner_join(interp_positions,by=c("horizontalPosition"="HOR")) |>
    rename(positions = interp_positions)


  ### Now do the interpolation - it is fastest to fill up a list and then join it onto our data frame.

  out_interp <- vector(mode="list",length=nrow(input_values))

  for(i in 1:nrow(input_values)) {
    measurement_sp <- FALSE
    if(input_values$measurement[[i]] == "VSWC") {measurement_sp = TRUE}
    out_interp[[i]] <-  fit_function(input_depth = input_values$measurement_data[[i]]$zOffset,
                              input_value = input_values$measurement_data[[i]]$Mean,
                              input_value_err = input_values$measurement_data[[i]]$ExpUncert,
                              interp_depth = input_values$positions[[i]]$zOffset,
                              measurement_special = measurement_sp) |>
      rename(Mean=value)

  }


  # Now start to clean things up here ... this will be a multistep process

  my_names <- names(out_interp[[1]])  # Define a vector of names which we will name each returned values with the measurement

  # Change then names of each data frame for the interpolated measurement from Mean to MeasurementMean, where Measurement = VSWC, etc.

  interpolated_measurements <- input_values |>
    cbind(tibble(vals=out_interp)) |>
    mutate(vals = map2(.x=vals,.y=measurement,.f= function(.x,.y) {new_names <- str_c(.y,my_names) |>
      str_replace(pattern=paste0(.y,"zOffset"),"zOffset");
    names(.x) <- new_names;
    return(.x)
    } ) )

  # Add in the position of each depth - this may vary by each horizontal posiition.
  interpolated_measurements2 <- interpolated_measurements |>
    select(-measurement_data) |>
    ungroup() |>
    mutate(vals = map2(.x=vals,.y=positions,.f=~(.x |> inner_join(.y,by="zOffset") |> rename(verticalPosition = VER) |> select(-zOffset))))

  # Now group by each measurement and unnest. Add in the observations and the reference data frame for different positions
  interpolated_measurements3 <- interpolated_measurements2 |>
    select(-positions) |>
    group_by(measurement) |>
    nest() |>
    mutate(data = map(.x=data,.f=~unnest(.x,cols=c("vals")))) |>
    mutate(positions = map(.x=data,.f=~interp_positions_long),
           n_obs = map_dbl(.x=data,.f=~nrow(.x)))


  # Now join up!
  # Get out the depths to which we will interpolate
  non_interp_measurements <- input_measurements |>
    filter(!(measurement %in% measurement_name)  ) |>
    mutate(data = map2(.x=data,.y=measurement,.f=~.x |>select(matches(c(str_c(.y,column_selectors),"horizontalPosition","verticalPosition","siteID","startDateTime") |> paste0(collapse="|"))) ) )

 out_fitted <- rbind(interpolated_measurements3,non_interp_measurements)


  # Now we need to go back and to write out the results

  return(out_fitted)



}

