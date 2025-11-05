#' @title Internal function that inserts smoothed mean value of a measurement at a site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given a site measurement and monthly mean, insert in the monthly mean value when the QF flag fails.

#' @param data Required. input data to use
#' @param monthly_mean Required. monthly mean of input data to use
#' @param measurement_name Required. name of measurement

#' @return Nested data frame of measurements



insert_mean <- function(data,monthly_mean,measurement_name) {

  # changelog and author contributions / copyrights
  #   John Zobitz (2024-01-18)
  #     original creation
  #     2024-04-08: update to get namespaces correct



  .data = NULL  # Appease R CMD Check

  joined_data <- data |>
    dplyr::left_join(monthly_mean,by=c("horizontalPosition","verticalPosition")) |>
    dplyr::select(-.data[["zOffset.y"]]) |>
    dplyr::rename(zOffset=.data[["zOffset.x"]])


  col_names <- names(joined_data)
  mean_names <- names(monthly_mean)


  var_mean <- dplyr::pull(joined_data,var=which(stringr::str_detect(col_names,"[^StdEr]Mean$")) )  # 30-min means
  monthly_mean <- dplyr::pull(joined_data,.data[["comp_mean"]])
  monthly_uncert <- dplyr::pull(joined_data,.data[["comp_sd"]])
  var_uncert <- dplyr::pull(joined_data,var=which(stringr::str_detect(col_names,'ExpUncert$') ) ) # expanded measurement uncertainty at 95% confidence
  var_QF <-  dplyr::pull(joined_data,var=which(stringr::str_detect(col_names,"FinalQF$")) )

  smoothed_data <- tibble::tibble(var_mean,var_uncert,monthly_mean,monthly_uncert,var_QF) |>
    dplyr::mutate(var_mean = dplyr::if_else(var_QF ==0,var_mean,monthly_mean),
           var_uncert = dplyr::if_else(var_QF ==0,var_uncert,monthly_uncert),
           mean_QF =dplyr::if_else(var_QF ==0,0,1),
           mean_QF = dplyr::if_else(is.na(var_mean)| is.na(var_uncert),2,.data[["mean_QF"]]))
  # if the monthly mean is a NA, then we use a 2
  # meanQF = 0 --> no smoothed mean
  # meanQF = 1 --> smoothed mean used
  # meanQF = 2 --> NA flag, so we can't use measurement

  # Update columns of joined data
  joined_data[stringr::str_detect(names(joined_data),"[^StdEr]Mean$")] <- dplyr::pull(smoothed_data,var_mean)
  joined_data[stringr::str_detect(names(joined_data),'ExpUncert$')] <- dplyr::pull(smoothed_data,var_uncert)


  out_data <- joined_data |>
    dplyr::ungroup() |>
    dplyr::mutate(mean_QF = dplyr::pull(smoothed_data,.data[["mean_QF"]])) |>
    dplyr::select(-.data[["comp_mean"]],-.data[["comp_sd"]],-.data[["simple_mean"]],-.data[["simple_sd"]])

  # rename final QF frame
  names(out_data)[names(out_data) == "mean_QF"] <- paste0(measurement_name,"MeanQF")

  return(out_data)

}


