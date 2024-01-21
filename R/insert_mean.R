#' @title Insert smoothed mean value of a measurement at a site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}


#' @description
#' Given a site measurement and monthly mean, insert in the monthly mean value when the QF flag fails.

#' @param data Required. input data to use
#' @param monthly_mean Required. monthly mean of input data to use
#' @param measurement_name Required. name of measurement


#'
#'
#' @import dplyr

#' @return Nested data frame of measurements

# changelog and author contributions / copyrights
#   John Zobitz (2024-01-18)
#     original creation


insert_mean <- function(data,monthly_mean,measurement_name) {

  joined_data <- data |>
    left_join(monthly_mean,by=c("horizontalPosition","verticalPosition")) |>
    select(-zOffset.y) |>
    rename(zOffset=zOffset.x)


  col_names <- names(joined_data)
  mean_names <- names(monthly_mean)


  var_mean <- pull(joined_data,var=which(str_detect(col_names,"[^StdEr]Mean$")) )  # 30-min means
  monthly_mean <- pull(joined_data,comp_mean)
  monthly_uncert <- pull(joined_data,comp_sd)
  var_uncert <- pull(joined_data,var=which(str_detect(col_names,'ExpUncert$') ) ) # expanded measurement uncertainty at 95% confidence
  var_QF <-  pull(joined_data,var=which(str_detect(col_names,"FinalQF$")) )

  smoothed_data <- tibble(var_mean,var_uncert,monthly_mean,monthly_uncert,var_QF) |>
    mutate(var_mean = if_else(var_QF ==0,var_mean,monthly_mean),
           var_uncert =if_else(var_QF ==0,var_uncert,monthly_uncert),
           mean_QF =if_else(var_QF ==0,0,1),
           mean_QF = if_else(is.na(monthly_mean) | is.na(monthly_uncert),2,mean_QF))
  # if the monthly mean is a NA, then we use a 2
  # meanQF = 0 --> no smoothed mean
  # meanQF = 1 --> smoothed mean used
  # meanQF = 2 --> NA flag, so we can't use measurement

  out_data <- joined_data |>
    mutate(across(.cols=contains("[^StdEr]Mean$"),.fns=~pull(smoothed_data,var_mean)),
           across(.cols=contains('ExpUncert$'),.fns=~pull(smoothed_data,var_uncert)) ) |>
    ungroup() |>
    mutate(mean_QF = pull(smoothed_data,mean_QF)) |>

    select(-comp_mean,-comp_sd,-simple_mean,-simple_sd)

  # rename final QF frame
  names(out_data)[names(out_data) == "mean_QF"] <- paste0(measurement_name,"MeanQF")

  return(out_data)

}


