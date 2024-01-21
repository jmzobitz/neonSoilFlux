#' @title Function to compute monthly means to smooth the data.

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a NEON measurement data frame calculate the monthly mean values across all horizontal and vertical locations. Based off code from Zoey Werbin.

#' @param NEON_data Required. Input vector of neon measurements for a month
#' @param position_columns Optional. Do we group by horiztonalPosition, verticalPosition, and? Default is both. Added this option in case we just want to average across a given dimension.

#' @return A data frame that reports for each horiztonal and vertical position the computed mean and standard deviation from sampling (similar to a bootstrap method) as well as the sample mean and sample standard deviation

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples
#' # Download the NEON data - here this would be soil moisture
#' NEON_moist_30m_orig <-  neonUtilities::loadByProduct(dpID="DP1.00094.001",
#' site="WREF",
#' startdate="2022-06",
#' enddate="2022-06",
#' package="expanded",
#' check.size = F)
#'
#' # Then correct the swc
#' site_swc <- swc_correct(NEON_moist_30m_orig,"WREF")
#'
#' # Select the columns and the time frequency
#' time_frequency = "30_minute"
#' column_selectors = c("Mean","Minimum","Maximum","ExpUncert","StdErMean")
#'
#' swc <- site_swc |>
#' pluck(paste0("SWS_",time_frequency)) |>
#' select(domainID,siteID,horizontalPosition,verticalPosition,startDateTime,matches(str_c("VSWC",column_selectors)),VSWCFinalQF)
#'
#' # Now apply the monthly mean:
#' swc_monthly_mean <- compute_monthly_mean(swc)

#' @import dplyr
#' @import tidyselect


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2024-01-17)
#     modified to also compute the depth (zOffset) from the max of all zOffsets that month
#   John Zobitz (2024-01-10)
#     original creation
#   Zoey Werbin (@zoey-rw): original author https://github.com/zoey-rw/microbialForecasts/blob/caa7b1a8aa8a131a5ff9340f1562cd3a3cb6667b/data_construction/covariate_prep/soil_moisture/clean_NEON_sensor_moisture_data.r



compute_monthly_mean <- function(NEON_data,position_columns = c("horizontalPosition","verticalPosition")) {

  out_mean <- NEON_data |>
    filter(if_any(ends_with("FinalQF"), ~ (.x  == 0))) |>
    drop_na() |>
    mutate(day = lubridate::day(startDateTime)) |>
    group_by(across(any_of(position_columns))) |>
    nest() |>
    mutate(month_stats = map(.x=data,.f= function(input_data) {

      zOffset = input_data |> count(zOffset) |>
        slice_max(n) |> pull(zOffset) # Take the max tally of the different depths (if there are any)



      if (length(unique(input_data$day)) >= 15) {
        col_names <- names(input_data)

        tsY <- pull(input_data,var=which(str_detect(col_names,"[^StdEr]Mean$")) )  # 30-min means
        uc <- pull(input_data,var=which(str_detect(col_names,'ExpUncert$') ) )/2 # expanded measurement uncertainty at 95% confidence
        uNAT <-  pull(input_data,var=which(str_detect(col_names,"StdErMean$")) )

        ub.sq <- (uc^2) - (uNAT^2)
        ub.sq[which(ub.sq < 0)] <- .0001 # negative values from rounded uncertainties...
        ub <- sqrt(ub.sq)


        nm = 5000
        tsamp = rep(NA,nm)
        tsamp_sd = rep(NA,nm)
        for(i in 1:nm){
          ub.samp <- sample(ub, 1) # sample 1 bias per month
          bias = rnorm(1,0,1) * ub.samp # assume that the bias is perfectly correlated from obs to obs

          uNAT.samp <- sample(uNAT, 1)
          tsim = rnorm(nm,tsY,uNAT.samp) ## sample random error per half hour - uses R recycling since nm < tsY

          tsamp[i] = mean(tsim,na.rm=TRUE)+bias # output monthly mean
          tsamp_sd[i] = sd(tsim,na.rm=TRUE)  # output monthly sd
        }
        comp_mean <- mean(tsamp)
        comp_sd <- mean(tsamp_sd)

        # just for sanity checking.
        simple_mean <- mean(tsY, na.rm=T)
        simple_sd <- sd(tsY, na.rm = T)


        out_tibble <- tibble(comp_mean,comp_sd,simple_mean,simple_sd,zOffset)

      } else {
        out_tibble <- tibble(comp_mean=NA,comp_sd=NA,simple_mean=NA,simple_sd=NA,zOffset)
      }

      return(out_tibble)


    }
    ) ) |>
    select(-data) |>
    unnest(cols=c(month_stats)) |>
    ungroup()

  return(out_mean)


}



