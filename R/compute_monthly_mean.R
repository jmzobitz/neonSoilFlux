#' @title Function to compute monthly means for a given month of NEON data.

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a NEON measurement data frame calculate the monthly mean values across all horizontal and vertical locations. Based off code from Zoey Werbin.

#' @param NEON_data Required. Input vector of neon measurements for a month
#' @param position_columns Optional. Do we group by horizontalPosition, verticalPosition, and? Default is both. Added this option in case we just want to average across a given dimension.

#' @return A data frame that reports for each horiztonal and vertical position the computed mean and standard deviation from sampling (similar to a bootstrap method) as well as the sample mean and sample standard deviation


#' @keywords Currently none

#' @examples
#' \donttest{
#' # Download the NEON data directly - here this would be soil moisture
#' NEON_moist_30m_orig <- neonUtilities::loadByProduct(
#'   dpID = "DP1.00094.001",
#'   site = "WREF",
#'   startdate = "2022-06",
#'   enddate = "2022-06",
#'   package = "expanded",
#'   check.size = F
#' )
#'
#' # Then correct the swc
#' site_swc <- swc_correct(NEON_moist_30m_orig, "WREF")
#'
#' # Select the columns and the time frequency
#' time_frequency <- "30_minute"
#' column_selectors <- c("Mean", "Minimum", "Maximum", "ExpUncert", "StdErMean")
#'
#' swc <- site_swc |>
#'   pluck(paste0("SWS_", time_frequency)) |>
#'   select(
#'   domainID, siteID, horizontalPosition, verticalPosition, startDateTime,
#'    matches(str_c("VSWC", column_selectors)),
#'    VSWCFinalQF)
#'
#' # Now apply the monthly mean:
#' swc_monthly_mean <- compute_monthly_mean(swc)
#' }


#' @export

# changelog and author contributions / copyrights
#   Zoey Werbin (@zoey-rw): original author https://github.com/zoey-rw/microbialForecasts/blob/caa7b1a8aa8a131a5ff9340f1562cd3a3cb6667b/data_construction/covariate_prep/soil_moisture/clean_NEON_sensor_moisture_data.r
#   John Zobitz (2024-01-17)
#     modified to also compute the depth (zOffset) from the max of all zOffsets that month
#   John Zobitz (2024-01-10)
#     original creation
#     2024-04-08: update to get namespaces correct

#' @references
#' Zoey Werbin (@zoey-rw): original author https://github.com/zoey-rw/microbialForecasts/blob/caa7b1a8aa8a131a5ff9340f1562cd3a3cb6667b/data_construction/covariate_prep/soil_moisture/clean_NEON_sensor_moisture_data.r

compute_monthly_mean <- function(NEON_data, position_columns = c("horizontalPosition", "verticalPosition")) {
  out_mean <- NEON_data |>
    dplyr::filter(if_any(ends_with("FinalQF"), ~ (.x == 0))) |>
    tidyr::drop_na() |>
    dplyr::mutate(day = lubridate::day(startDateTime)) |>
    dplyr::group_by(dplyr::across(tidyr::any_of(position_columns))) |>
    tidyr::nest() |>
    dplyr::mutate(month_stats = purrr::map(.x = data, .f = function(input_data) {
      zOffset <- input_data |>
        dplyr::count(zOffset) |>
        dplyr::slice_max(n) |>
        dplyr::pull(zOffset) # Take the max tally of the different depths (if there are any)



      if (length(unique(input_data$day)) >= 15) {
        col_names <- names(input_data)

        tsY <- dplyr::pull(input_data, var = which(stringr::str_detect(col_names, "[^StdEr]Mean$"))) # 30-min means
        uc <- dplyr::pull(input_data, var = which(stringr::str_detect(col_names, "ExpUncert$"))) / 2 # expanded measurement uncertainty at 95% confidence
        uNAT <- dplyr::pull(input_data, var = which(stringr::str_detect(col_names, "StdErMean$")))

        ub.sq <- (uc^2) - (uNAT^2)
        ub.sq[which(ub.sq < 0)] <- .0001 # negative values from rounded uncertainties...
        ub <- sqrt(ub.sq)


        nm <- 5000
        tsamp <- rep(NA, nm)
        tsamp_sd <- rep(NA, nm)
        for (i in 1:nm) {
          ub.samp <- sample(ub, 1) # sample 1 bias per month
          bias <- stats::rnorm(1, 0, 1) * ub.samp # assume that the bias is perfectly correlated from obs to obs

          uNAT.samp <- sample(uNAT, 1)
          tsim <- stats::rnorm(nm, tsY, uNAT.samp) ## sample random error per half hour - uses R recycling since nm < tsY

          tsamp[i] <- mean(tsim, na.rm = TRUE) + bias # output monthly mean
          tsamp_sd[i] <- sd(tsim, na.rm = TRUE) # output monthly sd
        }
        comp_mean <- mean(tsamp)
        comp_sd <- mean(tsamp_sd)

        # just for sanity checking.
        simple_mean <- mean(tsY, na.rm = T)
        simple_sd <- stats::sd(tsY, na.rm = T)


        out_tibble <- tibble::tibble(comp_mean, comp_sd, simple_mean, simple_sd, zOffset)
      } else {
        out_tibble <- tibble::tibble(comp_mean = NA, comp_sd = NA, simple_mean = NA, simple_sd = NA, zOffset)
      }

      return(out_tibble)
    })) |>
    dplyr::select(-data) |>
    tidyr::unnest(cols = c(month_stats)) |>
    dplyr::ungroup()

  return(out_mean)
}
