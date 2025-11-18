#' @title Acquire NEON data for processing

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a site code and dates, apply the neonUtilities package to download the data from NEON API
#' @param site_name Required. NEON code for a particular site (a string)
#' @param download_date Required. Date where we end getting NEON data. Format: YYYY-MM (can't specify day).  So "2020-05" means it will grab data for the entire 5th month of 2020. (a string). Downloads data for a given month only
#' @param time_frequency Required. Will you be using 30 minute ("30_minute") or 1 minute ("1_minute") recorded data? Defaults to 30 minutes.
#' @param provisional Required. Should you use provisional data when downloading? Defaults to FALSE. See \href{https://www.neonscience.org/data-samples/data-management/data-revisions-releases}{NEON Data Releases}. Defaults to FALSE (similar to include.provisional in \link[neonUtilities]{loadByProduct}).

#'
#' @examples
#' \donttest{
#' out_env_data <- acquire_neon_data("SJER","2022-06")
#' }


#' @return A list containing stacked environmental data (`site_data`) and soil properties (`site_megapit`).
#' @export




acquire_neon_data <- function(site_name,
                              download_date,
                              time_frequency = "30_minute",
                              provisional = FALSE) {


  # changelog and author contributions / copyrights
  #   John Zobitz (2021-07-22)
  #     original creation
  #     update to fix auto download (2021-07-25)
  #     2022-06-10: update to correct flags on swc
  #     2024-04-08: update to get namespaces correct
  #     2024-04-10: update to get the swc depths corrected
  #     2024-04-23: update to allow provisional data
  #     2024-05-23: update to prepare for CRAN submission
  #     2024-11-20: update for SWC correction

  .data = NULL  # Appease R CMD Check

  # Define the columns that we are plucking from each dataset:
  column_selectors = c("Mean","Minimum","Maximum","ExpUncert","StdErMean")

  # Stop if we don't specify 1 or 30 minutes
  if (!(time_frequency %in% c("30_minute","1_minute"))) {
    stop("Time frequency must be 30 minute (`30_minute`)or 1 minute (`1_minute`). Please revise.")
  }

  # Extract out the download time
  download_time <- stringr::str_extract(time_frequency,pattern="^[:digit:]+(?=_)")


  site_megapit <- neonUtilities::loadByProduct(dpID="DP1.00096.001",
                                               site=site_name,
                                               package="expanded",
                                               check.size = FALSE,
                                               include.provisional = provisional)


  site_temp <- neonUtilities::loadByProduct(dpID="DP1.00041.001",
                                            site=site_name,
                                            startdate=download_date,
                                            enddate=download_date,
                                            timeIndex = download_time,
                                            package="expanded",
                                            check.size = FALSE,
                                            include.provisional = provisional)


  # 11-20-24: We use a post-hoc correction for soil swc for calibration. Should revert back after release 2024?

  # site_swc <- neonUtilities::loadByProduct(dpID="DP1.00094.001",
  #                                          site=site_name,
  #                                          startdate=download_date,
  #                                          enddate=download_date,
  #                                          timeIndex = download_time,
  #                                          package="expanded",
  #                                          check.size = FALSE,
  #                                          include.provisional = provisional)
  # Then correct the swc
  site_swc <- reprocess_vswc(site_name,download_date)

  # Remove original swc and overwrite it with the corrected ones
  site_swc2 <- site_swc |>
    purrr::list_assign(SWS_30_minute = rlang::zap())


  names(site_swc2)[names(site_swc2) == "SWS_30_minute_corr"] <- "SWS_30_minute"

  #### Calibration correction end

   # Then correct the swc
  site_swc <- swc_correct(site_swc,site_name,download_date)



  site_press <- neonUtilities::loadByProduct(dpID="DP1.00004.001",
                                             site=site_name,
                                             startdate=download_date,
                                             enddate=download_date,
                                             timeIndex = download_time,
                                             package="expanded",
                                             check.size = FALSE,
                                             include.provisional = provisional)

  site_co2 <- neonUtilities::loadByProduct(dpID="DP1.00095.001",
                                           site=site_name,
                                           startdate=download_date,
                                           enddate=download_date,
                                           timeIndex = download_time,
                                           package="expanded",
                                           include.provisional = provisional,
                                           check.size = FALSE)



  # Process each site measurement
    co2 <- site_co2 |>
      purrr::pluck(paste0("SCO2C_",time_frequency)) |>
      dplyr::select(tidyselect::all_of(c("domainID","siteID","horizontalPosition","verticalPosition","startDateTime","finalQF")),tidyselect::matches(stringr::str_c("soilCO2concentration",column_selectors))) |>
      dplyr::rename(soilCO2concentrationFinalQF = tidyselect::all_of("finalQF"))


    # Determine a data frame of the different horizontal and vertical positions
    co2_positions <- site_co2 |>
      purrr::pluck(paste0("sensor_positions_","00095"))

    # Add on the positions for co2
    co2 <- determine_position(co2_positions,co2) |>
      dplyr::ungroup()

    # Apply monthly means
    co2_monthly_mean <- compute_monthly_mean(co2)

    temperature <- site_temp |>
      purrr::pluck(paste0("ST_",time_frequency)) |>
      dplyr::select(tidyselect::all_of(c("domainID","siteID","horizontalPosition","verticalPosition","startDateTime","finalQF")),tidyselect::matches(stringr::str_c("soilTemp",column_selectors)))  |>
      dplyr::rename(soilTempFinalQF = tidyselect::all_of("finalQF"))

    # Determine a data frame of the different horizontal and vertical positions
    temperature_positions <- site_temp |>
      purrr::pluck(paste0("sensor_positions_","00041"))


    # Add on the positions for temperature
    temperature <- determine_position(temperature_positions,temperature) |>
      dplyr::ungroup()


    # Apply monthly means
    temperature_monthly_mean <- compute_monthly_mean(temperature)

    swc <- site_swc |>
      purrr::pluck(paste0("SWS_",time_frequency)) |>
      dplyr::select(tidyselect::all_of(c("domainID","siteID","horizontalPosition","verticalPosition","startDateTime","VSWCFinalQF")),tidyselect::matches(stringr::str_c("VSWC",column_selectors)))


    # Determine a data frame of the different horizontal and vertical positions

    swc_positions <- site_swc |>
      purrr::pluck(paste0("sensor_positions_","00094"))

    # Add on the positions for swc
    swc <- determine_position(swc_positions,swc) |>
      dplyr::ungroup()




    # Apply monthly means
    swc_monthly_mean <- compute_monthly_mean(swc)

    time_frequency_bp <- dplyr::if_else(time_frequency == "30_minute","30min","1min")

    pressure <- site_press |>
      purrr::pluck(paste0("BP_",time_frequency_bp)) |>
      dplyr::select(tidyselect::all_of(c("domainID","siteID","horizontalPosition","verticalPosition","startDateTime","staPresFinalQF")),tidyselect::matches(stringr::str_c("staPres",column_selectors)))

    pressure_positions <- site_press |>
      purrr::pluck(paste0("sensor_positions_","00004"))


    # Add on the positions for pressure
    pressure <- determine_position(pressure_positions,pressure) |>
      dplyr::ungroup()

    # Apply monthly means - we adjust the monthly mean here to allow for a looser threshold.
    pressure_monthly_mean <- compute_monthly_mean(pressure,time_horizon = 10)


    # Put everything in a nested data frame
    site_data <- tibble::tibble(
      measurement=c("soilCO2concentration","VSWC","soilTemp","staPres"),
      data = list(co2,swc,temperature,pressure),
      monthly_mean = list(co2_monthly_mean,swc_monthly_mean,temperature_monthly_mean,pressure_monthly_mean)) |>
      dplyr::mutate(data = purrr::map(.x=.data[["data"]],.f=~(.x |> dplyr::mutate(startDateTime = lubridate::force_tz(.data[["startDateTime"]],tzone="UTC"))))) # Make sure the time zone stamp is in universal time

    return(list(site_data=site_data,site_megapit=site_megapit))


}
