#' @title Acquire NEON data for processing

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a site code and dates, apply the neonUtilities package to download the data from NEON API
#' @param site_name Required. NEON code for a particular site (a string)
#' @param download_date Required. Date where we end getting NEON data. Format: YYYY-MM (can't specify day).  So "2020-05" means it will grab data for the entire 5th month of 2020. (a string). Downloads data for a given month only
#' @param data_file_name Required. Path of location for save file. Must end in .Rda or .csv - otherwise exits gracefully. Note: Rda files save both the environmental measurements and megapit data as 2 nested data frames. .csv files save only the environmental data (including monthly means) as two separate csv files (not the megapit data)
#' @param time_frequency Required. Will you be using 30 minute ("30") or 1 minute ("1") recorded data? Defaults to 30 minutes.
#' @param column_selectors Required. Types of measurements we will be computing (typically column_selectors = c("Mean","Minimum","Maximum","ExpUncert","StdErMean"))

#'
#' @example acquire_neon_data("SJER","2020-05","my-file.Rda")
#'
#' @return Nothing is returned - the file is saved to the location provided

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation
#     update to fix auto download (2021-07-25)
#     2022-06-10: update to correct flags on swc
#     2024-04-08: update to get namespaces correct
#     2024-04-10: update to get the swc depths corrected

acquire_neon_data <- function(site_name,
                              download_date,
                              data_file_name,
                              time_frequency = "30_minute",
                              column_selectors = c("Mean","Minimum","Maximum","ExpUncert","StdErMean")
                              ) {


  # Stop if we don't specify 1 or 30 minutes
  if (!(time_frequency %in% c("30_minute","1_minute"))) {
    stop("Time frequency must be 30 minute (`30_minute`)or 1 minute (`1_minute`). Please revise.")
  }

  # Extract out the download time
  download_time <- stringr::str_extract(time_frequency,pattern="^[:digit:]+(?=_)")

  # Get the save file extension and do a quick check
  extension_name <- stringr::str_extract(data_file_name,pattern="(?<=\\.).{3}$")
  if (!(extension_name %in% c("csv","Rda","rda"))) {
    stop("Save file name extension must be a Rdata file (Rda) or comma separated file (csv). Please revise.")
  }

  site_megapit <- neonUtilities::loadByProduct(dpID="DP1.00096.001",
                                               site=site_name,
                                               package="expanded",
                                               check.size = F)


  site_temp <- neonUtilities::loadByProduct(dpID="DP1.00041.001",
                                            site=site_name,
                                            startdate=download_date,
                                            enddate=download_date,
                                            timeIndex = download_time,
                                            package="expanded",
                                            check.size = F)


  site_swc <- neonUtilities::loadByProduct(dpID="DP1.00094.001",
                                           site=site_name,
                                           startdate=download_date,
                                           enddate=download_date,
                                           timeIndex = download_time,
                                           package="expanded",
                                           check.size = F)
  # Then correct the swc
  site_swc <- swc_correct(site_swc,site_name,start_date)



  site_press <- neonUtilities::loadByProduct(dpID="DP1.00004.001",
                                             site=site_name,
                                             startdate=download_date,
                                             enddate=download_date,
                                             timeIndex = download_time,
                                             package="expanded",
                                             check.size = F)

  site_co2 <- neonUtilities::loadByProduct(dpID="DP1.00095.001",
                                           site=site_name,
                                           startdate=download_date,
                                           enddate=download_date,
                                           timeIndex = download_time,
                                           package="expanded",
                                           check.size = F)



  # Process each site measurement
    co2 <- site_co2 |>
      purrr::pluck(paste0("SCO2C_",time_frequency)) |>
      dplyr::select(domainID,siteID,horizontalPosition,verticalPosition,startDateTime,tidyr::matches(stringr::str_c("soilCO2concentration",column_selectors)),finalQF) |>
      dplyr::rename(soilCO2concentrationFinalQF = finalQF)


    # Determine a data frame of the different horizontal and vertical positions
    co2_positions <- site_co2 |>
      purrr::pluck(paste0("sensor_positions_","00095"))

    # Add on the positions for co2
    co2 <- determine_position(co2_positions,co2)

    # Apply monthly means
    co2_monthly_mean <- compute_monthly_mean(co2)

    temperature <- site_temp |>
      purrr::pluck(paste0("ST_",time_frequency)) |>
      dplyr::select(domainID,siteID,horizontalPosition,verticalPosition,startDateTime,tidyr::matches(stringr::str_c("soilTemp",column_selectors)),finalQF)  |>
      dplyr::rename(soilTempFinalQF = finalQF)

    # Determine a data frame of the different horizontal and vertical positions
    temperature_positions <- site_temp |>
      purrr::pluck(paste0("sensor_positions_","00041"))


    # Add on the positions for temperature
    temperature <- determine_position(temperature_positions,temperature)


    # Apply monthly means
    temperature_monthly_mean <- compute_monthly_mean(temperature)

    swc <- site_swc |>
      purrr::pluck(paste0("SWS_",time_frequency)) |>
      dplyr::select(domainID,siteID,horizontalPosition,verticalPosition,startDateTime,matches(stringr::str_c("VSWC",column_selectors)),VSWCFinalQF)


    # Determine a data frame of the different horizontal and vertical positions

    swc_positions <- site_swc |>
      purrr::pluck(paste0("sensor_positions_","00094"))

    # Add on the positions for swc
    swc <- determine_position(swc_positions,swc)




    # Apply monthly means
    swc_monthly_mean <- compute_monthly_mean(swc)

    time_frequency_bp <- dplyr::if_else(time_frequency == "30_minute","30min","1min")

    pressure <- site_press |>
      purrr::pluck(paste0("BP_",time_frequency_bp)) |>
      dplyr::select(domainID,siteID,horizontalPosition,verticalPosition,startDateTime,matches(stringr::str_c("staPres",column_selectors)),staPresFinalQF)

    pressure_positions <- site_press |>
      purrr::pluck(paste0("sensor_positions_","00004"))


    # Add on the positions for pressure
    pressure <- determine_position(pressure_positions,pressure)

    # Apply monthly means
    pressure_monthly_mean <- compute_monthly_mean(pressure)


    # Put everything in a nested data frame
    site_data <- tibble::tibble(
      data = list(co2,swc,temperature,pressure),
      monthly_mean = list(co2_monthly_mean,swc_monthly_mean,temperature_monthly_mean,pressure_monthly_mean),
      measurement=c("soilCO2concentration","VSWC","soilTemp","staPres")) |>
      dplyr::mutate(data = purrr::map(.x=data,.f=~(.x |> dplyr::mutate(startDateTime = lubridate::force_tz(startDateTime,tzone="UTC"))))) # Make sure the time zone stamp is in universal time


    # Now start saving
    location_name <- stringr::str_extract(data_file_name,pattern=".+(?=\\..{3}$)")
    if(extension_name == "csv") {
      site_env_data <- site_data |>
        dplyr::select(-monthly_mean) |>
        tidyr::unnest(cols=everything())

      # Write the environmental data
      readr::write_csv(site_env_data,file = data_file_name)

      # Save the monthly mean data as a separate file
      site_mm <- site_data |>
        dplyr::select(-data) |>
        tidyr::unnest(cols=everything())

      readr::write_csv(site_mm,file = paste0(location_name,"-monthly_mean.",extension_name))

    } else{
      save(site_data,site_megapit,file=data_file_name)
    }



}
