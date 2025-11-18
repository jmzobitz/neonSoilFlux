#' @title Internal helper function to reprocess SWC data from calibration data
#'
#' @author
#' John Zobitz \email{zobitz@augsburg.edu},

#' @description
#' Apply correct calibration data functions for SWC data - see https://data.neonscience.org/data-products/DP1.00094.001
#' @param site_name Required. NEON code for a particular site (a string)
#' @param download_date Required. Date where we end getting NEON data. Format: YYYY-MM (can't specify day).  So "2020-05" means it will grab data for the entire 5th month of 2020. (a string). Downloads data for a given month only

#' @return A data frame of startDateTime, horizontalPosition, and the associated QF flag.

#' @examples
#' \donttest{
#' out_swc <- reprocess_vswc("SJER","2022-06")
#' }


#' @return A list containing stacked swc data





reprocess_vswc <- function(site_name,download_date) {

  # See: https://data.neonscience.org/data-products/DP1.00094.001
  # Replace data with NA to prevent any carryover to next loop
  #swc <- NA
  #SWS_30_minute <- NA
  #sensor_positions_00094 <- NA
  #neonBoulded60Cal <- NA
  #neonCal <- NA

  # download the data
  swc <- neonUtilities::loadByProduct(dpID="DP1.00094.001", site=site_name, startdate=download_date, enddate=download_date, package="expanded", timeIndex="30", check.size=F, nCores=3,include.provisional = TRUE)


  #list2env(swc, .GlobalEnv)
  SWS_30_minute <- swc$SWS_30_minute
  # Change dates to posixct
  SWS_30_minute$startDateTime <- as.POSIXct(SWS_30_minute$startDateTime, tz="GMT")
  startYYYYMM <- substr(SWS_30_minute$startDateTime[1], 1, 7)


#  # Read in NEON calibrations - this is done in data-raw/swc_corrections.R
#  neonCal <- read.csv("data/raw/SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)
#  # Read in NEON bounded60 calibrations
#  neonBoulded60Cal <- read.csv("data/raw/Bounded60_SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)


  # Identify rows calculated with the Sentek manufacturer calibration based on calDefaultQM and VSWCExpUncert
  # defaultCalRows <- base::intersect(which(SWS_30_minute$calDefaultQM > 0), which(SWS_30_minute$VSWCExpUncert > 0.21)) # filtering by which(SWS_30_minute$VSWCExpUncert > 0.21) was causing rows to be missed in recent parts of time series (after ~2020-2021)
  defaultCalRows <- which(SWS_30_minute$calDefaultQM > 0)
  # Add rows where defaults calibration was used (uncertainty > 0.21), but calibration coefficients were not reported in the cal file so the calibration flag did not work correctly
  startDateCalFlagP1 <- SWS_30_minute$startDateTime[base::intersect(which(SWS_30_minute$horizontalPosition == "001"), defaultCalRows)][1]
  startDateCalFlagP2 <- SWS_30_minute$startDateTime[base::intersect(which(SWS_30_minute$horizontalPosition == "002"), defaultCalRows)][1]
  startDateCalFlagP3 <- SWS_30_minute$startDateTime[base::intersect(which(SWS_30_minute$horizontalPosition == "003"), defaultCalRows)][1]
  startDateCalFlagP4 <- SWS_30_minute$startDateTime[base::intersect(which(SWS_30_minute$horizontalPosition == "004"), defaultCalRows)][1]
  startDateCalFlagP5 <- SWS_30_minute$startDateTime[base::intersect(which(SWS_30_minute$horizontalPosition == "005"), defaultCalRows)][1]
  defaultCalRows <- append(defaultCalRows, base::intersect(base::intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP1), which(SWS_30_minute$horizontalPosition == "001")), which(SWS_30_minute$VSWCExpUncert > 0.21)))
  defaultCalRows <- append(defaultCalRows, base::intersect(base::intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP2), which(SWS_30_minute$horizontalPosition == "002")), which(SWS_30_minute$VSWCExpUncert > 0.21)))
  defaultCalRows <- append(defaultCalRows, base::intersect(base::intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP3), which(SWS_30_minute$horizontalPosition == "003")), which(SWS_30_minute$VSWCExpUncert > 0.21)))
  defaultCalRows <- append(defaultCalRows, base::intersect(base::intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP4), which(SWS_30_minute$horizontalPosition == "004")), which(SWS_30_minute$VSWCExpUncert > 0.21)))
  defaultCalRows <- append(defaultCalRows, base::intersect(base::intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP5), which(SWS_30_minute$horizontalPosition == "005")), which(SWS_30_minute$VSWCExpUncert > 0.21)))


  # Calculate sensor scaled frequency for the default calibration rows (based on Sentek Calibration Manual). Multiply water content by 100 to get % vol units
  SWS_30_minute$sf_mean <- NA
  SWS_30_minute$sf_mean[defaultCalRows] <- (0.1957 * ((SWS_30_minute$VSWCMean[defaultCalRows]*100)^0.404)) + 0.02852

  SWS_30_minute$sf_minimum <- NA
  SWS_30_minute$sf_minimum[defaultCalRows] <- (0.1957 * ((SWS_30_minute$VSWCMinimum[defaultCalRows]*100)^0.404)) + 0.02852

  SWS_30_minute$sf_maximum <- NA
  SWS_30_minute$sf_maximum[defaultCalRows] <- (0.1957 * ((SWS_30_minute$VSWCMaximum[defaultCalRows]*100)^0.404)) + 0.02852

  SWS_30_minute$sf_stder <- NA
  SWS_30_minute$sf_stder[defaultCalRows] <- (0.1957 * ((SWS_30_minute$VSWCStdErMean[defaultCalRows]*100)^0.404)) + 0.02852

  # Identify rows calculated with the NEON calibration based on calDefaultQM and VSWCExpUncert
  neonCalRowsP1D1 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "501"))
  neonCalRowsP2D1 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "501"))
  neonCalRowsP3D1 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "501"))
  neonCalRowsP4D1 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "501"))
  neonCalRowsP5D1 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "501"))
  ################################# Measurement level 2
  neonCalRowsP1D2 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "502"))
  neonCalRowsP2D2 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "502"))
  neonCalRowsP3D2 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "502"))
  neonCalRowsP4D2 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "502"))
  neonCalRowsP5D2 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "502"))
  ################################# Measurement level 3
  neonCalRowsP1D3 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "503"))
  neonCalRowsP2D3 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "503"))
  neonCalRowsP3D3 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "503"))
  neonCalRowsP4D3 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "503"))
  neonCalRowsP5D3 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "503"))
  ################################# Measurement level 4
  neonCalRowsP1D4 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "504"))
  neonCalRowsP2D4 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "504"))
  neonCalRowsP3D4 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "504"))
  neonCalRowsP4D4 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "504"))
  neonCalRowsP5D4 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "504"))
  ################################# Measurement level 5
  neonCalRowsP1D5 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "505"))
  neonCalRowsP2D5 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "505"))
  neonCalRowsP3D5 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "505"))
  neonCalRowsP4D5 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "505"))
  neonCalRowsP5D5 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "505"))
  ################################# Measurement level 6
  neonCalRowsP1D6 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "506"))
  neonCalRowsP2D6 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "506"))
  neonCalRowsP3D6 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "506"))
  neonCalRowsP4D6 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "506"))
  neonCalRowsP5D6 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "506"))
  ################################# Measurement level 7
  neonCalRowsP1D7 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "507"))
  neonCalRowsP2D7 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "507"))
  neonCalRowsP3D7 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "507"))
  neonCalRowsP4D7 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "507"))
  neonCalRowsP5D7 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "507"))
  ################################# Measurement level 8
  neonCalRowsP1D8 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "001"))
                               , which(SWS_30_minute$verticalPosition == "508"))
  neonCalRowsP2D8 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "002"))
                               , which(SWS_30_minute$verticalPosition == "508"))
  neonCalRowsP3D8 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "003"))
                               , which(SWS_30_minute$verticalPosition == "508"))
  neonCalRowsP4D8 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "004"))
                               , which(SWS_30_minute$verticalPosition == "508"))
  neonCalRowsP5D8 <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == "005"))
                               , which(SWS_30_minute$verticalPosition == "508"))
  # Create vector of plots and measurement levels where soil moisture will be recalculated
  neonCals <- ls()[grep("neonCalRowsP", ls())]


  # Calculate sensor scaled frequency for the neon calibration rows (based on neonCal data frame)
  for(i in 1:length(neonCals)){
    # Find the row in the calibration data frame corresponding to the sensor
    row <- base::intersect(base::intersect(grep(site_name, neonCal$site), which(neonCal$plot == substr(neonCals[i], 13, 13))), which(neonCal$measurementLevel == substr(neonCals[i], 15, 15)) )
    # Assign the calibration coefficients
    a <- neonCal$CVALA1[row]
    b <- neonCal$CVALA2[row]
    c <- neonCal$CVALA3[row]
    # Identify the rows where the calibration coefficients will be applied
    if(neonCals[i] == "neonCalRowsP1D1"){
      rows <- neonCalRowsP1D1
    }else if(neonCals[i] == "neonCalRowsP2D1"){
      rows <- neonCalRowsP2D1
    }else if(neonCals[i] == "neonCalRowsP3D1"){
      rows <- neonCalRowsP3D1
    }else if(neonCals[i] == "neonCalRowsP4D1"){
      rows <- neonCalRowsP4D1
    }else if(neonCals[i] == "neonCalRowsP5D1"){
      rows <- neonCalRowsP5D1
    }
    #### Measurement level 2
    if(neonCals[i] == "neonCalRowsP1D2"){
      rows <- neonCalRowsP1D2
    }else if(neonCals[i] == "neonCalRowsP2D2"){
      rows <- neonCalRowsP2D2
    }else if(neonCals[i] == "neonCalRowsP3D2"){
      rows <- neonCalRowsP3D2
    }else if(neonCals[i] == "neonCalRowsP4D2"){
      rows <- neonCalRowsP4D2
    }else if(neonCals[i] == "neonCalRowsP5D2"){
      rows <- neonCalRowsP5D2
    }
    #### Measurement level 3
    if(neonCals[i] == "neonCalRowsP1D3"){
      rows <- neonCalRowsP1D3
    }else if(neonCals[i] == "neonCalRowsP2D3"){
      rows <- neonCalRowsP2D3
    }else if(neonCals[i] == "neonCalRowsP3D3"){
      rows <- neonCalRowsP3D3
    }else if(neonCals[i] == "neonCalRowsP4D3"){
      rows <- neonCalRowsP4D3
    }else if(neonCals[i] == "neonCalRowsP5D3"){
      rows <- neonCalRowsP5D3
    }
    #### Measurement level 4
    if(neonCals[i] == "neonCalRowsP1D4"){
      rows <- neonCalRowsP1D4
    }else if(neonCals[i] == "neonCalRowsP2D4"){
      rows <- neonCalRowsP2D4
    }else if(neonCals[i] == "neonCalRowsP3D4"){
      rows <- neonCalRowsP3D4
    }else if(neonCals[i] == "neonCalRowsP4D4"){
      rows <- neonCalRowsP4D4
    }else if(neonCals[i] == "neonCalRowsP5D4"){
      rows <- neonCalRowsP5D4
    }
    #### Measurement level 5
    if(neonCals[i] == "neonCalRowsP1D5"){
      rows <- neonCalRowsP1D5
    }else if(neonCals[i] == "neonCalRowsP2D5"){
      rows <- neonCalRowsP2D5
    }else if(neonCals[i] == "neonCalRowsP3D5"){
      rows <- neonCalRowsP3D5
    }else if(neonCals[i] == "neonCalRowsP4D5"){
      rows <- neonCalRowsP4D5
    }else if(neonCals[i] == "neonCalRowsP5D5"){
      rows <- neonCalRowsP5D5
    }
    #### Measurement level 6
    if(neonCals[i] == "neonCalRowsP1D6"){
      rows <- neonCalRowsP1D6
    }else if(neonCals[i] == "neonCalRowsP2D6"){
      rows <- neonCalRowsP2D6
    }else if(neonCals[i] == "neonCalRowsP3D6"){
      rows <- neonCalRowsP3D6
    }else if(neonCals[i] == "neonCalRowsP4D6"){
      rows <- neonCalRowsP4D6
    }else if(neonCals[i] == "neonCalRowsP5D6"){
      rows <- neonCalRowsP5D6
    }
    #### Measurement level 7
    if(neonCals[i] == "neonCalRowsP1D7"){
      rows <- neonCalRowsP1D7
    }else if(neonCals[i] == "neonCalRowsP2D7"){
      rows <- neonCalRowsP2D7
    }else if(neonCals[i] == "neonCalRowsP3D7"){
      rows <- neonCalRowsP3D7
    }else if(neonCals[i] == "neonCalRowsP4D7"){
      rows <- neonCalRowsP4D7
    }else if(neonCals[i] == "neonCalRowsP5D7"){
      rows <- neonCalRowsP5D7
    }
    #### Measurement level 8
    if(neonCals[i] == "neonCalRowsP1D8"){
      rows <- neonCalRowsP1D8
    }else if(neonCals[i] == "neonCalRowsP2D8"){
      rows <- neonCalRowsP2D8
    }else if(neonCals[i] == "neonCalRowsP3D8"){
      rows <- neonCalRowsP3D8
    }else if(neonCals[i] == "neonCalRowsP4D8"){
      rows <- neonCalRowsP4D8
    }else if(neonCals[i] == "neonCalRowsP5D8"){
      rows <- neonCalRowsP5D8
    }

    # Back-calculate scaled frequency using the calibration coefficients
    SWS_30_minute$sf_mean[rows] <- (a * ((SWS_30_minute$VSWCMean[rows]*100)^b)) + c
    SWS_30_minute$sf_maximum[rows] <- (a * ((SWS_30_minute$VSWCMaximum[rows]*100)^b)) + c
    SWS_30_minute$sf_minimum[rows] <- (a * ((SWS_30_minute$VSWCMinimum[rows]*100)^b)) + c
    SWS_30_minute$sf_stder[rows] <- (a * ((SWS_30_minute$VSWCStdErMean[rows]*100)^b)) + c

    # prevent carryover to next loop
    row <- NA
    rows <- NA
  }


  # Calculate soil water content (cm3/cm3) using the neon bounded60 calibration coefficients
  SWS_30_minute$correctedVSWCMean <- NA
  SWS_30_minute$correctedVSWCMinimum <- NA
  SWS_30_minute$correctedVSWCMaximum <- NA
  SWS_30_minute$correctedVSWCStdErMean <- NA
  for(i in 1:length(neonCals)){
    # Find the row in the calibration data frame corresponding to the sensor
    row <- base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == substr(neonCals[i], 13, 13))), which(neonBoulded60Cal$measurementLevel == substr(neonCals[i], 15, 15)) )
    # Assign the calibration coefficients
    aBounded <- neonBoulded60Cal$CVALA1[row]
    bBounded <- neonBoulded60Cal$CVALA2[row]
    cBounded <- neonBoulded60Cal$CVALA3[row]
    # Identify the rows where the calibration coefficients will be applied
    if(neonCals[i] == "neonCalRowsP1D1"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "501"))
    }else if(neonCals[i] == "neonCalRowsP2D1"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "501"))
    }else if(neonCals[i] == "neonCalRowsP3D1"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "501"))
    }else if(neonCals[i] == "neonCalRowsP4D1"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "501"))
    }else if(neonCals[i] == "neonCalRowsP5D1"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "501"))
    }
    #### Measurement level 2
    if(neonCals[i] == "neonCalRowsP1D2"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "502"))
    }else if(neonCals[i] == "neonCalRowsP2D2"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "502"))
    }else if(neonCals[i] == "neonCalRowsP3D2"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "502"))
    }else if(neonCals[i] == "neonCalRowsP4D2"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "502"))
    }else if(neonCals[i] == "neonCalRowsP5D2"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "502"))
    }
    #### Measurement level 3
    if(neonCals[i] == "neonCalRowsP1D3"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "503"))
    }else if(neonCals[i] == "neonCalRowsP2D3"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "503"))
    }else if(neonCals[i] == "neonCalRowsP3D3"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "503"))
    }else if(neonCals[i] == "neonCalRowsP4D3"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "503"))
    }else if(neonCals[i] == "neonCalRowsP5D3"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "503"))
    }
    #### Measurement level 4
    if(neonCals[i] == "neonCalRowsP1D4"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "504"))
    }else if(neonCals[i] == "neonCalRowsP2D4"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "504"))
    }else if(neonCals[i] == "neonCalRowsP3D4"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "504"))
    }else if(neonCals[i] == "neonCalRowsP4D4"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "504"))
    }else if(neonCals[i] == "neonCalRowsP5D4"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "504"))
    }
    #### Measurement level 5
    if(neonCals[i] == "neonCalRowsP1D5"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "505"))
    }else if(neonCals[i] == "neonCalRowsP2D5"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "505"))
    }else if(neonCals[i] == "neonCalRowsP3D5"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "505"))
    }else if(neonCals[i] == "neonCalRowsP4D5"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "505"))
    }else if(neonCals[i] == "neonCalRowsP5D5"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "505"))
    }
    #### Measurement level 6
    if(neonCals[i] == "neonCalRowsP1D6"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "506"))
    }else if(neonCals[i] == "neonCalRowsP2D6"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "506"))
    }else if(neonCals[i] == "neonCalRowsP3D6"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "506"))
    }else if(neonCals[i] == "neonCalRowsP4D6"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "506"))
    }else if(neonCals[i] == "neonCalRowsP5D6"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "506"))
    }
    #### Measurement level 7
    if(neonCals[i] == "neonCalRowsP1D7"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "507"))
    }else if(neonCals[i] == "neonCalRowsP2D7"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "507"))
    }else if(neonCals[i] == "neonCalRowsP3D7"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "507"))
    }else if(neonCals[i] == "neonCalRowsP4D7"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "507"))
    }else if(neonCals[i] == "neonCalRowsP5D7"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "507"))
    }
    #### Measurement level 8
    if(neonCals[i] == "neonCalRowsP1D8"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "508"))
    }else if(neonCals[i] == "neonCalRowsP2D8"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "508"))
    }else if(neonCals[i] == "neonCalRowsP3D8"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "508"))
    }else if(neonCals[i] == "neonCalRowsP4D8"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "508"))
    }else if(neonCals[i] == "neonCalRowsP5D8"){
      rows <- base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "508"))
    }

    # Apply the calibration
    SWS_30_minute$correctedVSWCMean[rows] <- (((SWS_30_minute$sf_mean[rows] - cBounded) / aBounded)^(1/bBounded)) / 100
    SWS_30_minute$correctedVSWCMinimum[rows] <- (((SWS_30_minute$sf_minimum[rows] - cBounded) / aBounded)^(1/bBounded)) / 100
    SWS_30_minute$correctedVSWCMaximum[rows] <- (((SWS_30_minute$sf_maximum[rows] - cBounded) / aBounded)^(1/bBounded)) / 100
    SWS_30_minute$correctedVSWCStdErMean[rows] <- (((SWS_30_minute$sf_stder[rows] - cBounded) / aBounded)^(1/bBounded)) / 100

    # prevent carryover to next loop
    row <- NA
    rows <- NA
  }

  # Assign corrected water content final quality flag based on alpha and beta quality metrics, temp test, and science review flags
  # Use temperature test to inform flagging at all sites for measurement levels 1-3, but for deeper measurement levels only use temperature test at sites where >=35 cm soil temperature falls below 0.1 degrees C (based on 2016-April 2022 RELEASE 2023 soil temperature data)
  coldSites <- c("BARR", "HEAL", "TREE", "TOOL", "DEJU", "NOGP", "NIWO", "WOOD", "BONA", "DCFS", "CPER", "YELL", "RMNP", "MOAB", "STER", "BLAN", "ONAQ")
  if(length(grep(site_name, coldSites)) == 1){
    # Identify rows with higher alpha metric, but that pass all other tests (beta, temp test, science review)
    goodRows <- base::intersect(base::intersect(base::intersect(base::intersect(which(SWS_30_minute$VSWCAlphaQM < 30),
                                                        which(SWS_30_minute$VSWCBetaQM < 20)),
                                              c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0),
                                                which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=TRUE))),
                                    which(SWS_30_minute$tempPassQM > 0)),
                          which(SWS_30_minute$tempFailQM == 0))
  }else{
    goodRowsDeep <- base::intersect(base::intersect(base::intersect(which(SWS_30_minute$VSWCAlphaQM < 30),
                                                  which(SWS_30_minute$VSWCBetaQM < 20)),
                                        c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0),
                                          which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=T))),
                              which(SWS_30_minute$verticalPosition >= 504))
    goodRowsShallow <- base::intersect(base::intersect(base::intersect(base::intersect(base::intersect(which(SWS_30_minute$VSWCAlphaQM < 30),
                                                                         which(SWS_30_minute$VSWCBetaQM < 20)),
                                                               c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0),
                                                                 which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=T))),
                                                     which(SWS_30_minute$verticalPosition < 504)),
                                           which(SWS_30_minute$tempPassQM > 0)),
                                 which(SWS_30_minute$tempFailQM == 0))
    goodRows <- c(goodRowsShallow, goodRowsDeep)
  }
  SWS_30_minute$correctedVSWCFinalQF <- 1
  SWS_30_minute$correctedVSWCFinalQF[goodRows] <- 0





  # Assign measurement uncertainty to unflagged rows. NOTE this does not include changes in water content within the averaging period
  SWS_30_minute$correctedVSWCExpUncert <- NA
  goodP1D1 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  goodP2D1 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  goodP3D1 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  goodP4D1 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  goodP5D1 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D1] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 1) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D1] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 1) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D1] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 1) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D1] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 1) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D1] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 1) )] *2
  #### Measurement level 2
  goodP1D2 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "502")), goodRows)
  goodP2D2 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "502")), goodRows)
  goodP3D2 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "502")), goodRows)
  goodP4D2 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "502")), goodRows)
  goodP5D2 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "502")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D2] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 2) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D2] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 2) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D2] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 2) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D2] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 2) )] *2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D2] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 2) )] *2
  #### Measurement level 3
  goodP1D3 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "503")), goodRows)
  goodP2D3 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "503")), goodRows)
  goodP3D3 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "503")), goodRows)
  goodP4D3 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "503")), goodRows)
  goodP5D3 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "503")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D3] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 3) )] *2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D3] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 3) )] *2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D3] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 3) )] *2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D3] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 3) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D3] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 3) )] *2
  #### Measurement level 4
  goodP1D4 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "504")), goodRows)
  goodP2D4 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "504")), goodRows)
  goodP3D4 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "504")), goodRows)
  goodP4D4 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "504")), goodRows)
  goodP5D4 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "504")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D4] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 4) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D4] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 4) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D4] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 4) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D4] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 4) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D4] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 4) )]*2
  #### Measurement level 5
  goodP1D5 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "505")), goodRows)
  goodP2D5 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "505")), goodRows)
  goodP3D5 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "505")), goodRows)
  goodP4D5 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "505")), goodRows)
  goodP5D5 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "505")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D5] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 5) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D5] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 5) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D5] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 5) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D5] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 5) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D5] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 5) )]*2
  #### Measurement level 6
  goodP1D6 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "506")), goodRows)
  goodP2D6 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "506")), goodRows)
  goodP3D6 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "506")), goodRows)
  goodP4D6 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "506")), goodRows)
  goodP5D6 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "506")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D6] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 6) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D6] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 6) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D6] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 6) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D6] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 6) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D6] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 6) )]*2
  #### Measurement level 7
  goodP1D7 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "507")), goodRows)
  goodP2D7 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "507")), goodRows)
  goodP3D7 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "507")), goodRows)
  goodP4D7 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "507")), goodRows)
  goodP5D7 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "507")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D7] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 7) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D7] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 7) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D7] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 7) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D7] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 7) )]*2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D7] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 7) )]*2
  #### Measurement level 8
  goodP1D8 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "001"), which(SWS_30_minute$verticalPosition == "508")), goodRows)
  goodP2D8 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "002"), which(SWS_30_minute$verticalPosition == "508")), goodRows)
  goodP3D8 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "003"), which(SWS_30_minute$verticalPosition == "508")), goodRows)
  goodP4D8 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "004"), which(SWS_30_minute$verticalPosition == "508")), goodRows)
  goodP5D8 <- base::intersect(base::intersect(which(SWS_30_minute$horizontalPosition == "005"), which(SWS_30_minute$verticalPosition == "508")), goodRows)
  SWS_30_minute$correctedVSWCExpUncert[goodP1D8] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 8) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP2D8] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 2)), which(neonBoulded60Cal$measurementLevel == 8) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP3D8] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 3)), which(neonBoulded60Cal$measurementLevel == 8) )] *2
  SWS_30_minute$correctedVSWCExpUncert[goodP4D8] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 4)), which(neonBoulded60Cal$measurementLevel == 8) )] * 2
  SWS_30_minute$correctedVSWCExpUncert[goodP5D8] <- neonBoulded60Cal$U_CVALA3[base::intersect(base::intersect(grep(site_name, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 5)), which(neonBoulded60Cal$measurementLevel == 8) )] * 2


  # Overwrite any NAs in the ExpUncert with the maximum uncertianty
  SWS_30_minute$correctedVSWCExpUncert[is.na( SWS_30_minute$correctedVSWCExpUncert)] <- max(SWS_30_minute$correctedVSWCExpUncert,na.rm=TRUE)

  # Change names
  SWS_30_minute$VSWCMean <- SWS_30_minute$correctedVSWCMean
  SWS_30_minute$VSWCMinimum <- SWS_30_minute$correctedVSWCMinimum
  SWS_30_minute$VSWCMaximum <- SWS_30_minute$correctedVSWCMaximum
  SWS_30_minute$VSWCStdErMean <- SWS_30_minute$correctedVSWCStdErMean
  SWS_30_minute$VSWCExpUncert <- SWS_30_minute$correctedVSWCExpUncert
  SWS_30_minute$VSWCFinalQF <- SWS_30_minute$correctedVSWCFinalQF


  # Assign out names:
  swc$SWS_30_minute_corr <- SWS_30_minute |>
    dplyr::select(-correctedVSWCMean,-correctedVSWCMaximum,-correctedVSWCMinimum,-correctedVSWCStdErMean,-correctedVSWCExpUncert,-correctedVSWCFinalQF)

  return(swc)
}
