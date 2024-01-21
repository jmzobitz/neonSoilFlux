##############################################################################################
#' @title Calculate soil CO2 fluxes in NEON soil plots

#' @author 
#' Edward Ayres \email{eayres@battelleecology.org}

#' @description 
#' Definition function. Calulate soil CO2 fluxes at a NEON soil plot for a particular year and month


#' @param site Required. Character string. The NEON site code (e.g. 'CPER').
#' @param idDpMain Character string. The main DP ID consisting of the 3-character data product level, the 5-digit 
#' data product ID, and the data product revision, separted by periods (e.g. "DP1.00001.001").
#' @param locHor Required. Character string. The 3-digit horizontal location index of the data product (e.g. '000' for the tower)
#' @param locVer Required. Character string. The 3-digit vertical location index of the data product (e.g. '030' for tower level 3)
#' @param wndwAgr Required. Character string. The 3-digit timing (aggregation) index of the data product (e.g. '030' for 3-minute average data) 
#' @param year Required. Numeric value. The year in which to search for data (e.g. 2017).
#' @param mnth Required. Numeric value. The month in which to search for data (e.g. 1).
#' @param Pack Optional. Character string. The download package. Options are 'basic' (default) and 'expanded'.
#' @param UrlBase Optional. Character string. The base API URL. Defaults to 'http://data.neonscience.org/api/v0'

#' @return A data frame of the requested data. If more than one file is returned from the api, a list will be returned with one 
#' data frame per file. 

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples 


#' @seealso 

#' @export

# changelog and author contributions / copyrights
#   Ed Ayres (2019-03-26)
#     original creation
#   Ed Ayres (2019-05-15)
#     initial soil CO2 efflux work flow completed
#   Ed Ayres (2019-05-22)
#     Updating data quality flagging algorithms
#   Ed Ayres (2019-06-21)
#     Interpolating soil temperature across depths
#   Ed Ayres (2019-07-09)
#     Moved soil temperature and soil moisture interpolations to seperate functions.
##############################################################################################


soilResp <- function(site, year, mnth, locHor, wndwAgr){
  
  # Clear data frames
  data <- c()
  geoCO2 <- c()
  geoSwc <- c()
  geoTemp <- c()
  mgp <- c()
  tempInterp <- c()
  swcInterp <- c()
  tempInterp2 <- c()
  swcInterp2 <- c()
  diffu <- c()
  
  # Load required packages
  library(dplyr)
  library(httr)
  library(jsonlite)
  require(lattice)
  library(RColorBrewer)
  source("pack/soilResp/soil.resp.itpl.R")
  source("pack/soilResp/soil.resp.mega.R")
  
  # Assign values to constants
  R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
  absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)
  
  # Static inputs for downloading API data
  idDpMainCo2 <- 'DP1.00095.001'
  idDpMainTemp <- 'DP1.00041.001'
  idDpMainSwc <- 'DP1.00094.001'
  idDpMainPres <- 'DP1.00004.001'
  locVerCo2 <- c('501', "502", "503") # ordered from shallowest to deepest
  locVerTemp <- c('501', "502", "503", "504", "505", "506", "507", "508", "509")
  locVerSwc <- c('501', "502", "503", "504", "505", "506", "507", "508")
  locVerPres <- c("015", "025", "035") # specify all possible locVer values that could occur
  locHorPres <- "000"
  Pack <- 'expanded'
  
  #################################################
  #################################################
  #### Calculate soil CO2 concentration (micromol m-3) profile
  #################################################
  #################################################
  
  # Read in soil CO2 data
  dataCo2D1 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainCo2,locHor=locHor,locVer=locVerCo2[1],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  dataCo2D2 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainCo2,locHor=locHor,locVer=locVerCo2[2],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  dataCo2D3 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainCo2,locHor=locHor,locVer=locVerCo2[3],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  
  # Read in barometric pressure data
  req.pres <- GET(paste0("http://data.neonscience.org/api/v0/products/", idDpMainPres))
  avail.pres <- fromJSON(content(req.pres, as="text"), simplifyDataFrame=T, flatten=T)
  # Get the list of available data URLs
  temp.urls <- unlist(avail.pres$data$siteCodes$availableDataUrls)
  if(nchar(mnth)<2){
    tmp <- GET(temp.urls[intersect(grep(site, temp.urls), grep(paste(year, mnth, sep="-0"), temp.urls)) ])
  }else{
    tmp <- GET(temp.urls[intersect(grep(site, temp.urls), grep(paste(year, mnth, sep="-"), temp.urls)) ])
  }
  tmp.files <- fromJSON(content(tmp, as="text"))
  dataPres <- read.delim(tmp.files$data$files$url
                           [intersect(grep(paste0(substr(wndwAgr,3, 3), "min"), tmp.files$data$files$name),
                                      grep("expanded", tmp.files$data$files$name))][1], sep=",")
  # dataPres <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainPres,locHor=locHorPres,locVer=locVerPres[1],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  # dataPres <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainPres,locHor=locHorPres,locVer=locVerPres[2],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  # dataPres <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainPres,locHor=locHorPres,locVer=locVerPres[3],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
  
  # Append column names with data type
  colnames(dataCo2D1)[!grepl(pattern = "DateTime", colnames(dataCo2D1))] <- 
    paste0(colnames(dataCo2D1)[!grepl(pattern = "DateTime", colnames(dataCo2D1))], "Co2D1")
  colnames(dataCo2D2)[!grepl(pattern = "DateTime", colnames(dataCo2D2))] <- 
    paste0(colnames(dataCo2D2)[!grepl(pattern = "DateTime", colnames(dataCo2D2))], "Co2D2")
  colnames(dataCo2D3)[!grepl(pattern = "DateTime", colnames(dataCo2D3))] <- 
    paste0(colnames(dataCo2D3)[!grepl(pattern = "DateTime", colnames(dataCo2D3))], "Co2D3")
  
  colnames(dataPres)[!grepl(pattern = "DateTime", colnames(dataPres))] <- 
    paste0(colnames(dataPres)[!grepl(pattern = "DateTime", colnames(dataPres))], "Pres")
  
  
  # Function to merge multiple data frames into one
  MyMerge <- function(x, y){
    df <- merge(x, y, by= c("startDateTime", "endDateTime"), all.x= TRUE, all.y= TRUE)
    return(df)
  }
  
  # Merge data frames into a single data frame
  data <- Reduce(MyMerge, list(dataCo2D1, dataCo2D2, dataCo2D3, dataPres))
  
  # remove data frames that are no longer needed
  rm("dataCo2D1", "dataCo2D2", "dataCo2D3", "dataPres")
  
  # Add site and soil plot number to data frame
  data$site <- site
  data$soilPlot <- locHor
  
  # Read in CO2 sensor depth data. Use " %>% distinct()" to remove any duplicated rows.
  tmp.files <- som::def.neon.api.get.info(Type='data',idDpMain=idDpMainCo2,site=site,year=year,mnth=mnth)
  geoCO2 <- read.delim(tmp.files$data$files$url[grep("sensor_positions", tmp.files$data$files$name)][1], sep=",") %>% distinct()
  
  # Read in soil temperature sensor depth data. Use " %>% distinct()" to remove any duplicated rows.
  tmp.files <- som::def.neon.api.get.info(Type='data',idDpMain=idDpMainTemp,site=site,year=year,mnth=mnth)
  geoTemp <- read.delim(tmp.files$data$files$url[grep("sensor_positions", tmp.files$data$files$name)][1], sep=",") %>% distinct()
  
  # Read in soil moisture sensor depth data. Use " %>% distinct()" to remove any duplicated rows.
  tmp.files <- som::def.neon.api.get.info(Type='data',idDpMain=idDpMainSwc,site=site,year=year,mnth=mnth)
  geoSwc <- read.delim(tmp.files$data$files$url[grep("sensor_positions", tmp.files$data$files$name)][1], sep=",") %>% distinct()
  
  
  # Get interpolated soil temperature and moisture data
  tempInterp <- interp(site = site, year = year, mnth = mnth, locHor = locHor, wndwAgr = wndwAgr, saveInputs = T, variable = "soilTemp")
  swcInterp <- interp(site = site, year = year, mnth = mnth, locHor = locHor, wndwAgr = wndwAgr, saveInputs = T, variable = "soilMoist")
  
  # Append column names with name to differentiate soil temperature from soil moisture data
  colnames(tempInterp)[-c(grep("DateTime", colnames(tempInterp)))] <- paste0("tempInterp_", colnames(tempInterp)[-c(grep("DateTime", colnames(tempInterp)))])
  colnames(swcInterp)[-c(grep("DateTime", colnames(swcInterp)))] <- paste0("swcInterp_", colnames(swcInterp)[-c(grep("DateTime", colnames(swcInterp)))])
  
  # Merge data frames into a single data frame
  data <- Reduce(MyMerge, list(data, tempInterp, swcInterp))
  
  # Convert time to POSIXct
  data$startDateTime <- as.POSIXct(data$startDateTime, format='%Y-%m-%dT%H:%M:%S', tz = "GMT")
  data$endDateTime <- as.POSIXct(data$endDateTime, format='%Y-%m-%dT%H:%M:%S', tz = "GMT")
  
  
  ################################
  #### Make levelplots of soil temperature and soil moisture
  ################################
  
  # Make a new dataframe in tall and skinny format to create a level plot of soil temperature
  # Convert times to POSIXct
  tempInterp$startDateTime <- as.POSIXct(tempInterp$startDateTime, format='%Y-%m-%dT%H:%M:%S', tz = "GMT")
  ## make the colnames numeric-ish
  names(tempInterp)[-c(1:2)] <- sub("tempInterp_D_", "", names(tempInterp)[-c(1:2)])
  ## stack the data
  tempInterp2 <- data.frame(datetime = rep(tempInterp$startDateTime, times = ncol(tempInterp)-2),
                            stack(tempInterp[, -c(1:2)]))
  names(tempInterp2) <- c("startDateTime", "Temperature", "Depth")
  ## make Depth numeric
  tempInterp2 <- transform(tempInterp2, Depth = as.numeric(as.character(Depth))*-1)
  
  # Create a levelplot of soil temperature
  filenameGraphsoilTemp <- paste0("flow/flow.soilResp/soilTemp_", site, "_", locHor, "_", year, "-", mnth, ".png")
  png(filename = filenameGraphsoilTemp, width=800, height=800, res=150)
  levelplot(Temperature ~ startDateTime * Depth, data = tempInterp2, labels = FALSE, region = TRUE, col.regions = heat.colors(100),
            main=paste0(site, " ", locHor, ": ", year, "-", mnth, " Soil temperature (°C)"), ylab="Depth (m)", xlab="Date", ylim=c(-2, -0))
  dev.off()
  
  # Make a new dataframe in tall and skinny format to create a level plot of soil moisture
  # Convert times to POSIXct
  swcInterp$startDateTime <- as.POSIXct(swcInterp$startDateTime, format='%Y-%m-%dT%H:%M:%S', tz = "GMT")
  ## make the colnames numeric-ish
  names(swcInterp)[-c(1:2)] <- sub("swcInterp_D_", "", names(swcInterp)[-c(1:2)])
  ## stack the data
  swcInterp2 <- data.frame(datetime = rep(swcInterp$startDateTime, times = ncol(swcInterp)-2),
                           stack(swcInterp[, -c(1:2)]))
  names(swcInterp2) <- c("startDateTime", "Moisture", "Depth")
  ## make Depth numeric
  swcInterp2 <- transform(swcInterp2, Depth = as.numeric(as.character(Depth))*-1)
  
  # Create a levelplot of soil water content
  filenameGraphsoilMoist <- paste0("flow/flow.soilResp/soilMoist_", site, "_", locHor, "_", year, "-", mnth, ".png")
  png(filename = filenameGraphsoilMoist, width=800, height=800, res=150)
  levelplot(Moisture ~ startDateTime * Depth, data = swcInterp2, labels = FALSE, region = TRUE, col.regions = cm.colors(100),
            main=paste0(site, " ", locHor, ": ", year, "-", mnth, " Soil moisture (cm3 cm-3)"), ylab="Depth (m)", xlab="Date", ylim=c(-2, -0))
  dev.off()
  
  # Remove dataframes that are no longer needed
  rm("swcInterp2", "tempInterp2")
  
  ################################
  #### END: Make levelplots of soil temperature and soil moisture
  ################################
  
  
  
  
  # Interpolate soil CO concentration values over time for missing/bad data if values are present at the time point immediately before and after
  data$soilCO2concentrationMeanCo2D1_Interp <- data$soilCO2concentrationMeanCo2D1
  data$finalQFCo2D1_Interp <- data$finalQFCo2D1
  for(i in 2:(nrow(data)-1)){
    if(!is.na(data$finalQFCo2D1[i]) && !is.na(data$finalQFCo2D1[(i-1)]) && !is.na(data$finalQFCo2D1[(i+1)])){
      if(data$finalQFCo2D1[i] == 1 && sum(data$finalQFCo2D1[(i-1)], data$finalQFCo2D1[(i+1)]) == 0){
        data$soilCO2concentrationMeanCo2D1_Interp[i] <- mean(c(data$soilCO2concentrationMeanCo2D1[(i-1)], data$soilCO2concentrationMeanCo2D1[(i+1)]))
        data$finalQFCo2D1_Interp[i] <- 0
      }
    }
  }
  data$soilCO2concentrationMeanCo2D2_Interp <- data$soilCO2concentrationMeanCo2D2
  data$finalQFCo2D2_Interp <- data$finalQFCo2D2
  for(i in 2:(nrow(data)-1)){
    if(!is.na(data$finalQFCo2D2[i]) && !is.na(data$finalQFCo2D2[(i-1)]) && !is.na(data$finalQFCo2D2[(i+1)])){
      if(data$finalQFCo2D2[i] == 1 && sum(data$finalQFCo2D2[(i-1)], data$finalQFCo2D2[(i+1)]) == 0){
        data$soilCO2concentrationMeanCo2D2_Interp[i] <- mean(c(data$soilCO2concentrationMeanCo2D2[(i-1)], data$soilCO2concentrationMeanCo2D2[(i+1)]))
        data$finalQFCo2D2_Interp[i] <- 0
      }
    }
  }
  data$soilCO2concentrationMeanCo2D3_Interp <- data$soilCO2concentrationMeanCo2D3
  data$finalQFCo2D3_Interp <- data$finalQFCo2D3
  for(i in 2:(nrow(data)-1)){
    if(!is.na(data$finalQFCo2D3[i]) && !is.na(data$finalQFCo2D3[(i-1)]) && !is.na(data$finalQFCo2D3[(i+1)])){
      if(data$finalQFCo2D3[i] == 1 && sum(data$finalQFCo2D3[(i-1)], data$finalQFCo2D3[(i+1)]) == 0){
        data$soilCO2concentrationMeanCo2D3_Interp[i] <- mean(c(data$soilCO2concentrationMeanCo2D3[(i-1)], data$soilCO2concentrationMeanCo2D3[(i+1)]))
        data$finalQFCo2D3_Interp[i] <- 0
      }
    }
  }
  
  
  
  ##### 
  ##### MAY NEED TO UPDATE CODE BELOW TO ACCOUNT FOR SENSORS THAT ARE REINSTALLED AT A DIFFERENT DEPTH
  ##### 
  # Determine which soil temperature measurement level to use with each CO2 measurement level
  x <- c()
  y <- c()
  for(i in 1:(length(locVerCo2))){
    # Identify the depth of the CO2 sensor
    x[i] <- geoCO2[grep(paste0(substr(locHor, 3, 3), ".", locVerCo2[i]), geoCO2$HOR.VER), "zOffset"]
    y[i] <- round(x[i], digits = 2)
  }
  ##### 
  ##### END: MAY NEED TO UPDATE CODE BELOW TO ACCOUNT FOR SENSORS THAT ARE REINSTALLED AT A DIFFERENT DEPTH
  ##### 
  
  # Add interpolated soil temperature at the depth of the CO2 measurements to the data frame
  data$tempAtCo2ML1 <- data[, paste0("tempInterp_D_", substr(y[1], 2, 5))]
  data$tempAtCo2ML2 <- data[, paste0("tempInterp_D_", substr(y[2], 2, 5))]
  data$tempAtCo2ML3 <- data[, paste0("tempInterp_D_", substr(y[3], 2, 5))]
  
  # Convert CO2 concentration from ppm to µmol m-3 units
  data$soilCO2concentrationMeanCo2D1umol <- (data$soilCO2concentrationMeanCo2D1_Interp * data$staPresMeanPres) / (R * (data$tempAtCo2ML1 - absZero) )
  data$soilCO2concentrationMeanCo2D2umol <- (data$soilCO2concentrationMeanCo2D2_Interp * data$staPresMeanPres) / (R * (data$tempAtCo2ML2 - absZero) )
  data$soilCO2concentrationMeanCo2D3umol <- (data$soilCO2concentrationMeanCo2D3_Interp * data$staPresMeanPres) / (R * (data$tempAtCo2ML3 - absZero) )
  
  # Determine the final quality flag for the CO2 concentration data (µmol m-3 units) for CO2 measurement level 1
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]))){
      if(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]) > 0){
        data$finalQFCo2D1umol[i] <- 1
      }else{
        data$finalQFCo2D1umol[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFCo2D1umol[i] <- 1
    }
  }
  # Determine the final quality flag for the CO2 concentration data (µmol m-3 units) for CO2 measurement level 2
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]))){
      if(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]) > 0){
        data$finalQFCo2D2umol[i] <- 1
      }else{
        data$finalQFCo2D2umol[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFCo2D2umol[i] <- 1
    }
  }
  # Determine the final quality flag for the CO2 concentration data (µmol m-3 units) for CO2 measurement level 3
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]))){
      if(sum(data[i, "finalQFCo2D1_Interp"], data[i, "staPresFinalQFPres"]) > 0){
        data$finalQFCo2D3umol[i] <- 1
      }else{
        data$finalQFCo2D3umol[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFCo2D3umol[i] <- 1
    }
  }
  # Set the final quality flag for the CO2 concentration data (µmol m-3 units) to 1 if the value is NA
  data$finalQFCo2D1umol[which(is.na(data$soilCO2concentrationMeanCo2D1umol))] <- 1
  data$finalQFCo2D2umol[which(is.na(data$soilCO2concentrationMeanCo2D2umol))] <- 1
  data$finalQFCo2D3umol[which(is.na(data$soilCO2concentrationMeanCo2D3umol))] <- 1
  # Set the final quality flag for the CO2 concentration data (µmol m-3 units) to 1 if the value is negative
  data$finalQFCo2D1umol[which(data$soilCO2concentrationMeanCo2D1umol < 0)] <- 1
  data$finalQFCo2D2umol[which(data$soilCO2concentrationMeanCo2D2umol < 0)] <- 1
  data$finalQFCo2D3umol[which(data$soilCO2concentrationMeanCo2D3umol < 0)] <- 1
  
  # Plot time series of CO2 concentrations in µmol m-3 units
  maxUmol <- max(c(max(data$soilCO2concentrationMeanCo2D1umol, na.rm = T), 
               max(data$soilCO2concentrationMeanCo2D2umol, na.rm = T), 
               max(data$soilCO2concentrationMeanCo2D3umol, na.rm = T)))
  plot(data$startDateTime, data$soilCO2concentrationMeanCo2D1umol, pch=".", ylim=c(0, maxUmol*1.4), col=rgb(0, 0, 0, 0.1), 
       ylab="Soil CO2 concentration (µmol m-3)", main=paste0(site, " ", locHor, ": ", year, "-", mnth))
  points(data$startDateTime, data$soilCO2concentrationMeanCo2D2umol, pch=".", col=rgb(1, 0.5, 0, 0.1))
  points(data$startDateTime, data$soilCO2concentrationMeanCo2D3umol, pch=".", col=rgb(1, 0, 0, 0.1))
  points(data$startDateTime[which(data$finalQFCo2D1umol == 0)], data$soilCO2concentrationMeanCo2D1umol[which(data$finalQFCo2D1umol == 0)], pch=".", col=rgb(0, 0, 0, 1))
  points(data$startDateTime[which(data$finalQFCo2D2umol == 0)], data$soilCO2concentrationMeanCo2D2umol[which(data$finalQFCo2D2umol == 0)], pch=".", col=rgb(1, 0.5, 0, 1))
  points(data$startDateTime[which(data$finalQFCo2D3umol == 0)], data$soilCO2concentrationMeanCo2D3umol[which(data$finalQFCo2D3umol == 0)], pch=".", col=rgb(1, 0, 0, 1))
  legConcD1 <- paste0(x[1], " m")
  legConcD2 <- paste0(x[2], " m")
  legConcD3 <- paste0(x[3], " m")
  legend("topleft", legend=c(legConcD1, legConcD2, legConcD3), lty=1, col=c("black", "orange", "red"), bty="n")
  
  #################################################
  #################################################
  #### END: Calculate soil CO2 concentration (micromol m-3) profile
  #################################################
  #################################################
  
  
  
  
  
  #################################################
  #################################################
  #### Calculate soil CO2 diffusivity
  #################################################
  #################################################
  
  # Add interpolated soil moisture at the depth of the CO2 measurements to the data frame
  data$swcAtCo2ML1 <- data[, paste0("swcInterp_D_", substr(y[1], 2, 5))]
  data$swcAtCo2ML2 <- data[, paste0("swcInterp_D_", substr(y[2], 2, 5))]
  data$swcAtCo2ML3 <- data[, paste0("swcInterp_D_", substr(y[3], 2, 5))]
  
  # Get megapit data
  mgp <- getMegapit(site = site)
  
  #####
  # Calculate porosity of each soil horizon
  #####
  
  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130, 
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 + 
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################
  
  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density.
  rockVol <- ((mgp$coarseFrag2To5 + mgp$coarseFrag5To20) / 1000) / 2.65
  
  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3.
  porosSub2mm <- 1 - mgp$bulkDensExclCoarseFrag/2.65
  
  # Calculate porosity of the 0-20 mm fraction (cm3 cm-3). Assume no pores within rocks.
  mgp$porVol2To20 <- porosSub2mm * (1 - rockVol)
  
  #####
  # END: Calculate porosity of each soil horizon
  #####
  
  
  # Calculate profiles of air-filled porosity, tortuosity factor, free air CO2 diffusivity, and soil CO2 diffusivity 
  # to the depth of the deepest CO2 sensor. x = soil CO2 sensor depth
  diffuDepths <- seq(0, abs(min(x)), by=0.01)
  for(j in 1:length(diffuDepths)){
    
    # Create column names for air-filled porosity profile
    nameAirFilled <- paste0("porVol2To20AirFilled_", diffuDepths[j])
    
    # Create column names for gas tortuosity factor profile
    nameTort <- paste0("tort_", diffuDepths[j])
    
    # Create column names for CO2 diffusivity in free air (m2 s-1) profile
    nameDiffuFreeAir <- paste0("diffuFreeAir_", diffuDepths[j])
    namesTemp <- paste0("tempInterp_D_", diffuDepths[j])
    
    # Create column names for diffusivity profile
    nameDiffu <- paste0("diffu_", diffuDepths[j])
  
    # Identify the soil horizon corresponding to depth diffuDepths[j]. Use the shallower horizon if the sensor is at the boundary.
    horizon <- intersect(which(diffuDepths[j] >= mgp$horizonTopDepth/100), which(diffuDepths[j] <= mgp$horizonBottomDepth/100))
    if(length(horizon)>1){
      horizon <- horizon[which.min(mgp$horizonTopDepth[horizon])]
    }
    
    # Calculate the air filled porosity
    data[, nameAirFilled] <- mgp$porVol2To20[horizon] - data[, paste0("swcInterp_D_", diffuDepths[j])]
    
    # Calculate gas tortuosity factor based on Millington and Quirk 1961
    data[, nameTort] <- (data[, nameAirFilled]^(10/3)) / (mgp$porVol2To20[horizon]^2)
    
    # Calculate CO2 diffusivity in free air (m2 s-1). 
    data[, nameDiffuFreeAir] <- 0.0000147 * ((data[, namesTemp] - absZero) / (20 - absZero))^1.75 * (data$staPresMeanPres / 101.3)
    
    # Calculate CO2 diffusivity (m2 s-1). 
    data[, nameDiffu] <- data[, nameTort] * data[, nameDiffuFreeAir]
  }
  
  # Remove diffusivities calculated with flagged pressure data
  for(i in 1:nrow(data)){
    if(!is.na(data$staPresFinalQFPres[i])){
      if(data$staPresFinalQFPres[i] > 0){
        data[i, grep("diffu_", colnames(data))] <- NA
      }
    }
  }
  
  # Create level plot of diffusivity
  # Make a new dataframe in tall and skinny format to create a level plot of diffusivity
  diffu <- data.frame(datetime = rep(data$startDateTime, times = length(grep("diffu_", colnames(data))) ),
                            stack(data[, grep("diffu_", colnames(data))]))
  names(diffu) <- c("startDateTime", "diffu", "Depth")
  ## make Depth numeric
  diffu$Depth <- as.numeric(gsub("diffu_", "", diffu$Depth)) * -1
  
  # Create a levelplot of diffusivity
  filenameGraphDiffu <- paste0("flow/flow.soilResp/diffu_", site, "_", locHor, "_", year, "-", mnth, ".png")
  png(filename = filenameGraphDiffu, width=800, height=800, res=150)
  levelplot(diffu ~ startDateTime * Depth, data = diffu, labels = FALSE, region = TRUE, col.regions = heat.colors(100),
            main=paste0(site, " ", locHor, ": ", year, "-", mnth, " Diffusivity (m2 s-1)"), ylab="Depth (m)", xlab="Date", ylim=c(min(diffu$Depth), 0))
  dev.off()
  
  
  #################################################
  #################################################
  #### END: Calculate soil CO2 diffusivity
  #################################################
  #################################################
  
  
  
  
  #################################################
  #################################################
  #### Calculate soil CO2 fluxes
  #################################################
  #################################################
  
  # Calculate diffusivty between measurement levels (use harmonic mean based on Turcu et al. (2005) Vadose Zone J 4, 1161-1169).
  diffuDepthsD12 <- seq(abs(x[1]), abs(x[2]), by=0.01)
  for(i in 1:nrow(data)){
    # Calculate harmonic mean of diffusivity. Add 0.01 m to account for inclusive depths
    data$diffuD12[i] <- (x[1] - x[2] + 0.01) / sum(0.01 / data[i, paste0("diffu_", diffuDepthsD12)])
  }
  diffuDepthsD23 <- seq(abs(x[2]), abs(x[3]), by=0.01)
  for(i in 1:nrow(data)){
    # Calculate harmonic mean of diffusivity. Add 0.01 m to account for inclusive depths
    data$diffuD23[i] <- (x[2] - x[3] + 0.01) / sum(0.01 / data[i, paste0("diffu_", diffuDepthsD23)])
  }
  
  # Calculate diffusive flux between CO2 measurement levels (µmol m-2 s-1)
  data$diffuFluxD12 <- ((data$soilCO2concentrationMeanCo2D2umol - data$soilCO2concentrationMeanCo2D1umol) / (x[1] - x[2]) ) * data$diffuD12
  data$diffuFluxD23 <- ((data$soilCO2concentrationMeanCo2D3umol - data$soilCO2concentrationMeanCo2D2umol) / (x[2] - x[3]) ) * data$diffuD23
  
  # Determine the final quality flag for the diffusive flux between CO2 measurement levels 1 and 2. Diffusivity input is assumed to be good since only unflagged data are used to generate it.
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFCo2D2umol"], data[i, "finalQFCo2D1umol"]))){
      # final quality flag = 1 if one or more of the input variables had a raised final quality flag
      if(sum(data[i, "finalQFCo2D2umol"], data[i, "finalQFCo2D1umol"]) > 0){
        data$finalQFdiffuFluxD12[i] <- 1
      }else{
        data$finalQFdiffuFluxD12[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFdiffuFluxD12[i] <- 1
    }
  }
  # Determine the final quality flag for the diffusive flux between CO2 measurement levels 2 and 3
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFCo2D3umol"], data[i, "finalQFCo2D2umol"]))){
      # final quality flag = 1 if one or more of the input variables had a raised final quality flag
      if(sum(data[i, "finalQFCo2D3umol"], data[i, "finalQFCo2D2umol"]) > 0){
        data$finalQFdiffuFluxD23[i] <- 1
      }else{
        data$finalQFdiffuFluxD23[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFdiffuFluxD23[i] <- 1
    }
  }
  # Set the final quality flag for the diffusive flux to 1 if the value is NA
  data$finalQFdiffuFluxD12[which(is.na(data$diffuFluxD12))] <- 1
  data$finalQFdiffuFluxD23[which(is.na(data$diffuFluxD23))] <- 1
  # Set the final quality flag for the diffusive flux to 1 if the value is a large negative number. <<<Initially setting threshold to -1 µmol m-2 s-1, but may need to update this in the future>>>
  data$finalQFdiffuFluxD12[which(data$diffuFluxD12 < -1)] <- 1
  data$finalQFdiffuFluxD23[which(data$diffuFluxD23 < -1)] <- 1
  
  # Determine the average depth (m) between measurement levels so that this can be assigned to the flux measurement
  diffuFluxD12Depth <- (((x[2]-x[1])/2)+x[1])
  diffuFluxD23Depth <- (((x[3]-x[2])/2)+x[2])
  
  # Calculate soil CO2 diffusive flux at the soil surface (i.e., flux to the atmosphere) by linear extrapolation of the fluxes to the soil surface (µmol m-2 s-1).
  data$diffuFluxD0 <- data$diffuFluxD12 - (((data$diffuFluxD12 - data$diffuFluxD23) / (abs(diffuFluxD12Depth) - abs(diffuFluxD23Depth))) * abs(diffuFluxD12Depth))
  
  # Determine the final quality flag for the diffusive flux at the soil surface
  for(i in 1:nrow(data)){
    if(!is.na(sum(data[i, "finalQFdiffuFluxD12"], data[i, "finalQFdiffuFluxD23"]))){
      # final quality flag = 1 if one or more of the input variables had a raised final quality flag
      if(sum(data[i, "finalQFdiffuFluxD12"], data[i, "finalQFdiffuFluxD23"]) > 0){
        data$finalQFdiffuFluxD0[i] <- 1
      }else{
        data$finalQFdiffuFluxD0[i] <- 0
      }
    }else{ # do this if one or more final quality flags are NA
      data$finalQFdiffuFluxD0[i] <- 1
    }
  }
  # Set the final quality flag for the diffusive flux at the soil surface to 1 if the value is NA
  data$finalQFdiffuFluxD0[which(is.na(data$diffuFluxD0))] <- 1
  # Set the final quality flag for the diffusive flux at the soil surface to 1 if the value is a large negative number. <<<Initially setting threshold to -1 µmol m-2 s-1, but may need to update this in the future>>>
  data$finalQFdiffuFluxD0[which(data$diffuFluxD0 < 0)] <- 1
  
  
  # Plot time series of diffusive flux
  maxDiffuFlux <- max(c(max(data$diffuFluxD12, na.rm = T), 
                    max(data$diffuFluxD23, na.rm = T)), 
                    (max(data$diffuFluxD0, na.rm = T)))
  minDiffuFlux <- min(c(min(data$diffuFluxD12, na.rm = T), 
                    min(data$diffuFluxD23, na.rm = T), 
                    min(data$diffuFluxD0, na.rm = T)))
  filenameGraphFlux <- paste0("flow/flow.soilResp/Flux_", site, "_", locHor, "_", year, "-", mnth, ".png")
  png(filename = filenameGraphFlux, width=800, height=800, res=150)
  plot(data$startDateTime, data$diffuFluxD12, pch=".", ylim=c(minDiffuFlux, (((maxDiffuFlux-minDiffuFlux)*0.4)+maxDiffuFlux)), 
       col=rgb(0, 1, 0.9, 0.1), ylab="Diffusive flux (µmol m-2 s-1)", main=paste0(site, " ", locHor, ": ", year, "-", mnth))
  points(data$startDateTime, data$diffuFluxD23, pch=".", col=rgb(0, 1, 0, 0.1))
  points(data$startDateTime, data$diffuFluxD0, pch=".", col=rgb(0, 0, 1, 0.1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD0 == 0)], data$diffuFluxD0[which(data$finalQFdiffuFluxD0 == 0)], pch=".", col=rgb(0, 0, 1, 1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD12 == 0)], data$diffuFluxD12[which(data$finalQFdiffuFluxD12 == 0)], pch=".", col=rgb(0, 1, 0.9, 1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD23 == 0)], data$diffuFluxD23[which(data$finalQFdiffuFluxD23 == 0)], pch=".", col=rgb(0, 1, 0, 1))
  legDiffuFluxD0 <- paste0("0", " m")
  legDiffuFluxD12 <- paste0(diffuFluxD12Depth, " m")
  legDiffuFluxD23 <- paste0(diffuFluxD23Depth, " m")
  legend("topleft", legend = c(legDiffuFluxD0, legDiffuFluxD12, legDiffuFluxD23), lty=1, col=c("blue", "turquoise", "green"), bty="n")
  dev.off()
  
  
  
  #################################################
  #################################################
  #### END: Calculate soil CO2 fluxes
  #################################################
  #################################################
  
  
  #################################################
  #################################################
  #### Calculate soil CO2 production
  #################################################
  #################################################
  
  # Calculate CO2 production between different depths where fluxes were estimated
  data$prodD0toD12 <- data$diffuFluxD0 - data$diffuFluxD12
  data$prodD12toD23 <- data$diffuFluxD12 - data$diffuFluxD23
  data$prodD23toDeep <- data$diffuFluxD23
  
  # Calculate the mean depth of the CO2 production increments
  aaD0toD12 <- mean(c(0, mean(c(x[1], x[2]))))
  aaD12toD23 <- mean(c(mean(c(x[1], x[2])), mean(c(x[2], x[3])) ))
  aaD23toDeep <- mean(c(mean(c(x[2], x[3]))))
  
  # Determine soil temperature data to use with CO2 production increments
  data$closestTempProdD1 <- data[, paste0("tempInterp_D_", substr(round(aaD0toD12, digits=2), 2, 5))]
  data$closestTempProdD2 <- data[, paste0("tempInterp_D_", substr(round(aaD12toD23, digits=2), 2, 5))]
  data$closestTempProdD3 <- data[, paste0("tempInterp_D_", substr(round(aaD23toDeep, digits=2), 2, 5))]
  
  # Determine soil moisture data to use with CO2 production increments
  data$closestSwcProdD1 <- data[, paste0("swcInterp_D_", substr(round(aaD0toD12, digits=2), 2, 5))]
  data$closestSwcProdD2 <- data[, paste0("swcInterp_D_", substr(round(aaD12toD23, digits=2), 2, 5))]
  data$closestSwcProdD3 <- data[, paste0("swcInterp_D_", substr(round(aaD23toDeep, digits=2), 2, 5))]
  
  ##### 
  ##### END: MAY NEED TO UPDATE CODE BELOW TO ACCOUNT FOR SENSORS THAT ARE REINSTALLED AT A DIFFERENT DEPTH
  ##### 
  
  
  # Create plot of CO2 production. The quality flag data$finalQFdiffuFluxD0 applies to data$prodD0toD12 and data$prodD12toD23, while quality flag data$finalQFdiffuFluxD23 applies to data$prodD23toDeep
  maxDiffuProd <- max(c(max(data$prodD0toD12, na.rm = T), 
                        max(data$prodD12toD23, na.rm = T)), 
                      (max(data$prodD23toDeep, na.rm = T)))
  minDiffuProd <- min(c(min(data$prodD0toD12, na.rm = T), 
                        min(data$prodD12toD23, na.rm = T), 
                        min(data$prodD23toDeep, na.rm = T)))
  filenameGraphProd <- paste0("flow/flow.soilResp/Prod_", site, "_", locHor, "_", year, "-", mnth, ".png")
  png(filename = filenameGraphProd, width=800, height=800, res=150)
  plot(data$startDateTime, data$prodD12toD23, pch=".", ylim=c(minDiffuProd, (((maxDiffuProd-minDiffuProd)*0.4)+maxDiffuProd)), 
       col=rgb(0, 1, 0.9, 0.1), ylab="CO2 production (µmol m-2 s-1)", main=paste0(site, " ", locHor, ": ", year, "-", mnth))
  points(data$startDateTime, data$prodD23toDeep, pch=".", col=rgb(0, 1, 0, 0.1))
  points(data$startDateTime, data$prodD0toD12, pch=".", col=rgb(0, 0, 1, 0.1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD0 == 0)], data$prodD0toD12[which(data$finalQFdiffuFluxD0 == 0)], pch=".", col=rgb(0, 0, 1, 1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD0 == 0)], data$prodD12toD23[which(data$finalQFdiffuFluxD0 == 0)], pch=".", col=rgb(0, 1, 0.9, 1))
  points(data$startDateTime[which(data$finalQFdiffuFluxD23 == 0)], data$prodD23toDeep[which(data$finalQFdiffuFluxD23 == 0)], pch=".", col=rgb(0, 1, 0, 1))
  legDiffuProdD0toD12 <- paste0("0 to ", mean(c(x[1], x[2])), " m")
  legDiffuProdD12toD23 <- paste0(mean(c(x[1], x[2])), " to ", mean(c(x[2], x[3])), " m")
  legDiffuProdD23toDeep <- paste0("<=", mean(c(x[2], x[3])), " m")
  legend("topleft", legend = c(legDiffuProdD0toD12, legDiffuProdD12toD23, legDiffuProdD23toDeep), lty=1, col=c("blue", "turquoise", "green"), bty="n")
  dev.off()
  
  
  #################################################
  #################################################
  #### END: Calculate soil CO2 production
  #################################################
  #################################################
  
  
  # Save data frames
  if(exists("data")){
    filenameData <- paste0("flow/flow.soilResp/data_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(data, file = filenameData, row.names = F)
  }
  if(exists("geoCO2")){
    filenameGeoCO2 <- paste0("flow/flow.soilResp/geoCO2_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(geoCO2, file = filenameGeoCO2, row.names = F)
  }
  if(exists("geoSwc")){
    filenameGeoSwc <- paste0("flow/flow.soilResp/geoSwc_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(geoSwc, file = filenameGeoSwc, row.names = F)
  }
  if(exists("geoTemp")){
    filenameGeoTemp <- paste0("flow/flow.soilResp/geoTemp_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(geoTemp, file = filenameGeoTemp, row.names = F)
  }
  if(exists("mgp")){
    filenameMgp <- paste0("flow/flow.soilResp/mgp_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(mgp, file = filenameMgp, row.names = F)
  }
  if(exists("tempInterp")){
    filenameTempInterp <- paste0("flow/flow.soilResp/tempInterp_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(tempInterp, file = filenameTempInterp, row.names = F)
  }
  if(exists("swcInterp")){
    filenameSwcInterp <- paste0("flow/flow.soilResp/swcInterp_", site, "_", locHor, "_", year, "-", mnth, ".csv")
    write.csv(swcInterp, file = filenameSwcInterp, row.names = F)
  }

}

# # Specify year, month, site and soil plot where soil respiration will be calculated. Development test: site <- 'CPER', year <- 2018, mnth <- 6, locHor <- '001'
# site <- 'SERC'
# year <- 2019 # Year (YYYY)
# mnth <- 4 # Month of year (1-12)
# locHor <- '005' # Soil plot (001, 002, 003, 004, 005)

SITE <- c("ABBY", "BARR", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU", "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", 
          "JORN", "KONA", "KONZ", "LAJA", "LENO", "MLBS", "MOAB", "NIWO", "NOGP", "OAES", "ONAQ", "ORNL", "OSBS", "PUUM", "RMNP", "SCBI", 
          "SERC", "SJER", "SOAP", "SRER", "STEI", "STER", "TALL", "TEAK", "TOOL", "TREE", "UKFS", "UNDE", "WOOD", "WREF", "YELL")
LOCHOR <- c("001", "002", "003", "004", "005")
YEAR <- 2019
MNTH <- c(9)
WNDWWGR <- '030' # Averaging interval (030, 001; minutes)

for(ii in 1:length(SITE)){
  for(jj in 1:length(LOCHOR)){
    for(kk in length(YEAR)){
      for(ll in length(MNTH)){
        tryCatch(expr = {
          soilResp(site = SITE[ii], locHor = LOCHOR[jj], year = YEAR[kk], mnth = MNTH[ll], wndwAgr = WNDWWGR)

        },
        error = function(e){
          message(paste("site=", SITE[ii], "locHor=", LOCHOR[jj], "year=", YEAR[kk], "mnth=", MNTH[ll], "wndwAgr=", WNDWWGR, sep=" "))
          print(e)
        })
      }
    }
  }
}


