##############################################################################################
#' @title Download and interpolate soil temperature and moisture data

#' @author 
#' Edward Ayres \email{eayres@battelleecology.org}

#' @description 
#' Definition function. Produce interpolated soil temperature or soil moisture profiles for a particular year and month


#' @param site Required. Character string. The NEON site code (e.g. 'CPER').
#' @param locHor Required. Character string. The 3-digit horizontal location index of the data product (e.g. '000' for the tower)
#' @param wndwAgr Required. Character string. The 3-digit timing (aggregation) index of the data product (e.g. '030' for 3-minute average data) 
#' @param year Required. Numeric value. The year in which to search for data (e.g. 2017).
#' @param mnth Required. Numeric value. The month in which to search for data (e.g. 1).
#' @param variable Required. Character string. The type of data to use. Options are 'soilTemp' (soil temperature) and 'soilMoist' (soil moisture).
#' @param saveInputs Required. Character string. Option to save input data. Options are 'False' (default) and 'True'.

#' @return A data frame of the requested data. If more than one file is returned from the api, a list will be returned with one 
#' data frame per file. 

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples 


#' @seealso 

#' @export

# changelog and author contributions / copyrights
#   Ed Ayres (2019-07-09)
#     original creation
##############################################################################################


interp <- function(site, year, mnth, locHor, wndwAgr, variable, saveInputs){

  # Load required packages
  library(dplyr)
  library(httr)
  library(jsonlite)
  
  # Static inputs for downloading API data
  idDpMainTemp <- 'DP1.00041.001'
  idDpMainSwc <- 'DP1.00094.001'
  locVerTemp <- c('501', "502", "503", "504", "505", "506", "507", "508", "509")
  locVerSwc <- c('501', "502", "503", "504", "505", "506", "507", "508")
  Pack <- 'expanded'
  
  # Function to merge multiple data frames into one
  MyMerge <- function(x, y){
    df <- merge(x, y, by= c("startDateTime", "endDateTime"), all.x= TRUE, all.y= TRUE)
    return(df)
  }
  
  # Start interpolation function
  if(variable == "soilTemp"){
    # Read in soil temperature data
    dataTempD1 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[1],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD2 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[2],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD3 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[3],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD4 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[4],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD5 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[5],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD6 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[6],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD7 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[7],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD8 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[8],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataTempD9 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainTemp,locHor=locHor,locVer=locVerTemp[9],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    
    # Append column names with data type
    colnames(dataTempD1)[!grepl(pattern = "DateTime", colnames(dataTempD1))] <- 
      paste0(colnames(dataTempD1)[!grepl(pattern = "DateTime", colnames(dataTempD1))], "TempD1")
    colnames(dataTempD2)[!grepl(pattern = "DateTime", colnames(dataTempD2))] <- 
      paste0(colnames(dataTempD2)[!grepl(pattern = "DateTime", colnames(dataTempD2))], "TempD2")
    colnames(dataTempD3)[!grepl(pattern = "DateTime", colnames(dataTempD3))] <- 
      paste0(colnames(dataTempD3)[!grepl(pattern = "DateTime", colnames(dataTempD3))], "TempD3")
    colnames(dataTempD4)[!grepl(pattern = "DateTime", colnames(dataTempD4))] <- 
      paste0(colnames(dataTempD4)[!grepl(pattern = "DateTime", colnames(dataTempD4))], "TempD4")
    colnames(dataTempD5)[!grepl(pattern = "DateTime", colnames(dataTempD5))] <- 
      paste0(colnames(dataTempD5)[!grepl(pattern = "DateTime", colnames(dataTempD5))], "TempD5")
    colnames(dataTempD6)[!grepl(pattern = "DateTime", colnames(dataTempD6))] <- 
      paste0(colnames(dataTempD6)[!grepl(pattern = "DateTime", colnames(dataTempD1))], "TempD6")
    colnames(dataTempD7)[!grepl(pattern = "DateTime", colnames(dataTempD7))] <- 
      paste0(colnames(dataTempD7)[!grepl(pattern = "DateTime", colnames(dataTempD1))], "TempD7")
    colnames(dataTempD8)[!grepl(pattern = "DateTime", colnames(dataTempD8))] <- 
      paste0(colnames(dataTempD8)[!grepl(pattern = "DateTime", colnames(dataTempD1))], "TempD8")
    colnames(dataTempD9)[!grepl(pattern = "DateTime", colnames(dataTempD9))] <- 
      paste0(colnames(dataTempD9)[!grepl(pattern = "DateTime", colnames(dataTempD1))], "TempD9")
    
    # Merge data frames into a single data frame
    data <- Reduce(MyMerge, list(dataTempD1, dataTempD2, dataTempD3, dataTempD4, dataTempD5, dataTempD6, dataTempD7, dataTempD8, dataTempD9))
    
    # remove data frames that are no longer needed
    rm("dataTempD1", "dataTempD2", "dataTempD3", "dataTempD4", "dataTempD5", "dataTempD6", "dataTempD7", "dataTempD8", "dataTempD9")
    
    # Add site and soil plot number to data frame
    data$site <- site
    data$soilPlot <- locHor
    
    # Read in soil temperature sensor depth data. Use " %>% distinct()" to remove any duplicated rows.
    tmp.files <- som::def.neon.api.get.info(Type='data',idDpMain=idDpMainTemp,site=site,year=year,mnth=mnth)
    geoTemp <- read.delim(tmp.files$data$files$url[grep("sensor_positions", tmp.files$data$files$name)][1], sep=",") %>% distinct()
    

    #################################################
    #### Interpolate soil temperature data across depths
    #################################################
    
    # Create data frame for soil temperature data after interpolating across depths with columns from 0 m to 2 m
    tempInterp <- as.data.frame(data$startDateTime)
    colnames(tempInterp) <- "startDateTime"
    tempInterp$endDateTime <- data$endDateTime
    cols <- paste0("D_", seq.int(from=0, to=2.00, by=0.01))
    tempInterp[, cols] <- NA
    
    # Create a loop to interpolate soil temperature for each datetime
    for(i in 1:nrow(data)){
      # Identify good (i.e., unflagged) soil temperature data
      goodTemp <- substr(colnames(data)[grep("finalQFTemp", colnames(data))][which(data[i, grep("finalQFTemp", colnames(data))] == 0)], 13, 13)
      goodTemps <- paste0("soilTempMeanTempD", goodTemp)
      
      # Identify depths of sensors with good temperature data
      loc <- paste0(substr(locHor, 3, 3), ".50", goodTemp)
      goodTempsDepths <- geoTemp$zOffset[grep(paste(loc, collapse = "|"), geoTemp$HOR.VER)]
      
      # set top depth for interpolation
      if(max(goodTempsDepths) > -0.05){
        from <- 0 # Extrapolate to soil surface is sensor is less than 5 cm from soil surface
      }else{
        from <- round(max(goodTempsDepths), digits=2)
      }
      
      # set bottom depth for interpolation
      if(min(goodTempsDepths) < -0.2){
        to <- floor(min(goodTempsDepths)*10) / 10 # Extrapolate to nearest 10 cm increment below deepest sensor if that sensor is below 20 cm (i.e., not experiencing very high diurnal variability)
      }else{
        to <- round(min(goodTempsDepths), digits=2)
      }
      
      # Create sequence of depths in 1 cm intervals up to the depth of the deepest sensor producing good data
      depths <- seq.int(from=from, to=to, by=-0.01)
      
      # Only do Spline fit if there at least 4 soil temperature sensors producing good data
      if(length(goodTemp) > 3){
        
        # Fit a spline to the good temperature data
        tempSpline <- smooth.spline(y=data[i, goodTemps], x=goodTempsDepths)
        
        # Predict soil temperature from soil surface to depth of deepest sensor producing good data in 1 cm increments
        tempInterp[i, paste0("D_", abs(depths))] <- predict(tempSpline, x=depths)$y
      }else if(length(goodTemp) > 1){ # Do linear interpolation if there are only 2 or 3 good temperature sensors
        tempInterp[i, paste0("D_", abs(depths))] <- approx(x=goodTempsDepths, y=data[i, goodTemps], xout = depths, rule=2)$y
      }
    }
    
    # Interpolate missing soil temperature values over time if values are present at the time point immediately before and after
    for(i in 2:(nrow(tempInterp)-1)){
      for(j in 1:length(cols)){
        if(is.na(tempInterp[i , cols[j]])){
          tempInterp[i , cols[j]] <- mean(c(tempInterp[i-1 , cols[j]], tempInterp[i+1 , cols[j]]))
        }
      }
    }
    
    
    #################################################
    #### END: Interpolate soil temperature data across depths
    #################################################
    
    # save dataframes
    if(exists("data")){
      if(saveInputs == T){
        filenameTempInputs <- paste0("flow/flow.soilResp/tempInputs_", site, "_", locHor, "_", year, "-", mnth, ".csv")
        write.csv(data, file = filenameTempInputs, row.names = F)
        
      }
    }
    
    return(tempInterp)
    
    
  }else if(variable == "soilMoist"){
    
    # Read in soil water content data
    dataSwcD1 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[1],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD2 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[2],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD3 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[3],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD4 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[4],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD5 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[5],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD6 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[6],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD7 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[7],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    dataSwcD8 <- som::def.neon.api.get.data(site=site,idDpMain=idDpMainSwc,locHor=locHor,locVer=locVerSwc[8],wndwAgr=wndwAgr,year=year,mnth=mnth,Pack=Pack)
    
    # Append column names with data type
    colnames(dataSwcD1)[!grepl(pattern = "DateTime", colnames(dataSwcD1))] <- 
      paste0(colnames(dataSwcD1)[!grepl(pattern = "DateTime", colnames(dataSwcD1))], "SwcD1")
    colnames(dataSwcD2)[!grepl(pattern = "DateTime", colnames(dataSwcD2))] <- 
      paste0(colnames(dataSwcD2)[!grepl(pattern = "DateTime", colnames(dataSwcD2))], "SwcD2")
    colnames(dataSwcD3)[!grepl(pattern = "DateTime", colnames(dataSwcD3))] <- 
      paste0(colnames(dataSwcD3)[!grepl(pattern = "DateTime", colnames(dataSwcD3))], "SwcD3")
    colnames(dataSwcD4)[!grepl(pattern = "DateTime", colnames(dataSwcD4))] <- 
      paste0(colnames(dataSwcD4)[!grepl(pattern = "DateTime", colnames(dataSwcD4))], "SwcD4")
    colnames(dataSwcD5)[!grepl(pattern = "DateTime", colnames(dataSwcD5))] <- 
      paste0(colnames(dataSwcD5)[!grepl(pattern = "DateTime", colnames(dataSwcD5))], "SwcD5")
    colnames(dataSwcD6)[!grepl(pattern = "DateTime", colnames(dataSwcD6))] <- 
      paste0(colnames(dataSwcD6)[!grepl(pattern = "DateTime", colnames(dataSwcD6))], "SwcD6")
    colnames(dataSwcD7)[!grepl(pattern = "DateTime", colnames(dataSwcD7))] <- 
      paste0(colnames(dataSwcD7)[!grepl(pattern = "DateTime", colnames(dataSwcD7))], "SwcD7")
    colnames(dataSwcD8)[!grepl(pattern = "DateTime", colnames(dataSwcD8))] <- 
      paste0(colnames(dataSwcD8)[!grepl(pattern = "DateTime", colnames(dataSwcD8))], "SwcD8")
    
    # Merge data frames into a single data frame
    data <- Reduce(MyMerge, list(dataSwcD1, dataSwcD2, dataSwcD3, dataSwcD4, dataSwcD5, dataSwcD6, dataSwcD7, dataSwcD8))
    
    # remove data frames that are no longer needed
    rm("dataSwcD1", "dataSwcD2", "dataSwcD3", "dataSwcD4", "dataSwcD5", "dataSwcD6", "dataSwcD7", "dataSwcD8")
    
    # Add site and soil plot number to data frame
    data$site <- site
    data$soilPlot <- locHor
    
    # Read in soil moisture sensor depth data. Use " %>% distinct()" to remove any duplicated rows.
    tmp.files <- som::def.neon.api.get.info(Type='data',idDpMain=idDpMainSwc,site=site,year=year,mnth=mnth)
    geoSwc <- read.delim(tmp.files$data$files$url[grep("sensor_positions", tmp.files$data$files$name)][1], sep=",") %>% distinct()
    

    
    #################################################
    #### Interpolate soil water content data across depths
    #################################################
    
    # Create data frame for soil water content data after interpolating across depths with columns from 0 m to 2 m
    swcInterp <- as.data.frame(data$startDateTime)
    colnames(swcInterp) <- "startDateTime"
    swcInterp$endDateTime <- data$endDateTime
    cols <- paste0("D_", seq.int(from=0, to=2.00, by=0.01))
    swcInterp[, cols] <- NA
    
    # Create a loop to interpolate soil water content for each datetime
    for(i in 1:nrow(data)){
      # Identify good (i.e., unflagged) soil water content data
      goodSWC <- substr(colnames(data)[grep("VSWCFinalQFSwc", colnames(data))][which(data[i, grep("VSWCFinalQFSwc", colnames(data))] == 0)], 16, 16)
      # Only continue if there at least 2 soil water content sensors producing good data
      if(length(goodSWC) > 1){
        goodSWCs <- paste0("VSWCMeanSwcD", goodSWC)
        
        # Identify depths of sensors with good water content data
        loc <- paste0(substr(locHor, 3, 3), ".50", goodSWC)
        goodSWCsDepths <- geoSwc$zOffset[grep(paste(loc, collapse = "|"), geoSwc$HOR.VER)]
        
        # Fit a spline to the good water content data. Switched to linear interpolation (approx()) as spline often retunrned negative soil moistures.
        #swcSpline <- smooth.spline(y=data[i, goodSWCs], x=goodSWCsDepths)
        
        # set top depth for interpolation
        if(max(goodSWCsDepths) > -0.05){
          from <- 0 # Extrapolate to soil surface is sensor is less than 5 cm from soil surface
        }else{
          from <- round(max(goodSWCsDepths), digits=2)
        }
        
        # set bottom depth for interpolation
        if(min(goodSWCsDepths) < -0.2){
          to <- floor(min(goodSWCsDepths)*10) / 10 # Extrapolate to nearest 10 cm increment below deepest sensor if that sensor is below 20 cm (i.e., not experiencing very high diurnal variability)
        }else{
          to <- round(min(goodSWCsDepths), digits=2)
        }
        
        # Create sequence of depths in 1 cm intervals up to the depth of the deepest sensor producing good data
        depths <- seq.int(from=from, to=to, by=-0.01)
        
        # Predict soil water content from soil surface to depth of deepest sensor producing good data in 1 cm increments
        #swcInterp[i, paste0("D_", abs(depths))] <- predict(swcSpline, x=depths)$y
        swcInterp[i, paste0("D_", abs(depths))] <- approx(x=goodSWCsDepths, y=data[i, goodSWCs], xout = depths, rule=2)$y
      }
    }
    
    # Interpolate missing soil moisture values over time if values are present at the time point immediately before and after
    for(i in 2:(nrow(swcInterp)-1)){
      for(j in 1:length(cols)){
        if(is.na(swcInterp[i , cols[j]])){
          swcInterp[i , cols[j]] <- mean(c(swcInterp[i-1 , cols[j]], swcInterp[i+1 , cols[j]]))
        }
      }
    }
    
    
    #################################################
    #### END: Interpolate soil water content data across depths
    #################################################
    
    # save dataframes
    if(exists("data")){
      if(saveInputs == T){
        filenameSwcInputs <- paste0("flow/flow.soilResp/swcInputs_", site, "_", locHor, "_", year, "-", mnth, ".csv")
        write.csv(data, file = filenameSwcInputs, row.names = F)
        
      }
    }
    
    return(swcInterp)
    
  }
}

