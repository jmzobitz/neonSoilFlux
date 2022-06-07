#' @title Compute NEON fluxes at a site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Given a site filename (from acquire_neon_data), process and compute fluxes

#' @param file_name Required. Path of location for the save file from acquire_neon_data. Must end in .Rda (a string)
#'
#' @example acquire_neon_data("SJER","2020-05","2020-08","my-file.Rda")
#'
#' @import neonUtilities

#' @return Data frame of fluxes from the timeperiod

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation

# This file takes a saved data file from acquire.R:
# 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame
# 2) Interpolates across the measurements
# 3) Merges air pressure data into this data frame
# 4) Does a final QF check so we should have only timeperiods where all measurements exist
# 5) Adds in the megapit data so we have bulk density, porosity measurements at the interpolated depth.
# 6) Saves the data

# Todo:
# Make this a general function for the R package

# Changelog
# 07-21-21 Initial file creation by JZ

################
# 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame

compute_neon_flux <- function(input_file_name) {

  # Load up the data (this may take a while)
  load(input_file_name)

  soil_water <- measurement_merge(site_swc,"SWS","00094","VSWCMean","VSWCFinalQF") %>%
    mutate(measurement = "soil_water") %>%
    rename(value = VSWCMean,
           finalQF = VSWCFinalQF)

  temperature <- measurement_merge(site_temp,"ST","00041","soilTempMean","finalQF") %>%
    mutate(measurement = "temperature") %>%
    rename(value = soilTempMean)

  co2 <- measurement_merge(site_co2,"SCO2C","00095","soilCO2concentrationMean","finalQF") %>%
    mutate(measurement = "co2") %>%
    rename(value = soilCO2concentrationMean)

  # Bind these all up together
  site <- rbind(soil_water,temperature,co2)

  # Get the sensor positions for CO2:
  site_co2_positions <- sensor_positions(site_co2,"SCO2C","00095")

  ################


  ################
  # 2) Interpolates across the measurements
  # Interpolate all the measurements together in one nested function
  site_interp <- depth_interpolate(site)

  ################
  # 3) Merges air pressure data into this data frame
  # Next we need to join the pressure data - this is just one measurement by time
  pressure <- site_press$BP_30min %>%
    select(startDateTime,staPresMean,staPresFinalQF) %>%
    mutate(measurement = "pressure") %>%
    rename(value = staPresMean,
           finalQF = staPresFinalQF) %>%
    filter(finalQF == 0) %>%
    select(-finalQF) %>%
    group_by(startDateTime) %>%
    nest()

  # Then we will want to join the times where the pressure is - for ease of use it will be at all the depths

  site_press_nest <- site_interp %>%
    group_by(startDateTime,zOffset,horizontalPosition) %>%
    nest() %>%
    inner_join(pressure,by=c("startDateTime")) %>%
    mutate(data = map2(.x=data.x,.y=data.y,~(rbind(.x,.y))))

  # Remove the data
  site_interp_press <- site_press_nest %>%
    select(-data.x,-data.y)
  ################

  ################
  # 4) Does a final QF check so we should have only timeperiods where all measurements exist
  # Unnest and nest by date only pull out timeperiods where we have
  site_date_nest <- site_interp_press %>%
    unnest(cols=c(data)) %>%
    group_by(horizontalPosition,startDateTime) %>%
    nest()

  # Make sure we have the final QF and all 4 measurements at that time point
  site_finalQF_interp <- site_date_nest %>%
    mutate(finalQF = map(.x=data,.f=~(sum(is.na(.x$value)))),
           finalMeasurement = map(.x=data,.f=~(n_distinct(.x$measurement)) ),
           finalQF = as.numeric(finalQF),
           finalMeasurement = as.numeric(finalMeasurement)) %>%
    filter(finalQF == 0, finalMeasurement == 4) %>%
    unnest(cols=c(data)) %>%
    ungroup() %>%
    select(-finalQF,-finalMeasurement)

  ################

  ################
  # 5) Adds in the megapit data so we have bulk density, porosity measurements at the interpolated depth.
  # Now we group by depth to pull in the megapit data:
  site_depth_nest <- site_finalQF_interp %>%
    group_by(zOffset) %>%
    nest()


  # Pull in the megapit data

  # Merge the soil properties into a single data frame
  biogeo_sample <- site_megapit$mgp_perbiogeosample %>%
    inner_join(site_megapit$mgp_perbulksample , by=c("horizonID", "pitID", "domainID", "siteID", "pitNamedLocation", "horizonName",  "laboratoryName", "labProjID", "setDate", "collectDate")) %>%
    select(c("horizonID", "pitID",
             "coarseFrag2To5","coarseFrag5To20","biogeoTopDepth", "bulkDensExclCoarseFrag","biogeoBottomDepth",
             "biogeoCenterDepth")) %>%
    mutate(across(.cols=matches("biogeo"),~-.x/100)) %>%
    drop_na()

  # Now we should go across the nested depths and have the horizon
  # Kicking it old school with the double loop
  for(i in seq_along(site_depth_nest$zOffset)) {
    for(j in 1:dim(biogeo_sample)[1]) {
      if(between(site_depth_nest$zOffset[i],biogeo_sample$biogeoBottomDepth[j],biogeo_sample$biogeoTopDepth[j])) {
        site_depth_nest$data[[i]] <- site_depth_nest$data[[i]] %>% mutate(biogeo_sample[j,])
      }
    }
  }

  ################

  ################
  # 6) Saves the data
  site_final_interp <- site_depth_nest %>%
    unnest(cols=c(data))

  #7) Compute the fluxes
  out_fluxes <- neon_site_flux(site_final_interp,site_co2_positions)

  return(out_fluxes)

  ################



}
