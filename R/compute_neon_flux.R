#' @title Compute NEON fluxes at a site

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}
#' based on code developed by Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Given a site filename (from acquire_neon_data), process and compute fluxes.
#' This file takes a saved data file from acquire:
#' 1) Takes the needed components (QF and measurement flags) for soil water, temperature, co2, binding them together in a tidy data frame
#' 2) Interpolates across the measurements
#' 3) Merges air pressure data into this data frame
#' 4) Does a final QF check so we should have only timeperiods where all measurements exist
#' 5) Adds in the megapit data so we have bulk density, porosity measurements at the interpolated depth.
#' 6) Saves the data

#' @param input_file_name Required. Path of location for the save file from acquire_neon_data. Must end in .Rda (a string)
#' @param out_flux_file_name Required. Path of location where you will save file for fluxes
#' @param time_frequency Required. Will you be using 30 minute ("30_minute") or 1 minute ("1_minute") recorded data? Defaults to 30 minutes.
#' @param input_column_selectors Optional. List of measurements from which fluxes will be computed typically c("Mean","Minimum","Maximum","ExpUncert","StdErMean") (the more used increases computational time)
#'
#' @example acquire_neon_data("SJER","my-out-file.Rda")
#'
#' # Then process and compute the fluxes from that data file.
#'  compute_neon_flux(""my-file.Rda","flux-file.Rda)
#'
#' @import dplyr

#' @return Data frame of fluxes and gradient from the timeperiod

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation
#     2023-07-16: Update to take advantage of nested structures to improve / decrease computational time, allowing for multiple measurements to compute the flux.
#     2023-10-23: Update to exit computing a flux if there are no half-hourly measurements.





compute_neon_flux <- function(input_file_name,
                              out_flux_file_name,
                              time_frequency = "30_minute",
                              input_column_selectors = c("Mean","ExpUncert")) {

  ################
  # 1) Load up the data (this may take a while)  Will be two data frames:
  # site_megapit: a nested file containing specific information about the site (for bulk density calculations, etc)
  # site_data: a nested data file containing measurements for the required flux gradient model during the given time period

  load(input_file_name)

  ################

  ################
  # 2) Interpolates across the measurements
  site_data2 <- site_data |>
    mutate(data = pmap(.l=list(data,monthly_mean,measurement),.f=~insert_mean(..1,..2,..3)))

  # Filters out measurements that don't have enough QF flags
  site_filtered <- measurement_detect(site_data2)

  # Exit gracefully if no values get returned
  if(any(site_filtered$n_obs ==0)) {
    msg = paste0("No valid environmental timeperiod measurements for ", input_file_name)
    stop(msg)
  }



  # Interpolate all the measurements together in one nested function
  # We *need* to interpolate the errors - assume the errors interpolate as well?

  site_interp <- depth_interpolate(
    input_measurements = site_filtered,
    measurement_name = c("VSWC","soilTemp"),  # Measurements we are interpolating
    measurement_interpolate = "soilCO2concentration")  # We always want to interpolate to this depth

  # Add in the pressure measurements
  pressure_measurement <- site_filtered |>
    filter(measurement =="staPres") |>
    select(-monthly_mean) |>
    unnest(cols=c("data")) |>
    group_by(startDateTime) |>
    nest() |>
    rename(press_data = data)


  ### Then take each of the measurements to associate them with errors
  all_measures <- site_interp |>
    inner_join(pressure_measurement, by=c("startDateTime")) |>
    mutate(staPresMeanQF = map_int(.x=press_data,.f=~pull(.x,staPresFinalQF)))


  # Yay!  We solved the joining problem!

  ################


  ################
  # 3) Addsin the megapit data so we have bulk density, porosity measurements at the interpolated depth.

  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <- site_megapit$mgp_permegapit
  mgp.hzon <- site_megapit$mgp_perhorizon
  mgp.bgeo <- site_megapit$mgp_perbiogeosample
  mgp.bden <- site_megapit$mgp_perbulksample


  # Merge the soil properties into a single data frame

  mgp.hzon.bgeo <- inner_join(mgp.hzon, mgp.bgeo, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName","pitNamedLocation"))
  mgp.hzon.bgeo.bden <- inner_join(mgp.hzon.bgeo, mgp.bden, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName", "labProjID", "laboratoryName","pitNamedLocation"))
  mgp <- inner_join(mgp.hzon.bgeo.bden, mgp.pit, by=c("pitID", "domainID", "siteID", "pitNamedLocation", "nrcsDescriptionID"))

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

  ### Now go through the environmental data and add the correct porVol2To20 at each of the zOffsets -- a double map :-)

  all_measures2 <- all_measures |>
    mutate(env_data = map(.x=env_data,.f=function(x) {

      porVol2To20 <- map_dbl(.x=x$zOffset,.f=function(x) {
        horizon <- intersect(which(abs(x) >= mgp$horizonTopDepth/100), which(abs(x) <= mgp$horizonBottomDepth/100))

        if(length(horizon)>1){
          horizon <- horizon[which.min(mgp$horizonTopDepth[horizon])]
        }
        return( mgp$porVol2To20[horizon])

      })
      return(mutate(x,porVol2To20))

    }))

  ################


  ################
  # 4) It's flux computation time - first we do the diffusivity at different depths and then the conversion of co2 to umol, followed by the fluxes




  flux_out <- all_measures2 |>  # first filter out any bad measurements
    mutate(flux_compute = map2(.x=env_data,.y=press_data, .f=function(.x,.y) {c <- co2_to_umol(
      .x$soilTempMean,
      .y$staPresMean,
      .x$soilCO2concentrationMean,
      .x$soilTempExpUncert,
      .y$staPresExpUncert,
      .x$soilCO2concentrationExpUncert,
      .x$zOffset
    );



    d <- diffusivity(temperature = .x$soilTempMean,
                     soil_water = .x$VSWCMean,
                     pressure = .y$staPresMean,
                     temperature_err = .x$soilTempExpUncert,
                     soil_water_err = .x$VSWCExpUncert,
                     pressure_err = .y$staPresExpUncert,
                     zOffset = .x$zOffset,
                     porVol2To20 = .x$porVol2To20);


    new_data <- inner_join(c,d,by="zOffset");


    return(compute_surface_flux(new_data)) }) ) |>
    select(horizontalPosition,startDateTime,flux_compute)



  ################

  # 5) Compute the fluxes and join to a continuous timeperiod based on the input frequency, saving the final result
  seq_time_freq <- if_else(time_frequency == "30_minute","30 min","1 min")
  out_dates <- tibble(startDateTime = seq(min(flux_out$startDateTime),max(flux_out$startDateTime),by=seq_time_freq))

  # Join the time vector to the data and the QF flags and save
  measurement_flags <- all_measures |>
    select(-env_data,-press_data)

  out_fluxes <- out_dates |>
    inner_join(flux_out,by="startDateTime") |>
    inner_join(measurement_flags,by=c("horizontalPosition","startDateTime"))

  save(out_fluxes,file=out_flux_file_name)




}
