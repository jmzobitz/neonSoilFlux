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

  # Filters out measurements that don't have enough QF flags
  site_filtered <- measurement_detect(site_data)

  # Exit gracefully if no values get returned
  if(any(input_data_interp_ready$n_obs ==0)) {
    msg = paste0("No valid environmental timeperiod measurements for ", input_file_name)
    stop(msg)
  }



    # Interpolate all the measurements together in one nested function
    # We *need* to interpolate the errors - assume the errors interpolate as well?

    site_interp <- depth_interpolate(
      input_measurements = site_filtered,
      measurement_name = c("VSWC","soilTemp"),  # Measurements we are interpolating
      measurement_interpolate = "soilCO2concentration",  # We always want to interpolate to this depth
      column_selectors = input_column_selectors)


    ### Then take each of the measurements to associate them with errors
    site_interp2 <- site_interp |>
      mutate(data = map2(.x=data,.y=measurement,.f=~(

        if(.y !="staPres") {
          .x |> select(horizontalPosition,startDateTime,siteID,verticalPosition,matches(str_c(.y,input_column_selectors)))
        } else {
          .x |> select(startDateTime,siteID,matches(str_c(.y,input_column_selectors)))
        }

                                                     )))

    # Now we want to find a common dataset where the different times can be merged together
    # Because the pressure measurements are at a single point (not at a depth, we need to consider them separately from the temperature, co2, and SWC, which are all at depth)


    # Pivot measured data
    all_measures_pre <- site_interp2 |>
      mutate(data = map2(.x = data, .y= measurement,.f = ~ (.x |>
                                                 pivot_longer(cols = matches(str_c(.y,input_column_selectors))) |>
                                                 mutate(type = str_extract(name,pattern = paste0(input_column_selectors,collapse = "|")))
                                                 ) ) )

    # Separate out temperature, swc, and co2 concentration.
    # Group them at each value
    env_measures <- all_measures_pre |>
      filter(measurement != "staPres") |>
      select(measurement,data) |>
      unnest(cols=c("data")) |>
      select(measurement,horizontalPosition,verticalPosition,startDateTime,type,value) |>
      pivot_wider(names_from = "type") |>
      pivot_longer(cols=c("Mean","ExpUncert")) |>
      unite(col="measurement",measurement,name,sep="") |>
      pivot_wider(names_from="measurement") |>
      group_by(horizontalPosition,verticalPosition,startDateTime) |>
      nest() |>
      rename(env_data = data)

    # Create a grouped data frame from the pressure
    press_measures <- all_measures_pre |>
      filter(measurement == "staPres") |>
      select(measurement,data) |>
      unnest(cols=c("data")) |>
      select(measurement,startDateTime,type,value) |>
      unite(col="measurement",measurement,type,sep="") |>
      pivot_wider(names_from="measurement") |>
      group_by(startDateTime) |>
      nest() |>
      rename(press_data = data)

    # Now join them up. We only do the mean and the expanded measurement uncertainty
    all_measures <- env_measures |>
      inner_join(press_measures,by=c("startDateTime")) |>
      mutate(new_data = map2(.x=env_data,.y=press_data,.f=~cbind(.x,.y))) |>
      select(-env_data,-press_data) |>
      unnest(cols=c("new_data")) |>
      ungroup() |>
      group_by(horizontalPosition,verticalPosition,startDateTime) |>
      nest()

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

    mgp.hzon.bgeo <- inner_join(mgp.hzon, mgp.bgeo, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName"))
    mgp.hzon.bgeo.bden <- inner_join(mgp.hzon.bgeo, mgp.bden, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName", "labProjID", "laboratoryName"))
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

    # The position data should all be the same (since we harmonized it earlier)
    vertical_positions <- site_interp$positions[[1]]
    mgp_info <- vector(mode = "list", length = nrow(vertical_positions))

    # Now we should go across the nested depths and have the horizon
    # Kicking it old school with the double loop (the indices are small, so that is ok.)

    for(i in seq_along(vertical_positions$zOffset)) {

      horizon <- intersect(which(abs(vertical_positions$zOffset[i]) >= mgp$horizonTopDepth/100), which(abs(vertical_positions$zOffset[i]) <= mgp$horizonBottomDepth/100))

      if(length(horizon)>1){
        horizon <- horizon[which.min(mgp$horizonTopDepth[horizon])]
      }

      mgp_info[[i]] <- mgp$porVol2To20[horizon]


    }

    # make the list a tibble and bind to the vertical positions data frame

    mgp_list <- tibble(porVol2To20 = mgp_info) |>
      unnest(cols=c("porVol2To20"))

    mgp_measures <- cbind(vertical_positions,mgp_list) |>
      group_by(HOR,VER) |> nest() |>
      rename(mgp_data = data)

    # now join to all the data together
    all_measures2 <- all_measures |>
      inner_join(mgp_measures,by=c("horizontalPosition"="HOR","verticalPosition" = "VER")) |>
      unnest(cols = c("mgp_data")) |>
      unnest(cols=c("data")) |>
      ungroup() |>
      drop_na() |>
      group_by(horizontalPosition,startDateTime) |>
      nest()
    ################


    ################
    # 4) It's flux computation time - first we do the diffusivity at different depths and then the conversion of co2 to umol, followed by the fluxes


    flux_out <- all_measures2 |>
      mutate(flux_compute = map(.x=data, .f=function(.x) {c <- co2_to_umol(
        .x$soilTempMean,
        .x$staPresMean,
        .x$soilCO2concentrationMean,
        .x$soilTempExpUncert,
        .x$staPresExpUncert,
        .x$soilCO2concentrationExpUncert, .x$zOffset
      );



      d <- diffusivity(temperature = .x$soilTempMean,
                       soil_water = .x$VSWCMean,
                       pressure = .x$staPresMean,
                       temperature_err = .x$soilTempExpUncert,
                       soil_water_err = .x$VSWCExpUncert,
                       pressure_err = .x$staPresExpUncert,
                       zOffset = .x$zOffset,
                       porVol2To20 = .x$porVol2To20);

      new_data <- inner_join(c,d,by="zOffset");

      return(compute_surface_flux(new_data)) }) ) |>
      select(horizontalPosition,startDateTime,flux_compute) |>
      unnest(cols=c("flux_compute")) |>
      rename(fluxExpUncert = ExpUncert)



    ################

    # 5) Compute the fluxes and join to a continuous timeperiod based on the input frequency, saving the final result
    seq_time_freq <- if_else(time_frequency == "30_minute","30 min","1 min")
    out_dates <- tibble(startDateTime = seq(min(flux_out$startDateTime),max(flux_out$startDateTime),by=seq_time_freq))

    # Join the time vector to the data and save
    out_fluxes <- out_dates |>
      inner_join(flux_out,by="startDateTime")

    save(out_fluxes,file=out_flux_file_name)




}
