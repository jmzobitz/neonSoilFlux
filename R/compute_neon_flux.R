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

#' @param input_site_env Required. Input list of environmental data.  Usually given from acquire_neon_data
#' @param input_site_megapit Required. Input list of environmental soil data.  Usually given from acquire_neon_data
#'
#' @examples
#' \donttest{
#'  out_flux <- compute_neon_flux(input_site_env = sjer_env_data_2022_06,
#'  input_site_megapit = sjer_megapit_data_2022_06)
#'  }

#' @return Data frame of fluxes and gradient from the timeperiod


#' @export
#'



compute_neon_flux <- function(input_site_env,
                              input_site_megapit) {
  .data = NULL  # Appease R CMD Check


  # changelog and author contributions / copyrights
  #   John Zobitz (2021-07-22)
  #     original creation
  #     2023-07-16: Update to take advantage of nested structures to improve / decrease computational time, allowing for multiple measurements to compute the flux.
  #     2023-10-23: Update to exit computing a flux if there are no half-hourly measurements.
  #     2024-04-08: update to get namespaces correct
  #     2024-11-20: update to get incorporate changes to compute_surface_flux_layer (include gradient & diffusivity, code cleanup)





  ################
  # 1) Load up the data (this may take a while)  Will be two data frames:
  # site_megapit: a nested file containing specific information about the site (for bulk density calculations, etc)
  # site_data: a nested data file containing measurements for the required flux gradient model during the given time period


  # Adjust the ExpUncert to 1 SD from 2 -- note from Ed on 10/17
  input_site_env <- input_site_env |>
    mutate(data=map(data,.f=~mutate(.x,across(.cols=ends_with("ExpUncert"),.fns=~.x/2))))


  ################
  # 1) Addsin the megapit data so we have bulk density, porosity measurements at the interpolated depth.

  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <-  input_site_megapit$mgp_permegapit

  ### Information about the horizons
  mgp.hzon <- input_site_megapit$mgp_perhorizon |>
    dplyr::select(horizonID,horizonTopDepth,horizonBottomDepth) |>
    dplyr::arrange(horizonTopDepth)

  ###
  mgp.bgeo <- input_site_megapit$mgp_perbiogeosample
  mgp.bden <- input_site_megapit$mgp_perbulksample


  # Merge the soil properties into a single data frame.  We average by horizon

  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130,
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 +
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################

  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density for each horizon.
  #rockVol <- ((mgp$coarseFrag2To5 + mgp$coarseFrag5To20) / 1000) / 2.65
  rockVol <- mgp.bgeo |>
    dplyr::mutate(rockVol = (coarseFrag2To5 + coarseFrag5To20) / 1000 / 2.65) |>
    dplyr::group_by(horizonID) |>
    dplyr::summarize(rockVol = mean(rockVol,na.rm=TRUE)) |>
    dplyr::ungroup()

  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3. (done across each horizon)
  porosSub2mm <- mgp.bden |>
    dplyr::mutate(porosSub2mm = 1 - bulkDensExclCoarseFrag / 2.65) |>
    dplyr::group_by(horizonID) |>
    dplyr::summarize(porosSub2mm = mean(porosSub2mm,na.rm=TRUE)) |>
    dplyr::ungroup()

  # Join these all up together in a megapit data frame, convert depths to m

  mgp <- mgp.hzon |>
    dplyr::inner_join(rockVol,by="horizonID") |>
    dplyr::inner_join(porosSub2mm,by="horizonID") |>
    dplyr::mutate(porVol2To20 = porosSub2mm * (1 - rockVol),  # Define the porosity
           horizonTopDepth = horizonTopDepth/100,
           horizonBottomDepth = horizonBottomDepth/100)  # convert to m



  # Now interpolate everything to the depth of the co2 measurements

  # Tells us the depths at each site, adds in the porosity
  site_depths <- input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]] |>
    dplyr::group_by(horizontalPosition,verticalPosition) |>
    dplyr::distinct(zOffset) |>
    dplyr::mutate(porVol2To20 = purrr::map_dbl(.x=.data[["zOffset"]],
                                               .f=~mgp[abs(.x) > mgp$horizonTopDepth & abs(.x) <= mgp$horizonBottomDepth,"porVol2To20"])
    ) |>
    dplyr::select(horizontalPosition,verticalPosition,porVol2To20)

  ### Now we can join things together
  input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]]  <- input_site_env[input_site_env$measurement == "soilCO2concentration",]$data[[1]] |>
    dplyr::inner_join(site_depths,by=c("horizontalPosition","verticalPosition"))


  # Now correct
  corrected_data <- correct_env_data(input_site_env)

  qf_flags <- corrected_data$all_flags
  all_measures <- corrected_data$site_filtered



  ################


  ################
  # 4) It's flux computation time - first we do the diffusivity at different depths and then the conversion of co2 to umol, followed by the fluxes



  flux_out <- all_measures |> # first filter out any bad measurements
    dplyr::mutate(flux_intro = purrr::map2(.x = .data[["env_data"]], .y = .data[["press_data"]], .f = function(.x, .y) {
      c <- co2_to_umol(
        .x$soilTempMean,
        .y$staPresMean,
        .x$soilCO2concentrationMean,
        .x$soilTempStdErMean,
        .y$staPresStdErMean,
        .x$soilCO2concentrationStdErMean,
        .x$zOffset
      )


      d <- diffusivity(
        temperature = .x$soilTempMean,
        soil_water = .x$VSWCMean,
        pressure = .y$staPresMean,
        temperature_err = .x$soilTempStdErMean,
        soil_water_err = .x$VSWCStdErMean,
        pressure_err = .y$staPresStdErMean,
        zOffset = .x$zOffset,
        porVol2To20 = .x$porVol2To20
      )


      new_data <- dplyr::inner_join(c, d, by = "zOffset")


      return(new_data)
    })) |>
    dplyr::mutate(
      flux_compute = purrr::map(.x=.data[["flux_intro"]],
                                .f= ~(.x |>
                                        dplyr::group_by(diffus_method) |>
                                        tidyr::nest() |>
                                        dplyr::mutate(out = purrr::map(data,compute_surface_flux_layer)) |>
                                        dplyr::select(diffus_method,out) |>
                                        tidyr::unnest(cols=c("out")) |>
                                        dplyr::ungroup()) ),
      surface_diffusivity = purrr::map(.x = .data[["flux_intro"]], .f = ~ (.x |>
        dplyr::slice_max(order_by = zOffset) |>
        dplyr::select(zOffset, diffusivity, diffusStdErMean,diffus_method)
      ))
    ) |>
    dplyr::select(tidyselect::all_of(c("horizontalPosition","startDateTime","flux_compute", "surface_diffusivity")))



  ################  Fluxes computed!  Now join back to the original data frame and we are ready to rock and roll!


  # Create an empty vector of NA fluxes
  na_fluxes <- flux_out$flux_compute[[1]]|>
    dplyr::mutate(dplyr::across(-tidyselect::all_of(c("diffus_method","method")), ~ NA))



  na_diffusivity <-   flux_out$surface_diffusivity[[1]]|>
    dplyr::mutate(dplyr::across(-tidyselect::all_of(c("zOffset","diffus_method")), ~ NA))

  out_fluxes <- qf_flags |>
    dplyr::left_join(flux_out,by=c("startDateTime","horizontalPosition")) |>
    dplyr::relocate(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["flux_compute"]],.data[["surface_diffusivity"]])

  # Kicking it out school again with a loop - easiest to fill in where we aren't able to compute
  for(i in 1:nrow(out_fluxes)) {
    if(is.null(out_fluxes$surface_diffusivity[[i]])) {
      out_fluxes$surface_diffusivity[[i]] <- na_diffusivity
    }

    if(is.null(out_fluxes$flux_compute[[i]])) {
      out_fluxes$flux_compute[[i]] <- na_fluxes
    }
  }



  return(out_fluxes)

}
