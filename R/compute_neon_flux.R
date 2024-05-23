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
#'  # First acquire the NEON data at a given NEON site
#' out_env_data <- acquire_neon_data("SJER","2020-05")
#'
#' # Then process and compute the fluxes:
#'  out_flux <- compute_neon_flux(input_site_env = sjer_env_data_2022_06,
#'  input_site_megapit = sjer_megapit_data_2022_06)
#' }
#' @return Data frame of fluxes and gradient from the timeperiod


#' @export
#'
# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation
#     2023-07-16: Update to take advantage of nested structures to improve / decrease computational time, allowing for multiple measurements to compute the flux.
#     2023-10-23: Update to exit computing a flux if there are no half-hourly measurements.
#     2024-04-08: update to get namespaces correct






compute_neon_flux <- function(input_site_env,
                              input_site_megapit) {
  .data = NULL  # Appease R CMD Check


  ################
  # 1) Load up the data (this may take a while)  Will be two data frames:
  # site_megapit: a nested file containing specific information about the site (for bulk density calculations, etc)
  # site_data: a nested data file containing measurements for the required flux gradient model during the given time period


  ### Then take each of the measurements to associate them with errors
  corrected_data <- correct_env_data(input_site_env)

  qf_flags <- corrected_data$all_flags
  all_measures <- corrected_data$site_filtered

  # Yay!  We solved the joining problem!

  ################


  ################
  # 3) Addsin the megapit data so we have bulk density, porosity measurements at the interpolated depth.

  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <-  input_site_megapit$mgp_permegapit
  mgp.hzon <- input_site_megapit$mgp_perhorizon
  mgp.bgeo <- input_site_megapit$mgp_perbiogeosample
  mgp.bden <- input_site_megapit$mgp_perbulksample


  # Merge the soil properties into a single data frame

  mgp.hzon.bgeo <- dplyr::inner_join(mgp.hzon, mgp.bgeo, by = c("horizonID", "pitID", "domainID", "siteID", "horizonName", "pitNamedLocation"))
  mgp.hzon.bgeo.bden <- dplyr::inner_join(mgp.hzon.bgeo, mgp.bden, by = c("horizonID", "pitID", "domainID", "siteID", "horizonName", "labProjID", "laboratoryName", "pitNamedLocation"))
  mgp <- dplyr::inner_join(mgp.hzon.bgeo.bden, mgp.pit, by = c("pitID", "domainID", "siteID", "pitNamedLocation", "nrcsDescriptionID"))

  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130,
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 +
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################

  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density.
  rockVol <- ((mgp$coarseFrag2To5 + mgp$coarseFrag5To20) / 1000) / 2.65

  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3.
  porosSub2mm <- 1 - mgp$bulkDensExclCoarseFrag / 2.65

  # Calculate porosity of the 0-20 mm fraction (cm3 cm-3). Assume no pores within rocks.
  mgp$porVol2To20 <- porosSub2mm * (1 - rockVol)

  ### Now go through the environmental data and add the correct porVol2To20 at each of the zOffsets -- a double map :-)

  all_measures2 <- all_measures |>
    dplyr::mutate(env_data = purrr::map(.x = .data[["env_data"]], .f = function(x) {
      porVol2To20 <- purrr::map_dbl(.x = x$zOffset, .f = function(x) {
        horizon <- dplyr::intersect(which(abs(x) >= mgp$horizonTopDepth / 100), which(abs(x) <= mgp$horizonBottomDepth / 100))

        if (length(horizon) > 1) {
          horizon <- horizon[which.min(mgp$horizonTopDepth[horizon])]
        }
        return(mgp$porVol2To20[horizon])
      })
      return(dplyr::mutate(x, porVol2To20))
    }))

  ################


  ################
  # 4) It's flux computation time - first we do the diffusivity at different depths and then the conversion of co2 to umol, followed by the fluxes



  flux_out <- all_measures2 |> # first filter out any bad measurements
    dplyr::mutate(flux_intro = purrr::map2(.x = .data[["env_data"]], .y = .data[["press_data"]], .f = function(.x, .y) {
      c <- co2_to_umol(
        .x$soilTempMean,
        .y$staPresMean,
        .x$soilCO2concentrationMean,
        .x$soilTempExpUncert,
        .y$staPresExpUncert,
        .x$soilCO2concentrationExpUncert,
        .x$zOffset
      )



      d <- diffusivity(
        temperature = .x$soilTempMean,
        soil_water = .x$VSWCMean,
        pressure = .y$staPresMean,
        temperature_err = .x$soilTempExpUncert,
        soil_water_err = .x$VSWCExpUncert,
        pressure_err = .y$staPresExpUncert,
        zOffset = .x$zOffset,
        porVol2To20 = .x$porVol2To20
      )


      new_data <- dplyr::inner_join(c, d, by = "zOffset")


      return(new_data)
    })) |>
    dplyr::mutate(
      flux_compute = purrr::map(.data[["flux_intro"]], compute_surface_flux),
      diffusivity = purrr::map(.x = .data[["flux_intro"]], .f = ~ (.x |>
        dplyr::slice_max(order_by = zOffset) |>
        dplyr::select(zOffset, diffusivity, diffusExpUncert)
      ))
    ) |>
    dplyr::select(tidyselect::all_of(c("horizontalPosition","startDateTime","flux_compute", "diffusivity")))



  ################  Fluxes computed!  Now join back to the original data frame and we are ready to rock and roll!

  na_fluxes <- tibble::tibble(
    flux = NA,
    flux_err = NA,
    method = c(
      "dejong_shappert_1972",
      "hirano_2005",
      "tang_2003",
      "tang_2005"
    )
  )

  na_diffusivity <- tibble::tibble(
    zOffset = NA,
    diffusivity = NA,
    diffusExpUncert = NA
  )

  out_fluxes <- qf_flags |>
    dplyr::left_join(flux_out,by=c("startDateTime","horizontalPosition")) |>
    dplyr::relocate(.data[["startDateTime"]],.data[["horizontalPosition"]],.data[["flux_compute"]],.data[["diffusivity"]])

  # Kicking it out school again with a loop - easiest to fill in where we aren't able to compute
  for(i in 1:nrow(out_fluxes)) {
    if(is.null(out_fluxes$diffusivity[[i]])) {
      out_fluxes$diffusivity[[i]] <- na_diffusivity
    }

    if(is.null(out_fluxes$flux_compute[[i]])) {
      out_fluxes$flux_compute[[i]] <- na_fluxes
    }
  }


  return(out_fluxes)

}
