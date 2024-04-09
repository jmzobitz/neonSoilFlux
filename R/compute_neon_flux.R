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

#' @param input_file_name Required. Path of location for the save file from acquire_neon_data. Must end in .Rda (a string), otherwise exits gracefully
#' @param out_flux_file_name Required. Path of location where you will save file for fluxes.  Must end in .Rda or .csv - otherwise exits gracefully.
#' @param time_frequency Required. Will you be using 30 minute ("30_minute") or 1 minute ("1_minute") recorded data? Defaults to 30 minutes.
#' @param input_column_selectors Optional. List of measurements from which fluxes will be computed typically c("Mean","Minimum","Maximum","ExpUncert","StdErMean") (the more used increases computational time)
#'
#' @examples acquire_neon_data("SJER","my-out-file.Rda")
#'
#' # Then process and compute the fluxes from that data file.
#'  compute_neon_flux("my-out-file.Rda","flux-file.Rda")
#'

#' @return Data frame of fluxes and gradient from the timeperiod

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation
#     2023-07-16: Update to take advantage of nested structures to improve / decrease computational time, allowing for multiple measurements to compute the flux.
#     2023-10-23: Update to exit computing a flux if there are no half-hourly measurements.
#     2024-04-08: update to get namespaces correct

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr





compute_neon_flux <- function(input_file_name,
                              out_flux_file_name,
                              time_frequency = "30_minute",
                              input_column_selectors = c("Mean", "ExpUncert")) {
  # Get the save file extension and do a quick check
  extension_name <- stringr::str_extract(out_flux_file_name, pattern = "(?<=\\.).{3}$")
  if (!(extension_name %in% c("csv", "Rda", "rda"))) {
    stop("Save file name extension must be a Rdata file (Rda) or comma separated file (csv). Please revise.")
  }

  input_extension_name <- stringr::str_extract(input_file_name, pattern = "(?<=\\.).{3}$")
  if (!(input_extension_name %in% c("Rda", "rda"))) {
    stop("Input file name extension must be a Rdata file (Rda). Please revise.")
  }

  ################
  # 1) Load up the data (this may take a while)  Will be two data frames:
  # site_megapit: a nested file containing specific information about the site (for bulk density calculations, etc)
  # site_data: a nested data file containing measurements for the required flux gradient model during the given time period

  load(input_file_name)


  ### Then take each of the measurements to associate them with errors
  all_measures <- extract_env_data(input_file_name)


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
    dplyr::mutate(env_data = purrr::map(.x = env_data, .f = function(x) {
      porVol2To20 <- purrr::map_dbl(.x = x$zOffset, .f = function(x) {
        horizon <- dplyr::intersect(which(abs(x) >= mgp$horizonTopDepth / 100), which(abs(x) <= mgp$horizonBottomDepth / 100))

        if (length(horizon) > 1) {
          horizon <- horizon[which.min(mgp$horizonTopDepth[horizon])]
        }
        return(mgp$porVol2To20[horizon])
      })
      return(mutate(x, porVol2To20))
    }))

  ################


  ################
  # 4) It's flux computation time - first we do the diffusivity at different depths and then the conversion of co2 to umol, followed by the fluxes



  flux_out <- all_measures2 |> # first filter out any bad measurements
    dplyr::mutate(flux_intro = purrr::map2(.x = env_data, .y = press_data, .f = function(.x, .y) {
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
      flux_compute = purrr::map(flux_intro, compute_surface_flux),
      diffusivity = purrr::map(.x = flux_intro, .f = ~ (.x |>
        dplyr::slice_max(order_by = zOffset) |>
        dplyr::select(zOffset, diffusivity, diffusExpUncert)
      ))
    ) |>
    dplyr::select(horizontalPosition, startDateTime, flux_compute, diffusivity)



  ################

  # 5) Compute the fluxes and join to a continuous timeperiod based on the input frequency, saving the final result
  seq_time_freq <- dplyr::if_else(time_frequency == "30_minute", "30 min", "1 min")
  out_dates <- tidyr::expand_grid(startDateTime = seq(min(flux_out$startDateTime), max(flux_out$startDateTime), by = seq_time_freq), horizontalPosition = unique(all_measures$horizontalPosition))

  # Join the time vector to the data and the QF flags and save
  measurement_flags <- all_measures |>
    dplyr::select(-env_data, -press_data)

  null_fluxes <- tibble::tibble(
    flux = NA,
    flux_err = NA,
    method = c(
      "dejong_shappert_1972",
      "hirano_2005",
      "tang_2003",
      "tang_2005"
    )
  )

  null_diffusivity <- tibble::tibble(
    zOffset = NA,
    diffusivity = NA,
    diffusExpUncert = NA
  )

  out_fluxes <- out_dates |>
    dplyr::left_join(flux_out, by = c("startDateTime", "horizontalPosition")) |>
    dplyr::left_join(measurement_flags, by = c("horizontalPosition", "startDateTime")) |>
    dplyr::mutate(
      flux_compute = purrr::map(.x = flux_compute, .f = ~ (if (is.null(.x)) {
        null_fluxes
      } else {
        .x
      })),
      diffusivity = purrr::map(.x = diffusivity, .f = ~ (if (is.null(.x)) {
        null_diffusivity
      } else {
        .x
      })),
      VSWCMeanQF = purrr::map_dbl(.x = VSWCMeanQF, .f = ~ (if (is.na(.x)) {
        2
      } else {
        .x
      })),
      soilTempMeanQF = purrr::map_dbl(.x = soilTempMeanQF, .f = ~ (if (is.na(.x)) {
        2
      } else {
        .x
      })),
      soilCO2concentrationMeanQF = purrr::map_dbl(.x = soilCO2concentrationMeanQF, .f = ~ (if (is.na(.x)) {
        2
      } else {
        .x
      })),
      staPresMeanQF = purrr::map_dbl(.x = staPresMeanQF, .f = ~ (if (is.na(.x)) {
        2
      } else {
        .x
      }))
    )



  # Now start saving
  if (extension_name == "csv") {
    out_flux_full <- out_fluxes |>
      tidyr::unnest(cols = tidyselect::everything())

    readr::write_csv(out_flux_full, file = out_flux_file_name)
  } else {
    save(out_fluxes, file = out_flux_file_name)
  }
}
