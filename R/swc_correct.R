#' @title Internal function to correct depths for VSWC NEON data.

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given the expanded SWC data, return a corrected version based on the values below


#' @param input_swc Required. input soil water content data from acquire_neon_data (as a list)
#' @param curr_site Current site we are working with
#' @param reference_time Current month we are working with

#' @return A revised list of corrected soil water content and depths.
#'
#' @examples
#' \donttest{
#' # Download the soil water content data:
#' site_swc <- neonUtilities::loadByProduct(
#' dpID="DP1.00094.001",
#' site="SJER",
#' startdate="2020-05",
#' enddate="2020-05",
#' timeIndex = "30",
#' package="expanded",
#' check.size = FALSE,
#' include.provisional = TRUE
#' )
#'
#' # Then correct the swc:
#' site_swc <- swc_correct(site_swc,"SJER","2020-05")
#' }


swc_correct <- function(input_swc,curr_site,reference_time) {


  ### NOTES
  # # (From correspondence w/ Edward Ayres \email{eayres@battelleecology.org}): The first issue is the sensor depths being incorrectly reported in the sensor_positions file. Our current data processing system does not allow a fix to this issue, so in the meantime we’ve added a file containing the installation depths and an associated readme file (both attached) to the data product documentation. This is mentioned in the Description section of the webpage you linked and you can find the files in the Documentation section. If you adapt your code to pull the depths from this file it should solve this issue.

  # # The second issue is that most of the soil moisture data were calculated using the manufacturers default calibration, rather than a soil-specific calibration, which causes the final quality flag to be raised. The fix to this issue is more complex depending on the option that you prefer. This issue is mentioned in the middle of the Abstract section of the webpage, which also includes a potential solution:
  #
  # # The final quality flag for soil water content data is raised (i.e., VSWCFinalQF = 1) if it was calculated using the sensor manufacturer’s default calibration rather than a soil-specific calibration. However, for many use cases the default calibration is sufficient. To identify data that were solely flagged because they were generated using the default calibration, download the “Expanded” data package and identify rows where VSWCFinalQF = 1, VSWCAlphaQM < 10, VSWCBetaQM < 20, (VSWCFinalQFSciRvw = 0 or NA), and tempFailQM = 0.
  #
  # # (Added 07-17-2023) Prior to RELEASE-2023, the final quality flag for soil water content data was raised (i.e., VSWCFinalQF = 1) when soil water content was calculated using the sensor manufacturer’s default calibration (calDefaultQM > 0) rather than a soil-specific calibration (calNEONQM > 0) . However, for many use cases the default calibration is sufficient. To identify data that were solely flagged because they were generated using the default calibration, download the “Expanded” data download package and identify rows where VSWCFinalQF = 1, VSWCAlphaQM < 10, VSWCBetaQM < 20, (VSWCFinalQFSciRvw = 0 or NA), and tempFailQM = 0. As of RELEASE-2023 (issued in January 2023), the final quality flag is no longer raised when the manufacturer's default calibration is used. The quality metrics indicating which calibration was used (calDefaultQM and calNEONQM) continue to be published in the expanded download package as informational metrics. (from the data documentation page for this product) - in this case we commented out the correction.



  # changelog and author contributions / copyrights
  #   John Zobitz (2022-06-10)
  #     original creation
  #     2024-04-08: update to get namespaces correct
  #     2024-04-10: update to account for more correct soil depths



  ###


  .data = NULL  # Appease R CMD Check
   ### Determine which of the sites we are working with:
  swc_corrections_site <- swc_corrections |>
    dplyr::filter(.data[[ "siteID" ]] == curr_site) |>
    dplyr::mutate(interval = purrr::map2_lgl(.x=.data[["startDateTime"]],
                                             .y=.data[["endDateTime"]],
                                             .f=~inside_interval(.x,.y,reference_time)
    )
    ) |>
    dplyr::filter(.data[["interval"]])

    # Update the depths of the soil water sensors
    input_swc$sensor_positions_00094 <- input_swc$sensor_positions_00094 |>
      dplyr::group_by(.data[["siteID"]],.data[["HOR.VER"]]) |>
      tidyr::nest() |>
      dplyr::left_join(swc_corrections_site,by=c("siteID","HOR.VER")) |>
      dplyr::mutate(data = purrr::map(.data[["data"]],dplyr::slice_head), # Get one variable
                    data = purrr::map2(.x=.data[["data"]],
                                      .y=.data[["sensorDepth"]],
                                      .f=~(.x |> dplyr::mutate(zOffset = .y) )
                    )
      ) |>
      dplyr::select(.data[["siteID"]],.data[["HOR.VER"]],.data[["data"]]) |>
      tidyr::unnest(cols=c("data"))


    return(input_swc)

}

