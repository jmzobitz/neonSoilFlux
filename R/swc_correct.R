#' @title Convert co2 concentration from ppm to µmol m-3 units

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given the expanded SWC data, return a corrected version based on the values below


#' @param input_swc Required. input soil water content data from acquire_neon_data (as a list)
#' @param curr_site Current site we are working with

#' @return A revised list

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples
#' (From corr w/ Ed): The first issue is the sensor depths being incorrectly reported in the sensor_positions file. Our current data processing system does not allow a fix to this issue, so in the meantime we’ve added a file containing the installation depths and an associated readme file (both attached) to the data product documentation. This is mentioned in the Description section of the webpage you linked and you can find the files in the Documentation section. If you adapt your code to pull the depths from this file it should solve this issue.
#'
#' The second issue is that most of the soil moisture data were calculated using the manufacturers default calibration, rather than a soil-specific calibration, which causes the final quality flag to be raised. The fix to this issue is more complex depending on the option that you prefer. This issue is mentioned in the middle of the Abstract section of the webpage, which also includes a potential solution:
#'
#' The final quality flag for soil water content data is raised (i.e., VSWCFinalQF = 1) if it was calculated using the sensor manufacturer’s default calibration rather than a soil-specific calibration. However, for many use cases the default calibration is sufficient. To identify data that were solely flagged because they were generated using the default calibration, download the “Expanded” data package and identify rows where VSWCFinalQF = 1, VSWCAlphaQM < 10, VSWCBetaQM < 20, (VSWCFinalQFSciRvw = 0 or NA), and tempFailQM = 0.

#' @import tidyr


#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2022-06-10)
#     original creation

swc_correct <- function(input_swc,curr_site) {

   ### Determine which of the sites we are working with:
  swc_corrections_site <- swc_corrections %>%
    filter(siteID == curr_site) %>%
    select(-siteID)   # Remove this so it doesn't get weird

    # Update the depths of the soil water sensors
    input_swc$sensor_positions_00094 <- input_swc$sensor_positions_00094 %>%
      left_join(swc_corrections_site,by="HOR.VER") %>%
      select(-zOffset) %>%
      rename(zOffset = sensorDepth) %>%
      relocate(zOffset,.after="yOffset")

  # Next update the swc flags for both 30 and 1 minutes:


    input_swc$SWS_30_minute <- input_swc$SWS_30_minute %>%
      rowwise() %>%
      mutate(VSWCFinalQF = if_else(
        VSWCFinalQF == 1 &
          VSWCAlphaQM < 10 &
          VSWCBetaQM < 20 &
          tempFailQM == 0,0,VSWCFinalQF
      ) )

    input_swc$SWS_1_minute <- input_swc$SWS_1_minute %>%
      rowwise() %>%
      mutate(newQF = if_else(
        VSWCFinalQF == 1 &
          VSWCAlphaQM < 10 &
          VSWCBetaQM < 20 &
          tempFailQM == 0,0,VSWCFinalQF
      ) )

    return(input_swc)

}

