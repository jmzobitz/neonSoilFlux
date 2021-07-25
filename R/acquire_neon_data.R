#' @title Acquire NEON data for processing

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a site code and dates, apply the neonUtilities package to download the data from NEON API
#' @param site_name Required. NEON code for a particular site (a string)
#' @param start_date Required. Date where we end getting NEON data. Format: YYYY-MM (can't specify day).  So "2020-05" means it will grab data for the entire 5th month of 2020. (a string)
#' @param end_date Required. Date where we end getting NEON data. Format: YYYY-MM (can't specify day).  So "2020-08" means it will grab data for the entire 8th month of 2020. (a string)
#' @param file_name Required. Path of location for the save file. Must end in .Rda (a string)
#'
#' @example acquire_neon_data("SJER","2020-05","2020-08","my-file.Rda")
#'
#' @import neonUtilities

#' @return Nothing is returned - the file is saved to the location provided

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-22)
#     original creation
#     update to fix auto download (2021-07-25)

acquire_neon_data <- function(site_name,start_date,end_date,file_name) {

  site_megapit <- neonUtilities::loadByProduct(dpID="DP1.00096.001",
                                               site=site_name,
                                               package="expanded",
                                               check.size = F)


  site_temp <- neonUtilities::loadByProduct(dpID="DP1.00041.001",
                                            site=site_name,
                                            startdate=start_date,
                                            enddate=end_date,
                                            package="expanded",
                                            check.size = F)


  site_swc <- neonUtilities::loadByProduct(dpID="DP1.00094.001",
                                           site=site_name,
                                           startdate=start_date,
                                           enddate=end_date,
                                           package="expanded",
                                           check.size = F)

  site_press <- neonUtilities::loadByProduct(dpID="DP1.00004.001",
                                             site=site_name,
                                             startdate=start_date,
                                             enddate=end_date,
                                             package="expanded",
                                             check.size = F)

  site_co2 <- neonUtilities::loadByProduct(dpID="DP1.00095.001",
                                           site=site_name,
                                           startdate=start_date,
                                           enddate=end_date,
                                           package="expanded",
                                           check.size = F)

  # Save the files
  save(site_co2,site_press,site_swc,site_temp,site_megapit,
       file=file_name)



}
