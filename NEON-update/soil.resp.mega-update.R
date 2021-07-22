##############################################################################################
#' @title Download and combine megapit phsyical and chemical properties data data

#' @author
#' Edward Ayres \email{eayres@battelleecology.org}

#' @description
#' Definition function. Download and combine megapit phsyical and chemical properties data data

#' @param site Required. Name of megapit data frame list

#' @return A data frame of the requested data.

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


getMegapit <- function(site){
  # Get data availability of megapit soil physical properties
  mgp.dp <- GET("http://data.neonscience.org/api/v0/products/DP1.00096.001")
  mgp.avail <- fromJSON(content(mgp.dp, as="text"), simplifyDataFrame=T, flatten=T)

  # Get the list of available data URLs and find the one corresponding to the site
  mgp.tmp.urls <- unlist(mgp.avail$data$siteCodes$availableDataUrls)
  mgp.tmp.urls <- mgp.tmp.urls[grep(site, mgp.tmp.urls)]
  mgp.tmp <- GET(mgp.tmp.urls)
  mgp.tmp.files <- fromJSON(content(mgp.tmp, as="text"))

  # Ingest the megapit soil physical properties pit, horizon, and biogeo data
  mgp.pit <- sjer_megapit$mgp_permegapit

  mgp.hzon <- sjer_megapit$mgp_perhorizon

  mgp.bgeo <- sjer_megapit$mgp_perbiogeosample

  mgp.bden <- sjer_megapit$mgp_perbulksample


  # Ingest megapit soil Chemical properties biogeo data
  mgc.bgeo <- sjer_megapit$mgp_perbiogeosample


  # Remove audit sample rows, if any
  mgp.bgeo <- mgp.bgeo[!grepl("Audit", mgp.bgeo$biogeoSampleType),]
  mgp.bden <- mgp.bden[!grepl("Audit", mgp.bden$bulkDensSampleType),]
  mgc.bgeo <- mgc.bgeo[!grepl("Audit", mgc.bgeo$biogeoSampleType),]

  # Merge the soil properties into a single data frame
  mgp.bgeo.bgeo <- merge(mgp.bgeo, mgc.bgeo, by=c("horizonID", "pitID", "domainID", "siteID",
                                                  "pitNamedLocation", "horizonName", "biogeoTopDepth", "biogeoBottomDepth",
                                                  "biogeoCenterDepth", "remarks", "laboratoryName", "labProjID", "biogeoID",
                                                  "biogeoHorizonProportion", "biogeoSampleType", "setDate", "collectDate"))
  mgp.hzon.bgeo <- merge(mgp.hzon, mgp.bgeo.bgeo, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName"))
  mgp.hzon.bgeo.bden <- merge(mgp.hzon.bgeo, mgp.bden, by=c("horizonID", "pitID", "domainID", "siteID", "horizonName", "labProjID", "laboratoryName"))
  mgp <- merge(mgp.hzon.bgeo.bden, mgp.pit, by=c("pitID", "domainID", "siteID", "pitNamedLocation", "nrcsDescriptionID"))

  # Clean up data frames no longer needed
  rm("mgp.pit", "mgp.hzon", "mgp.bgeo", "mgp.bden", "mgc.bgeo", "mgp.bgeo.bgeo", "mgp.hzon.bgeo", "mgp.hzon.bgeo.bden")

  return(mgp)
}
