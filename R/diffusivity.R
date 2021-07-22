#' @title Compute soil diffusivity

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}

#' @description
#' Given a tidied data frame of soil measurements (from interpolate.R), compute the diffusivity in a given soil layer


#' @param temperature Required. Soil temperature (degrees C)
#' @param soil_water Required. Soil water content
#' @param pressure Required. Barometric air pressure ?
#' @param coarseFrag2To5 Required. Coarse fragmentation of rocks
#' @param coarseFrag5To20 Required. More coarse fragmentation of rocks
#' @param bulkDensExclCoarseFrag Required. Bulk density of horizon excluding the coarse fragmented
#'
#' @return A value of the computed diffusivity

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords Currently none

#' @examples TBD
#'

#' @seealso

#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-21)
#     original creation

diffusivity <- function(temperature,soil_water,pressure,coarseFrag2To5,coarseFrag5To20,bulkDensExclCoarseFrag) {

  # Assign values to constants
  R <- 0.008314472 # Ideal gas constant = 0.008314472 m3 kPa °K-1 mol-1
  absZero <- -273.15 # Absolute zero (-273.15 °C; 0 °K)


  #####
  # Calculate porosity of each soil horizon
  #####

  ###############################
  # Future development: Estimate particle density of <2 mm fraction based on Ruhlmann et al. 2006 Geoderma 130,
  # 272-283. Assumes C content of organic matter is 55%. Constants 1.127, 0.373, 2.684 come
  # from Ruhlman et al. 2006 (2.684 = particle density of the mineral fraction, "(1.127 +
  # 0.373*(dfBGChem$Estimated.organic.C..../55))" = particle density of organic matter).
  ###############################

  # Calculate 2-20 mm rock volume (cm3 cm-3). Assume 2.65 g cm-3 density.
  rockVol <- ((coarseFrag2To5 + coarseFrag5To20) / 1000) / 2.65

  # Calculate porosity of the <2 mm fraction (cm3 cm-3). Assume soil particle density of 2.65 g cm-3.
  porosSub2mm <- 1 - bulkDensExclCoarseFrag/2.65

  # Calculate porosity of the 0-20 mm fraction (cm3 cm-3). Assume no pores within rocks.
  porVol2To20 <- porosSub2mm * (1 - rockVol)



    # Calculate the air filled porosity
    porVol2To20AirFilled <- porVol2To20 - soil_water

    # Calculate gas tortuosity factor based on Millington and Quirk 1961
    tort <- (porVol2To20AirFilled^(10/3)) / (porVol2To20^2)

    # Calculate CO2 diffusivity in free air (m2 s-1).
    diffuFreeAir <- 0.0000147 * ((temperature - absZero) / (20 - absZero))^1.75 * (pressure / 101.3)

    # Calculate CO2 diffusivity (m2 s-1).
    diffusivity <- tort * diffuFreeAir

    return(diffusivity)




}
