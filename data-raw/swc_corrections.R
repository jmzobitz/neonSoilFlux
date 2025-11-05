## code to prepare `swc_corrections` dataset goes here

# Function that loads up the current soil water correction depths.
library(devtools)
library(tidyverse)

# Created 2022-06-10
# Updated 2024-04-10
# Updated 2024-11-20 - include the SoilMostureDepths corrections

#' (From corr w/ Ed): The first issue is the sensor depths being incorrectly reported in the sensor_positions file. Our current data processing system does not allow a fix to this issue, so in the meantime we’ve added a file containing the installation depths and an associated readme file (both attached) to the data product documentation. This is mentioned in the Description section of the webpage you linked and you can find the files in the Documentation section. If you adapt your code to pull the depths from this file it should solve this issue.
#'




swc_corrections <- read_csv('data-raw/swc_depthsV2.csv') |>
  unite(col=HOR.VER,horizontalPosition.HOR,verticalPosition.VER,sep=".")


#use_data(swc_corrections,overwrite = TRUE,internal = TRUE)

# Read in NEON calibrations
neonCal <- read.csv("data-raw/SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)

#use_data(neonCal,overwrite = TRUE,internal = TRUE)

# Read in NEON bounded60 calibrations
neonBoulded60Cal <- read.csv("data-raw/Bounded60_SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)

use_data(swc_corrections,neonCal,neonBoulded60Cal,overwrite = TRUE,internal = TRUE)
