# Function that loads up the current soil water correction depths.
library(devtools)

# Created 2022-06-10

#' (From corr w/ Ed): The first issue is the sensor depths being incorrectly reported in the sensor_positions file. Our current data processing system does not allow a fix to this issue, so in the meantime we’ve added a file containing the installation depths and an associated readme file (both attached) to the data product documentation. This is mentioned in the Description section of the webpage you linked and you can find the files in the Documentation section. If you adapt your code to pull the depths from this file it should solve this issue.
#'
#' The second

swc_corrections <- read_csv('swc-corrections/swc_depths.csv') %>%
  unite(col=HOR.VER,horizontalPosition.HOR,verticalPosition.VER,sep=".") %>%
  select(siteID,HOR.VER,sensorDepth)


use_data(swc_corrections,overwrite = TRUE)
