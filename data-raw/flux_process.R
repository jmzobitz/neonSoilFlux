### Generate test dataset for easy processing

library(devtools)
library(tidyverse)

sjer_data <- neonSoilFlux::acquire_neon_data("SJER","2022-06")

sjer_env_data_2022_06 <- sjer_data$site_data
use_data(sjer_env_data_2022_06,overwrite = TRUE)

sjer_megapit_data_2022_06 <- sjer_data$site_megapit
use_data(sjer_megapit_data_2022_06,overwrite = TRUE)

sjer_flux_2022_06 <- neonSoilFlux::compute_neon_flux(sjer_env_data_2022_06,sjer_megapit_data_2022_06)
use_data(sjer_flux_2022_06,overwrite = TRUE)
