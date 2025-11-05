### Generate test dataset for easy processing

library(devtools)
library(tidyverse)

sjer_data <- load('inst/extdata/env-meas-SJER-2022-06.Rda')

sjer_env_data_2022_06 <- site_data
use_data(sjer_env_data_2022_06,overwrite = TRUE)

sjer_megapit_data_2022_06 <- site_megapit
use_data(sjer_megapit_data_2022_06,overwrite = TRUE)

sjer_flux <- load('inst/extdata/out-flux-SJER-2022-06.Rda')
sjer_flux_2022_06 <- out_fluxes
use_data(sjer_flux_2022_06,overwrite = TRUE)
