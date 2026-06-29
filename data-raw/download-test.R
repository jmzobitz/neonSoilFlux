library(tidyverse)  # Make sure dplyr, purrr, and lubridate are installed
library(neonUtilities)
library(neonSoilFlux)
#

out_env_data <- acquire_neon_data(site_name = 'SJER',
                                  download_date = '2022-06'
)

out_env_data_dc <- acquire_neon_data(site_name = 'SJER',
                                  download_date = '2022-06',
                                  depth_chop = 4
)

out_fluxes_dc <- compute_neon_flux(input_site_env = out_env_data_dc$site_data,
                                input_site_megapit = out_env_data_dc$site_megapit
)

out_env_data_wref <- acquire_neon_data(site_name = 'WREF',
                                  download_date = '2022-06'
)

out_fluxes_wref <- compute_neon_flux(input_site_env = out_env_data$site_data,
                                input_site_megapit = out_env_data$site_megapit
)


VSWC_data <- out_env_data_wref$site_data |>
  filter(measurement == 'VSWC') |>
  unnest(cols=c("data"))

# Plot data
VSWC_data |>
  ggplot(aes(x=startDateTime,y=VSWCMean)) +
  geom_point(aes(color=as.factor(VSWCFinalQF))) +
  facet_grid(verticalPosition~horizontalPosition)

out_env_data_wref$site_data$data[[3]]$verticalPosition |> unique() |>
  sort() |>
  head(4)

