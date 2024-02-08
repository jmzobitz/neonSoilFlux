# Install the package devtools: (one time only)
#  install.packages("devtools")


# Load the devtools package and install the github.
#devtools::install_github("jmzobitz/NEONSoils", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)

library(NEONSoils)
library(tidyverse)



# --> YOU ADD: Define the name of the site:
curr_site_name <- "SJER"


# --> Create the dates vector You can shorten these to a single month if you want.
dates <- c("2020-01",
           "2020-02",
           "2020-03",
           "2020-04",
           "2020-05",
           "2020-06",
           "2020-07",
           "2020-08",
           "2020-09",
           "2020-10",
           "2020-11",
           "2020-12",
           "2021-01",
           "2021-02",
           "2021-03",
           "2021-04",
           "2021-05",
           "2021-06",
           "2021-07",
           "2021-08",
           "2021-09",
           "2021-10",
           "2021-11",
           "2021-12")



# Now we go through and do the dirty work of saving and computing fluxes. Yay .... :)
for(i in seq_along(dates)) {
  print(dates[i])

  # Name current month (you will need to adjust this on your computer)
  curr_month <- dates[i]
  acquire_name <- "curr-flux.Rda"
  env_name <- paste0("env-meas-",curr_month,".Rda")
  flux_name <- paste0('out-flux-',curr_month,'.Rda')

  # Process
try(
  # NOTE: you will need to say y/n at several points here
 { acquire_neon_data(site_name =curr_site_name,
                    start_date = curr_month,
                    end_date = curr_month,
                    data_file_name = acquire_name,
                    env_file_name = env_name)

  # Then process and compute the fluxes from that data file.
  out_fluxes <- compute_neon_flux("curr-flux.Rda")

  save(out_fluxes,file=flux_name)
 }

)
}
