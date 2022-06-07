# Install the package devtools: (one time only)
#  install.packages("devtools")


# Load the devtools package and install the github.
#devtools::install_github("jmzobitz/NEONSoils", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)

library(NEONSoils)
library(tidyverse)

# Now that we have the data ready, then we can acquire the data
# Define the name of the site (SJER), the start and end dates, and where you will save them.  This will download the data and save it to your folder.

# Create the dates vector
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
years <- c("2020-","2021-")
dates <- years %>% str_c(months)

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

dates <- c("2020-02",
           "2020-04",
           "2020-06",
           "2020-07",
           "2020-08",
           "2020-09",
           "2020-10",
           "2020-11",
           "2020-12",
           "2021-01",
           "2021-03",
           "2021-05",
           "2021-07",
           "2021-08",
           "2021-09",
           "2021-10",
           "2021-11",
           "2021-12")

# Now we go through and do the dirty work of saving and computing fluxes. Yay .... :)
# "2021-06, 05" 07 fluxes not computed
for(i in 14:length(dates)) {
  print(dates[i])

  # Name current month (you will need to adjust this on your computer)
  curr_month <- dates[i]
  acquire_name <- "flux-results/curr-flux.Rda"
  env_name <- paste0("flux-results/env-meas-",curr_month,".Rda")
  flux_name <- paste0('flux-results/out-flux-',curr_month,'.Rda')

  # Process
  # NOTE: you will need to say y/n at several points here
  acquire_neon_data(site_name ="SJER",
                     start_date = curr_month,
                     end_date = curr_month,
                     file_name = acquire_name,
                     env_file_name = env_name)

  # Then process and compute the fluxes from that data file.
  out_fluxes <- compute_neon_flux("flux-results/curr-flux.Rda")

  save(out_fluxes,file=flux_name)


}
