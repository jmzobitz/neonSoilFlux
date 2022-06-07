# Install the package devtools: (one time only)
  install.packages("devtools")


# Load the devtools package and install the github.
devtools::install_github("jmzobitz/NEONSoils", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)


# Now that we have the data ready, then we can acquire the data
# Define the name of the site (SJER), the start and end dates, and where you will save them.  This will download the data and save it to your folder.

# NOTE: you will need to say y/n at several points here

acquire_neon_data(site_name ="SJER",
                  start_date = "2020-05",
                  end_date = "2020-05",
                  file_name = "process-files/my-file.Rda")

# Then process and compute the fluxes from that data file.
out_fluxes <- compute_neon_flux("process-files/my-file.Rda")


# This whole process takes some time - so be patient!
