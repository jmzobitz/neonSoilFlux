
# Define the name of the site (SJER), the start and end dates, and where you will save them.  This will download the data and save it to your folder.

# NOTE: you will need to say y/n at several points here

acquire_neon_data(site_name ="SJER",
                  start_date = "2020-05",
                  end_date = "2020-05",
                  file_name = "process-files/my-file.Rda")

# Then process and compute the fluxes from that data file.
out_fluxes <- compute_neon_flux("process-files/my-file.Rda")


# This whole process takes some time - so be patient!
