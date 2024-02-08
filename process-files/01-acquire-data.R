# author: John Zobitz \email{zobitz@augsburg.edu}
# last revised: 10-28-21

# description: acquire NEON data across 5 sites (SJER, WREF, KONZ, WOOD, and UNDE) for a range of quarters from 2019 (as of this date, 2020 data were provision)

library(tidyverse)
library(NEONSoils)
library(lubridate)



dates <- tibble(times = seq(ymd("2019-01-01"),ymd("2019-12-01"),by="months")) %>%
  separate(times,into=c("year","month",NA),sep="-") %>%
  unite(col="times",sep="-")


# Define a nested tibble of different start and end times
my_time <- tibble(
                  start_dates = dates$times,
                  end_dates = dates$times
                  ) %>%
  nest(data = everything())

# Define a tibble adding on the times
my_sites <- tibble( sites = c("SJER","WREF","KONZ","WOOD","UNDE"),
                    times = my_time$data) %>%
  unnest(cols=c(times)) %>%
  mutate(filename = pmap(.,~paste0("flux-results/",..1,"-",..2)))

# Cool!  Now we are ready to iterate through!

# Now that we have the data ready, then we can acquire the data
# Define the name of the site (SJER), the start and end dates, and where you will save them.  This will download the data and save it to your folder.

# NOTE: you will need to say y/n at several points here

# This "starts" the stopwatch
start_time <- Sys.time()

acquire_neon_data(site_name =my_sites$sites[[1]],
                  start_date = my_sites$start_dates[[1]],
                  end_date = my_sites$end_dates[[1]],
                  data_file_name = "data-neon/test.Rda")

# Then process and compute the fluxes from that data file.
out_fluxes <- compute_neon_flux("data-neon/test.Rda")

### Save the file:
save(out_fluxes,file=my_sites$filename[[1]])

end_time <- Sys.time()


####
acquire_neon_data(site_name ="SJER",
                  start_date = "2020-02",
                  end_date = "2020-02",
                  data_file_name = "flux-jan-june/flux-feb.Rda")

# Then process and compute the fluxes from that data file.
out_fluxes_feb <- compute_neon_flux("flux-jan-june/flux-feb.Rda")

####  # Repeat as necessary
# Combine all the fluxes together
out_fluxes <- rbind(out_fluxes_jan,out_fluxes_feb)


# This whole process takes some time - so be patient!
