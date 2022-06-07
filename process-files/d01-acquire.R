# Load up the requisite data products.  Can only do one at a time unfortunately.

# Do this one at a time.  Focus on the SJER site
# We want the expanded data form

# acquire_neon_data
# interpolate_neon_data
# qc_neon_data
# flux_compute




library(neonUtilities)

sjer_megapit <- neonUtilities::loadByProduct(dpID="DP1.00096.001",
                              site=c("SJER"),
                              #startdate="2020-05",
                              #enddate="2020-08",
                              package="expanded")


sjer_temp <- neonUtilities::loadByProduct(dpID="DP1.00041.001",
                          site=c("SJER"),
                          startdate="2020-05",
                          enddate="2020-08",
                          package="expanded")


sjer_swc <- neonUtilities::loadByProduct(dpID="DP1.00094.001",
                          site=c("SJER"),
                          startdate="2020-05",
                          enddate="2020-08",
                          package="expanded")

sjer_press <- neonUtilities::loadByProduct(dpID="DP1.00004.001",
                          site=c("SJER"),
                          startdate="2020-05",
                          enddate="2020-08",
                          package="expanded")

# Save the files
save(sjer_co2,sjer_press,sjer_swc,sjer_temp,file="data-neon/sjer-2020-05-2020-08.Rda")

# Save the megapit data separately
save(sjer_megapit,file="data-neon/sjer-megapit-2020-05-2020-08.Rda")

# Adds each element to your data frame.  Cool!
#list2env(sjer_swc, .GlobalEnv)
