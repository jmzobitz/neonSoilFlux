# Sample code to download a NEON data product

library(neonUtilities)

sjer_temp <- loadByProduct(dpID="DP1.00041.001",
                           site=c("SJER"),
                           startdate="2020-05",
                           enddate="2020-08",
                           package="expanded")
