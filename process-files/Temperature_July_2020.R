#Author: KO
#Purpose: Temperature QF fails in July 2020

library(tidyverse)
library(readr)
library(neonUtilities)
library(dplyr)

sjer_july_temperature<-loadByProduct(dpID="DP1.00041.001",
                                  site=c("SJER"),
                                  startdate = "2020-07",
                                  enddate = "2020-07",
                                  package="expanded")
y

#Flags fail when QF=1
qfFailTempJuly<-sjer_july_temperature[["ST_30_minute"]] %>%
  select(startDateTime,endDateTime,finalQF) %>%
  filter(finalQF==1)%>%
  arrange(startDateTime)


