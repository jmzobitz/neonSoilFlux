#Author: KO
#Purpose: Temperature QF fails in May 2020

library(tidyverse)
library(readr)
library(neonUtilities)
library(dplyr)

sjer_may_temperature<-loadByProduct(dpID="DP1.00041.001",
                                     site=c("SJER"),
                                     startdate = "2020-05",
                                     enddate = "2020-05",
                                     package="expanded")
y

#Flags fail when QF=1
qfFailTempMay<-sjer_may_temperature[["ST_30_minute"]] %>%
  select(startDateTime,endDateTime,finalQF)%>%
  filter(finalQF==1)%>%
  arrange(startDateTime)

