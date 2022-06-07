#Author: KO
#Purpose: Pressure QF fails in May 2020

library(tidyverse)
library(readr)
library(neonUtilities)
library(dplyr)

sjer_may_pressure<-loadByProduct(dpID="DP1.00004.001",
                                 site=c("SJER"),
                                 startdate = "2020-05",
                                 enddate = "2020-05",
                                 package="expanded")
y

#Flags fail when QF=1
qfFailPresMay<-sjer_may_pressure[["BP_30min"]]%>%
  select(startDateTime,endDateTime,staPresFinalQF) %>%
  filter(staPresFinalQF==1)%>%
  arrange(startDateTime)


