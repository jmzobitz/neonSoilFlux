#Author: LL
#Purpose: Volumentric Water Content  QF Fails in June
#According to NEON.DOC.000007vC, QF =1 if quality test fail

library(tidyverse)
library(readr)
june_QF_volumetric <- read_csv("june_QF_volumetric.csv")

glimpse(june_QF_volumetric)

#Periods where flags fail (VSICFinalQF = 1)
QF_fail <- june_QF_volumetric %>%
  select(startDateTime, endDateTime,VSICFinalQF)%>%
  filter(VSICFinalQF == 1)
