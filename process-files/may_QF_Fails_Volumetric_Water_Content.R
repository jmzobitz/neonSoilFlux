#Author: LL
#Purpose: Volumentric Water Content  QF Fails in May
#According to NEON.DOC.000007vC, QF =1 if quality test fail

library(tidyverse)
library(readr)
may_QF_volumetric <- read_csv("may_QF_volumetric.csv")

glimpse(may_QF_volumetric)

#Periods where flags fail (VSICFinalQF = 1)
QF_fail <- may_QF_volumetric %>%
  select(startDateTime, endDateTime,VSICFinalQF)%>%
  filter(VSICFinalQF == 1)
