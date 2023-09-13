#Load tidyverse
library(tidyverse)
#Read in data files
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
#Join "pseed" table with water tunnel speed data table, "speeds"
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
#Join pseed body lengths to thew new table 
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()