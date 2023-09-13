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