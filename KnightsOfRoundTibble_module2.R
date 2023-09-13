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
#Computer specific speed by speed/ body length for each fish 
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#Plot fin amplitudes over specific speeds
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
#Left fin amplitude for a specific date
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()
