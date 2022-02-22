#individual farm profiles data preparation
library(tidyverse)

wd <- getwd()

raw_df <- read.csv(paste0(wd,"/data/flav.txt"),sep = '\t')
zn_cor <- read.csv(paste0(wd,"/data/zones.csv"))
colnames(raw_df)

ant_df <- raw_df[,c(2,3,4,5,7,8,9,12)]

ant_df <- ant_df %>% left_join(.,zn_cor,by=c("farm.number"="fnum"))

ant_df <- subset(ant_df,zone==2|zone==3)

colnames(ant_df)[1:8] <- c("f.numb","s.numb","s.date","o.date","ds.date","de.date","moisture","antiox")

ant_df$week.no <- as.numeric(strftime(as.Date(ant_df$s.date),format="%V"))


ggplot(ant_df,aes(x=week.no,y=antiox,group=f.numb))+
  geom_point()+
  #geom_line()+
  facet_grid(zone~.)