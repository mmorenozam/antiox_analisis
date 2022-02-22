#individual farm profiles data preparation
library(tidyverse)

wd <- getwd()

raw_df <- read.csv(paste0(wd,"/data/flav.txt"),sep = '\t')
zn_cor <- read.csv(paste0(wd,"/data/zones.csv"))
colnames(raw_df)
ant_df <- raw_df[,c(2,3,4,5,7,8,9,12)]
colnames(ant_df)[1:8] <- c("f.numb","s.numb",
                           "sampling.date","opening.date","dry.str.date",
                           "dry.end.date","moisture","antiox")

ant_df$year <- as.numeric(strftime(as.Date(ant_df$sampling.date),format = "%Y"))

ant_df$week.no <- as.numeric(strftime(as.Date(ant_df$dry.end.date),format="%V"))-28
ant_df$week.no <- ifelse(ant_df$year != 2016, ant_df$week.no + 52, ant_df$week.no)
ant_df <- left_join(ant_df,zn_cor,by=c("f.numb"="fnum"))
ant_df$week.no <- ifelse(ant_df$zone==3,ant_df$week.no-1,ant_df$week.no)
ant_df$zone <- NULL
wdf <- data.frame(expand.grid(0:41,ant_df$f.numb))
colnames(wdf) <- c("week","farm")

ant <- left_join(wdf,ant_df,by=c("week"="week.no","farm"="f.numb"),keep=F)
ant$year <- NULL

ant <- left_join(ant,zn_cor,by=c("farm"="fnum"))

ant <- subset(ant,zone==2|zone==3)

ggplot(ant,aes(x=week,y=antiox,group=farm))+
  geom_line()+
  # geom_line(data=ant[!is.na(ant$antiox),])+
  geom_point()+
  facet_grid(zone~.)

