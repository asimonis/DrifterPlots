library(ggmap)
library(here)
library(dplyr)
library(ggplot2)
library(viridis)

Tracks<-readRDS(here('data','ADRIFT_Tracks.rds'))

CCES <- c(left = -132, bottom = 27, right = -114, top = 50)
CCESm<-get_stamenmap(CCES, zoom = 5, maptype = "toner-lite")

ADRIFT_010<-filter(Tracks,DriftName=="ADRIFT_010")
ADRIFT_010$Date<-as.Date(ADRIFT_010$UTC)

MapLim<-c(left = min(ADRIFT_010$Longitude)-2, 
          bottom = min(ADRIFT_010$Latitude)-2,
          right = max(ADRIFT_010$Longitude)+2,
          top = max(ADRIFT_010$Latitude)+2)

CCESm<-get_stamenmap(MapLim, zoom = 5, maptype = "toner-lite")

ggmap(CCESm) +  
  geom_point(data=ADRIFT_010,aes(Longitude,Latitude,colour=Date))+
  scale_color_viridis_c(,trans="date",direction=-1)
