#Function to plot a map with acoustic events of interest
#Inputs include: 
#ClickEventFile = R data file containing dataframe including columns for UTC (POSIXct), Latitude,Longitude,eventId, and station 
#DriftTrackFile = CSV file containing columns for dateTime, lat, long, station
#extent = options include 'CCES' or 'Other'. Other allows user to define map boundaries
#savedir = path where plots should be saved
#speciesID = vector of species codes to plot. 
#station.numbers = vector of station numbers to drift

EasyDriftMap<-function(ClickEventFile = 'D:/Analysis/Acoustic Studies/Alex_CCES_ClickEvents.rda',
                  DriftTrackFile='D:/Analysis/CCES/CCES2018_DriftTracks.rda',
                  extent='CCES',
                  savedir='D:/Analysis/CCES/Odonotocetes/Dolphins_SpermWhale/Acoustic Studies/EventLocations',
                  speciesID=c('LO','GG','UD'),
                  station.numbers=c(7,8,14)){

#Required libraries
library(marmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(scales)
library(PAMpal)

IDrift<-function(DriftTracks){
    DriftMin<-seq.POSIXt(DriftTracks$dateTime[1],DriftTracks$dateTime[nrow(DriftTracks)],by="1 min")
    fLat<-approxfun(DriftTracks$dateTime,DriftTracks$lat) #Interpolation function for latitudes
    fLong<-approxfun(DriftTracks$dateTime,DriftTracks$long) #Interpolation function for longitudes
    iLat<-fLat(DriftMin) 
    iLong<-fLong(DriftMin)
    Drift<-data.frame(lat = iLat, long = iLong,dateTime = DriftMin,station=rep(unique(DriftTracks$station),length(iLat)))
    return(Drift)
  }
  
#Create folder to save maps
dir.create(paste(savedir,"EventLocations/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)

#Create Map Boundaries
# define boundaries of the map
if(extent=="CCES"){lon1=-130;lon2=-114;lat1=30;lat2=45}
if(extent=="Other"){
  lon1<-readline('Please enter the left boundary of the map [-180:180]: ')
  lon2<-readline('Please enter the right boundary of the map [-180:180]: ')
  lat1<-readline('Please enter the top boundary of the map [-90:90]: ')
  lat2<-readline('Please enter the bottom boundary of the map [-90:90]: ')
  lon1<-as.numeric(lon1); lon2<-as.numeric(lon2); lat1<-as.numeric(lat1); lat2<-as.numeric(lat2)
}

#Extract bathymetry data from NOAA (saves local file to speed future performance)
bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=4,keep=TRUE)
#Create color palettes
blues<-c("royalblue4","royalblue3",
         "royalblue2","royalblue1")
greys<-c(grey(0.8),grey(0.93),grey(0.99))

#Load Drifter Tracks
load(DriftTrackFile)

DriftTracks07<-IDrift(distinct(filter(AllTracks,station==7)))
DriftTracks08<-IDrift(distinct(filter(AllTracks,station ==8)))
DriftTracks14<-IDrift(distinct(filter(AllTracks,station ==14)))

#Load Acoustic Event Data
load(ClickEventFile)

#Filter Event Data based on map boundaries
ClickEvents<-filter(ClickEvents,Longitude<lon1 & Longitude>lon2)
ClickEvents<-filter(ClickEvents,Latitude<lat1 & Latitude>lat2)
#Create Plot
autoplot(bat,geom=c('tile'),coast=TRUE)+
  coord_cartesian(expand = 0)+
  geom_contour(aes(z=z),breaks=c(-100,-500,-1000),color="gray24")+
  scale_fill_gradientn(values = scales::rescale(c(-4000, -1000, -400,300, 2000)),
                       colors = c("midnightblue", "royalblue3",  "grey40","grey50", "grey80"),name="Depth (m)")+
  geom_point(data=DriftTracks07,aes(long, lat),size=.1)+
  geom_point(data=DriftTracks08,aes(long, lat),size=.1)+
  geom_point(data=DriftTracks14,aes(long, lat),size=.1)+
  new_scale_color()+
  geom_point(data=ClickEvents,aes(x=Longitude,y=Latitude,color=species),size=1)

# setwd(paste(savedir,"EventLocations/",Sys.Date(), sep=""))
species<-paste(speciesID,collapse='_')
stations<-paste(station.numbers,collapse='_')

ggsave(paste(savedir,"EventLocations/",Sys.Date(),'/CCES',stations,'_',species,'_Map.png',sep=""),width=6,height=6,units="in",dpi=300,device='png')
}
