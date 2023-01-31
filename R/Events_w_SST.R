#Plot Acoustic Events with Environmental Data
#Anne Simonis
#23 March 2022

library(ggplot2)
library(dplyr)
library(PAMmisc)

#Load relevant data
load('E:/Analysis/CCES/Odonotocetes/Dolphins_SpermWhale/Acoustic Studies/Alex_CCES_ClickEvents.rda')

# #Load LO Visual Observation Data
# VisualObsFile = 'E:/Analysis/CCES/Odonotocetes/Dolphins_SpermWhale/Visual Observations/Lo_sightings_CCE_1991_2018.csv'
# Vis<-read.csv(VisualObsFile)
# Vis<-rename(Vis,Latitude=slat,Longitude=slon,UTC=)

#Get SST
edinfo<-browseEdinfo(var='sst')
AcousticWithEnv <- matchEnvData(ClickEvents, nc = edinfo, fileName = NULL, buffer = c(0, 0, 0))
Acoustic_Env_LO<-filter(AcousticWithEnv,species=="LO")

# VisualWithEnv <- matchEnvData(Vis, nc = edinfo, fileName = NULL, buffer = c(0, 0, 0))



#Plots with SST
ggplot(AcousticWithEnv,aes(species,sst_mean))+geom_boxplot()
ggplot(AcousticWithEnv,aes(sst_mean))+geom_histogram()+facet_grid(species~.)
ggplot(Acoustic_Env_LO,aes(species,sst_mean))+geom_boxplot()+
  theme_bw()+ylab('Average Sea Surface Temperature (C)')+xlab('')+
  ggtitle('SST at Pacific white-sided dolphin \n acoustic detections')

ggplot(Acoustic_Env_LO,aes(Longitude,Latitude,color=sst_mean))+
  geom_point()+scale_color_gradientn(values=seq(0,1,length.out = 4),
    colours=c('mediumorchid','blue','cyan','green'))+

  
  #Map
  #Required libraries
  library(marmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(scales)
library(PAMpal)

lon1=-130;lon2=-114;lat1=30;lat2=45

#Extract bathymetry data from NOAA (saves local file to speed future performance)
bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=4,keep=TRUE)
#Create color palettes
blues<-c("skyblue4","skyblue3",
         "skyblue2","skyblue1")
greys<-c(grey(0.8),grey(0.93),grey(0.99))

#Load Drifter Tracks
DriftTrackFile='C:/Users/anne.simonis.NMFS/Documents/GitHub/DrifterPlots/data/DriftPositions.rda'
load(DriftTrackFile)

IDrift<-function(DriftTracks){
  DriftMin<-seq.POSIXt(DriftTracks$dateTime[1],DriftTracks$dateTime[nrow(DriftTracks)],by="1 min")
  fLat<-approxfun(DriftTracks$dateTime,DriftTracks$lat) #Interpolation function for latitudes
  fLong<-approxfun(DriftTracks$dateTime,DriftTracks$long) #Interpolation function for longitudes
  iLat<-fLat(DriftMin) 
  iLong<-fLong(DriftMin)
  Drift<-data.frame(lat = iLat, long = iLong,dateTime = DriftMin,station=rep(unique(DriftTracks$station),length(iLat)))
  return(Drift)
}

DriftTracks07<-IDrift(distinct(filter(AllTracks,station==7)))
DriftTracks08<-IDrift(distinct(filter(AllTracks,station ==8)))
DriftTracks14<-IDrift(distinct(filter(AllTracks,station ==14)))

autoplot(bat,geom=c('tile'),coast=TRUE)+
  coord_cartesian(expand = 0)+
  # geom_contour(aes(z=z),breaks=c(-1000),color="gray24")+
  scale_fill_gradientn(values = scales::rescale(c(-4000, -1000, -400,300, 2000)),
                       colors = c("royalblue4", "skyblue3",  "grey40","grey50", "grey80"),name="Depth (m)")+
  geom_point(data=DriftTracks07,aes(long, lat),size=.1)+
  geom_point(data=DriftTracks08,aes(long, lat),size=.1)+
  geom_point(data=DriftTracks14,aes(long, lat),size=.1)+
  new_scale_color()+
  geom_point(data=Acoustic_Env_LO,aes(Longitude,Latitude,color=sst_mean),size=1.5)+
  scale_color_gradientn(values=seq(0,1,length.out = 4),
                        colours=c('mediumorchid','blue','cyan','green'))

setwd('E:/Analysis/CCES/Odonotocetes/Dolphins_SpermWhale/Plots')
ggsave('CCES_2018_LO_SST.pdf',device='pdf')


