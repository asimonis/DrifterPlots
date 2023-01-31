#Create uniform maps of PASCAL, CCES, ADRIFT drifts

library(marmap)
library(here)
library(dplyr)

ADRIFT<-readRDS(here('data','ADRIFT_Tracks.rds'))
ADRIFT$DeploymentID<-as.numeric(gsub('ADRIFT_0','',ADRIFT$DriftName))

Info<-read.csv(here('data','DeploymentDetails.csv'),header=TRUE)
Complete<-filter(Info,Status == 'Complete')

#Only save complete and recovered drifts
ADRIFT<-filter(ADRIFT,DeploymentID %in% unique(Complete$DeploymentID))

#CCES drifts
load(here('data','DriftPositions.rda'))
CCES<-AllTracks

#PASCAL
PASCAL<-read.csv(here('data','PASCAL_AllSpotTracks wUTC.csv'))


lon1=-132
lon2=-114
lat1=27
lat2=50
bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=4,keep=TRUE)

#Create color palettes
blues<-c("royalblue4","royalblue3",
         "royalblue2","royalblue1")
greys<-c(grey(0.8),grey(0.93),grey(0.99))


plotfile<-here(paste0('ADRIFT_',Sys.Date(),'.png'))
png(plotfile,width=4.5,height=6,units="in",res=300)

plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),
           land=TRUE,n=0, main='2021-2022', deepest.isobath=-500, 
           shallowest.isobath=-500,col='grey32',asp=NA)
scaleBathy(bat, deg=3.02, x="bottomleft", inset=5)
points(ADRIFT$Longitude,ADRIFT$Latitude,pch=20,cex=.01)  
dev.off()


plotfile<-here(paste0('CCES_',Sys.Date(),'.png'))
png(plotfile,width=4.5,height=6,units="in",res=300)

plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),
           land=TRUE,n=0, main='2018', deepest.isobath=-500, 
           shallowest.isobath=-500,col='grey32',asp=NA)
scaleBathy(bat, deg=3.02, x="bottomleft", inset=5)
points(CCES$long,CCES$lat,pch=20,cex=.01)  
dev.off


plotfile<-here(paste0('PASCAL_',Sys.Date(),'.png'))
png(plotfile,width=4.5,height=6,units="in",res=300)

plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),
           land=TRUE,n=0, main='2016', deepest.isobath=-500, 
           shallowest.isobath=-500,col='grey32',asp=NA)
scaleBathy(bat, deg=3.02, x="bottomleft", inset=5)
points(PASCAL$long,PASCAL$lat,pch=20,cex=.01)  
dev.off()
