

library(here)
library(dplyr)
library(marmap)
library(lubridate)
library(ggplot2)
library(ggnewscale)
library(viridis)

#Load in drifter tracks
Drifts<-readRDS('C:\\Users\\anne.simonis\\Documents\\GitHub\\ADRIFT_Report\\data\\AllDeploymentGPS.rds')
Drifts$Year<-year(Drifts$UTC)
Drifts$Month<-month(Drifts$UTC)

Drifts202303<-filter(Drifts,Month==3 & Year==2023)
Drifts202307<-filter(Drifts,Month==7 & Year==2023)
Drifts202311<-filter(Drifts,Month==11 & Year==2023)

#Load in Wind Call Boundary
WEA<-readRDS('C:\\Users\\anne.simonis\\Documents\\GitHub\\ADRIFT_Report\\data\\map\\WindCallBoundary.rdata')

#Load in beaked whale detections
beakedwhales<-c("ZC","BB")
# beakedwhales<-c("ZC","BB","MS","MC","BW","BW43","BWC")
dolphinspecies<-c('Lo','Gg','UO')

Detections<-read.csv('C:\\Users\\anne.simonis\\Documents\\GitHub\\ADRIFT_Report\\data\\AllDetections_wGPS.csv')
Detections$DateTime<-as.POSIXct(Detections$UTC,format='%Y-%m-%d %H:%M:%OS')
Detections$Month<-month(Detections$DateTime)
Detections$Year<-year(Detections$DateTime)

bwAdrift<-filter(Detections,species %in% beakedwhales)
bw202303<-filter(bwAdrift,Month==3 & Year==2023)
bw202307<-filter(bwAdrift,Month==7 & Year==2023)
bw202311<-filter(bwAdrift,Month==11 & Year==2023)

bw03<-filter(bwAdrift,Month==3)
bw07<-filter(bwAdrift,Month==7)
bw11<-filter(bwAdrift,Month==11)

dolphins<-filter(Detections,species %in% dolphinspecies)
dolphins202303<-filter(dolphins,Month==3 & Year==2023)
dolphins202307<-filter(dolphins,Month==7 & Year==2023)
dolphins202311<-filter(dolphins,Month==11 & Year==2023)

lon1=-123
lon2=-120.3
lat1=34.8
lat2=36
#Extract bathymetry data from NOAA (saves local file to speed future performance)
bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)

#Create color palettes
blues<-c("royalblue4","royalblue3",
         "royalblue2","royalblue1")
greys<-c(grey(0.8),grey(0.93),grey(0.99))
SpColor<-rainbow(length(beakedwhales))

#March2023
png(here('figs','BW_202303.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='March 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202303$Longitude,Drifts202303$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(beakedwhales)){
  sub<-filter(bw202303,species==beakedwhales[s])
  if(nrow(sub)>0){
points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
legend('bottomright', legend=c('Cuvier\'s beaked whale','Baird\'s beaked whale'), pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()

#Dolphins
png(here('figs','Dolphins_202303.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='March 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202303$Longitude,Drifts202303$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(dolphinspecies)){
  sub<-filter(dolphins202303,species==dolphinspecies[s])
  if(nrow(sub)>0){
    points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
legend('bottomright', legend=dolphinspecies, pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()

#July2023
png(here('figs','BW_202307.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='July 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202307$Longitude,Drifts202307$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(beakedwhales)){
  sub<-filter(bw202307,species==beakedwhales[s])
  if(nrow(sub)>0){
    points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
# legend('bottomright', legend=beakedwhales, pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
legend('bottomright', legend=c('Cuvier\'s beaked whale','Baird\'s beaked whale'), pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()

#Dolphins
png(here('figs','Dolphins_202307.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='July 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202307$Longitude,Drifts202307$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(dolphinspecies)){
  sub<-filter(dolphins202307,species==dolphinspecies[s])
  if(nrow(sub)>0){
    points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
legend('bottomright', legend=dolphinspecies, pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()

#November2023
png(here('figs','BW_202311.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='November 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202311$Longitude,Drifts202311$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(beakedwhales)){
  sub<-filter(bw202311,species==beakedwhales[s])
  if(nrow(sub)>0){
    points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
# legend('bottomright', legend=beakedwhales, pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
legend('bottomright', legend=c('Cuvier\'s beaked whale','Baird\'s beaked whale'), pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()

#Dolphins
png(here('figs','Dolphins_202311.png'),width=5,height=5,units="in",res=300)
plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
           main='November 2023', asp=NA)
scaleBathy(bat, deg=1, x="bottomleft", inset=5)
plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(WEA$geometry, add=TRUE, border='purple', lwd=1,lty=3)
points(Drifts202311$Longitude,Drifts202311$Latitude,col=grey(0.5),pch=19,cex=.2)
for(s in 1:length(dolphinspecies)){
  sub<-filter(dolphins202311,species==dolphinspecies[s])
  if(nrow(sub)>0){
    points(sub$Longitude, sub$Latitude, col=SpColor[s], pch=19,cex=.7)}}
legend('bottomright', legend=dolphinspecies, pch=19,cex=.8,pt.cex = 1, col=SpColor, bg="white")
dev.off()



