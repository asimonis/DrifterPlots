#Calculate distance of each drift
#Anne Simonis 28 April 2020


library(geosphere)
library(dplyr)
source('~/CCE DASBR/code/cutoutliers.R')

Drifts<-read.csv('C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/EventLocations/2020-04-27/DriftTracks.csv')

DriftDist<-matrix(nrow=length(unique(Drifts$station)))
                 
for(s in 1:length(unique(Drifts$station))){
  data<-filter(Drifts,station==unique(Drifts$station)[s])
  data$dateTime<-as.POSIXct(data$dateTime,tz="UTC")
  
  DriftMin<-seq.POSIXt(data$dateTime[1],data$dateTime[nrow(data)],by="1 min")
  fLat<-approxfun(data$dateTime,data$lat) #Interpolation function for latitudes
  fLong<-approxfun(data$dateTime,data$long) #Interpolation function for longitudes
  iLat<-fLat(DriftMin) 
  iLong<-fLong(DriftMin)
  Drift<-data.frame(lat = iLat, long = iLong,dateTime = DriftMin)
  data<-Drift
  
  data<-cutoutliers(data,4)
  c= length(data$lat)
  dist= distGeo(cbind(data$long[1:(c-1)],data$lat[1:(c-1)]),cbind(data$long[2:c],data$lat[2:c])) / 1000
  dist[c]= 0
  
  DriftDist[s]<-sum(dist)
  
}





