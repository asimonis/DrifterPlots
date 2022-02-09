

IDrift<-function(DriftTracks){

  DriftMin<-seq.POSIXt(DriftTracks$dateTime[1],DriftTracks$dateTime[nrow(DriftTracks)],by="1 min")
  fLat<-approxfun(DriftTracks$dateTime,DriftTracks$lat) #Interpolation function for latitudes
  fLong<-approxfun(DriftTracks$dateTime,DriftTracks$long) #Interpolation function for longitudes
  iLat<-fLat(DriftMin) 
  iLong<-fLong(DriftMin)
  Drift<-data.frame(lat = iLat, long = iLong,dateTime = DriftMin,station=rep(unique(DriftTracks$station),length(iLat)))
  return(Drift)
}

