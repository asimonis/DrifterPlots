#Marmap tutorial: https://www.molecularecologist.com/2015/07/marmap/

#Required libraries
library(marmap)
library(RSQLite)
library(RColorBrewer)
library(dplyr)
library(lubridate)

sqlite <- dbDriver("SQLite")

## FUNCTION map.dasbr.events ##

# Created by AES, 30 August 2019; Adapted from MAP.DASBR2 (by JEM, 3 Sept 2016)
# Function plots the path of a single DASBR drift and associated events 
# with event colors defined by comments in Pamguard database

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# speciesID = a list of the species ID's to include in the plot; e.g. list(c("ZC","BB","BW43","MS","MD","BW39V","BW70","BWC","IP","?BW","BW","BW26-47"))
# ClassLabels = a list of classes defined within comments of Pamguard acoustic events - these will determine the color of plotted events
# spotcsvfile = a list containing names of csv file/s containing date-time-locations for a set of spot devices, created using Advanced Download in the SPOT website
# shiptrack.xy = two-column matrix with longitude and latitude coordinates of the ship's progress, in decimal degrees
# lookup = lookup table indicating spotID and deployment period for each DASBR station.  LOOKUP MUST ONLY INCLUDE DEVICES THAT WILL BE PLOTTED.
# showStudyBound = TRUE to show the CA Current tradtional study boundary, FALSE to suppress this
# extent, defines map extent.  Options are 'CCES' (default; CCES=US + MX), 'US', 'MX' or 'Other'
# figtitle = Title to appear at top of each figure
# SpColor = a list of colors to use to plot event types; defaults to list of 9 contrasting colors
# SaveTracks = logical to save tracks for all drifts in a csv file
# See example after function code

Drift.map.comment.events <- function(outfilename, station.numbers = NULL, speciesID=NULL, ClassLabels=NULL,MapDir=NULL,DBDir=NULL,spotcsvfile, DriftFile='Drift_FileLookup.csv',
                                   shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv",figtitle=NULL,SpColor=NULL,SaveTracks=FALSE){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(DBDir)) {
    DBDir<-choose.dir(caption = "Choose folder containing databases with event info")} 
  if(is.null(figtitle)){
    figtitle<-readline('What are you plotting? (this will be the main title of the figure): ')} 
  if(is.null(ClassLabels)){
    ClassLabels<-c('<110 kHz','114-124 kHz','125-129 kHz','130-139 kHz','140+ kHz')}
  AllTracks<-data.frame()
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste("EventLocations/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)
  
  #Create color palettes
  blues<-c("royalblue4","royalblue3",
           "royalblue2","royalblue1")
  greys<-c(grey(0.8),grey(0.93),grey(0.99))
  dasbr.ptColor = "black"  # color for DASBR track and deployment points
  SPLabels<-unlist(unique(speciesID))
  if(is.null(SpColor)){
    SpColor<-c('green','orange','cyan','yellow','red','pink','lightblue','magenta2','firebrick2')} 
  
  #Load in lookup file
  lookup = read.csv(lookupfile)
  ncsvfiles = length(spotcsvfile[[1]])
  stations <- unique(station.numbers)
  
  lookup$dateTimeStart =  strptime(lookup$dateTimeStart, format="%m/%d/%y %H:%M")
  lookup$dateTimeEnd =  strptime(lookup$dateTimeEnd, "%m/%d/%y %H:%M")
  lookup$dateTimeStart<-as.POSIXct(lookup$dateTimeStart,tz="UTC")
  lookup$dateTimeEnd<-as.POSIXct(lookup$dateTimeEnd,tz="UTC")

  # combine GPS locations from multiple csv files into a single table, with most recent data at the top
  spotcsv = read.csv(spotcsvfile[[1]][1],header=FALSE,stringsAsFactors=FALSE,sep=",", dec=".")
  
  if(ncsvfiles>1){
    for(p in 2:ncsvfiles){
      nextspot = read.csv(spotcsvfile[[1]][p],header=FALSE,stringsAsFactors=FALSE,sep=",")
      nextspot$V4 <-as.numeric(nextspot$V4)
      nextspot$V5 <-as.numeric(nextspot$V5)
      spotcsv = rbind(spotcsv,nextspot)}
    if(!is.null(station.numbers)){  # if user has specified only a subset of DASBRs, this filters the data accordingly
      spotcsv = spotcsv[spotcsv$V2 %in% lookup$spot.number[lookup$station %in% station.numbers] , ]
    }}
  
  colnames(spotcsv) = c("dateTime", "spotID", "readingType", "lat", "long")
  spotcsv$dateTime = strptime(spotcsv$dateTime, "%m/%d/%y %H:%M",tz="UTC")
  n.stations = length(station.numbers)
  
  #Read in Drift Database lookup for Event Info
  setwd(DBDir)
  DriftDB<-read.csv(DriftFile)
  
  #Create Plot
  #Loop through data for each station and plot DASBR tracks
  #OffsetV and offsetH parameter moves text label slightly away (V=vertical, H=horizontal)...
  #from deployment point; User may need to adjust offset for different map boundaries 
  offsetV<-0.2
  offsetH<-0
  
  for(n in 1:n.stations){
    if(is.na(lookup$dateTimeStart[n*2])) next  # for DASBRS that have not yet been deployed, skip to next record
    if(!is.na(lookup$dateTimeStart[n*2])){  # for DASBRS that have been deployed...
      outfilenameN<-paste(outfilename,'Drift',station.numbers[n],sep="_")
      
      # spot data for station n, only select the first 5 columns
      data.n <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[n]], 1:5]  
      data.n$dateTime<-as.POSIXct(data.n$dateTime,tz="UTC")
      
      # truncate data to only include location records while DASBR was deployed, and plot the tracks
      LookupInd<-which(lookup$station==stations[n]) 
      if(!is.na(lookup$dateTimeEnd[LookupInd[1]])){ # for DASBRs that have been retrieved
        
        #Determine end of recording time and plot position 
        dbInd<-which(DriftDB$Drift==stations[n])
        conn <- dbConnect(sqlite,file.path(DBDir,DriftDB$Database[dbInd]))
        SoundAq<- dbReadTable(conn, "Sound_Acquisition")         #read offline events
        SoundAq$UTC<-strptime(SoundAq$UTC,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
        EndTime<-SoundAq$UTC[nrow(SoundAq)]
        EndTime<-as.POSIXct(EndTime, tz="UTC")
        #If pickup time is before end of recording, use pickup time (sometimes recorders are left on)
        if(lookup$dateTimeEnd[LookupInd[1]]<EndTime){EndTime<-lookup$dateTimeEnd[LookupInd[1]]}
        
        data.n.trunc<- filter(data.n, dateTime>=lookup$dateTimeStart[LookupInd[1]] & dateTime<=EndTime)
        
        #Remove outliers based on speed between detections (set to 1 km/hour here)
        data.n.trunc<-cutoutliers(data.n.trunc,4)
        data.n.trunc<-arrange(data.n.trunc,dateTime)
        if(SaveTracks==TRUE){
          data.n.trunc$station<-station.numbers[n]
          AllTracks<-rbind(AllTracks,data.n.trunc)}
          
        #Interpolate between SPOT GPS positions
        #Create vector of time
        DriftMin<-seq.POSIXt(data.n.trunc$dateTime[1],data.n.trunc$dateTime[nrow(data.n.trunc)],by="1 min")
        fLat<-approxfun(data.n.trunc$dateTime,data.n.trunc$lat) #Interpolation function for latitudes
        fLong<-approxfun(data.n.trunc$dateTime,data.n.trunc$long) #Interpolation function for longitudes
        iLat<-fLat(DriftMin) 
        iLong<-fLong(DriftMin)
        Drift<-data.frame(lat = iLat, long = iLong,dateTime = DriftMin)
        
        #Define buffer of area to plot around drifter track
        if(stations[n] %in% c(12,14,21)){buffer<-0.5} 
        if(stations[n] %in% c(7,12,16,17,18,20,22,23)){buffer<-1}
        if(stations[n] %in% c(4,8,10,13,19)){buffer<-2}
        lat1<-min(data.n.trunc$lat)-buffer
        lat2<-max(data.n.trunc$lat)+buffer
        lon1<-min(data.n.trunc$lon)-buffer
        lon2<-max(data.n.trunc$lon)+buffer
        
        #Extract bathymetry data from NOAA (saves local file to speed future performance)
        bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)
        
        #Plot
        # figtitleN = paste(figtitle,"Drift",stations[n],sep=" ")
        plotfile<-paste(MapDir,"/","EventLocations/",Sys.Date(),'/',outfilenameN,'.png', sep="")
        png(plotfile,width=4.5,height=6,units="in",res=300)
        plot.bathy(bat,image=TRUE,bpal=list(c(0,10,greys),c(min(bat),0,blues)),
                   land=TRUE,n=0,  deepest.isobath=-500, shallowest.isobath=-500,col='grey32',asp=NA)
        scaleBathy(bat, deg=0.12, x="bottomleft", inset=5)
        
        points(Drift$long,Drift$lat,col="Grey",pch='.')
        points(data.n.trunc$long, data.n.trunc$lat, col=dasbr.ptColor,pch=20,cex=.5)  # plot the track
        points(data.n.trunc$long[which.min(data.n.trunc$dateTime)], 
               data.n.trunc$lat[which.min(data.n.trunc$dateTime)], pch=15,cex=1.3)  # add points showing DASBR origins
        points(Drift$long[nrow(Drift)],Drift$lat[nrow(Drift)], col=dasbr.ptColor, pch=17,cex=1.3)  # add points showing end time of DASBR recording
        
        # add station number labels
        if(stations[n] %in% c(19)){offsetV<- -0.3}
        if(stations[n] %in% c(18)){offsetV<- -0.15}
        if(stations[n] %in% c(7,20)){offsetV<- -0.1}
        if(stations[n] %in% c(14)){offsetV<- 0}
        if(stations[n] %in% c(12,22)){offsetV<- 0.15}
        
        if(stations[n] %in% c(10)){offsetH<- 0.4}
        if(stations[n] %in% c(13,16)){offsetH<- 0.2}
        if(stations[n] %in% c(4,14,23)){offsetH<- 0.1}
        if(stations[n] %in% c(7,12,18,19,22)){offsetH<- -0.2}
        if(stations[n] %in% c(8)){offsetH<- -.5}
        text(data.n.trunc$long[2]+offsetH, data.n.trunc$lat[2]+offsetV, labels=stations[n], cex=1.5)
        
        text(lon2-(lon2-lon1)/10,lat1+(lat2-lat1)/15, labels=paste(round(as.numeric(EndTime - lookup$dateTimeStart[LookupInd[1]])),' Days',sep=""))
        
        
        #Load in Event Info from Database 
        Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
        Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)
        Events<-filter(Events,eventType %in% SPLabels)
        Events$dateTime<-strptime(Events$UTC,format="%Y-%m-%d %H:%M:%OS")
        Events$dateTime<-as.POSIXct(Events$dateTime, tz="UTC")
        
        #Compare Event Start Time with Time in Drift GPS data.frame (data.n.trunc)
        for(e in 1:nrow(Events)){
          TD<-as.numeric(difftime(Events$dateTime[e],Drift$dateTime,units="min"))
          GPSind<-which.min(abs(TD))
          Events$lat[e]<-Drift$lat[GPSind]
          Events$long[e]<-Drift$long[GPSind]}
        
        # add points showing NBHF locations based on center freq
        Events$comment<-as.numeric(gsub(" ", "", Events$comment, fixed = TRUE))
        Events$Class<-NA
        
        for(e in 1:nrow(Events)){
          if(Events$comment[e]<=110){Events$Class[e]<-'<110 kHz'}
          if(Events$comment[e]>114 && Events$comment[e]<=124){Events$Class[e]<-'114-124 kHz'}
          if(Events$comment[e]>124 && Events$comment[e]<=129){Events$Class[e]<-'125-129 kHz'}
          if(Events$comment[e]>129 && Events$comment[e]<=139){Events$Class[e]<-'130-139 kHz'}
          if(Events$comment[e]>139){Events$Class[e]<-'140+ kHz'}}
        
        for(t in 1:length(unique(Events$Class))){
          ColInd<-which(ClassLabels == unique(Events$Class)[t])
          SubEvent<-dplyr::filter(Events,Class==unique(Events$Class)[t])
          points(SubEvent$long, SubEvent$lat, col=SpColor[ColInd], pch=20,cex=.75)
          # points(SubEvent$long, SubEvent$lat, col='LightGray', pch=19,cex=.5)
        }
      }}
    legend('topright', legend=ClassLabels, pch=19,cex=1,pt.cex = 1, col=SpColor[1:length(ClassLabels)], bg="white")
    
    dev.off()}
  setwd(paste(MapDir,"/EventLocations/",Sys.Date(), sep=""))
  if(SaveTracks==TRUE){write.csv(AllTracks,file = 'DriftTracks.csv',row.names = FALSE)}
}

# Drift.map.comment.events(outfilename = "NBHF", station.numbers=c(7),
#                          speciesID=list(c("NBHF")),
#                          MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/NBHF Data',
#                          spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
#                                               "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
#                                               "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
#                                               "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
#                                               "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
#                                               "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv")),
#                          DriftFile='NBHF_FileLookup.csv',
#                          shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", figtitle='NBHF')



