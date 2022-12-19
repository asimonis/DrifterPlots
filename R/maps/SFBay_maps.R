#Marmap tutorial: https://www.molecularecologist.com/2015/07/marmap/

#Required libraries
library(marmap)
library(RSQLite)
library(RColorBrewer)
library(dplyr)
library(sf)

sqlite <- dbDriver("SQLite")

## FUNCTION map.dasbr.events ##

# Created by AES, 5 October 2022
# Function plots the path of a group of DASBR 

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# GPSDir = folder with time and position data for each station
# showStudyBound = TRUE to show the CA Current tradtional study boundary, FALSE to suppress this
# extent, defines map extent.  Options are 'CCES' (default; CCES=US + MX), 'US', 'MX' or 'Other'
# figtitle = Title to appear at top of each figure
# See example after function code

SFmap <- function(outfilename = 'Map', station.numbers = c('ADRIFT_034','ADRIFT_035'), MapDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/ACCESS/',
                         GPSDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/ACCESS/GPS', 
                         showStudyBound=FALSE, extent="SF",figtitle=NULL){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(figtitle)){
    figtitle<-readline('What are you plotting? (this will be the main title of the figure): ')} 
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste("Maps/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)
  
  # define boundaries of the map
  if(extent=="CCES"){lon1=-132;lon2=-114;lat1=27;lat2=50}
  if(extent=="US") {lon1=-132;lon2= -116;lat1=30;lat2=50}
  if(extent=="SF") {lon1= -124;lon2= -122.2;lat1=38.4;lat2=36.8}
  if(extent=="Bodega") {lon1= -124;lon2= -122.7;lat1=38.5;lat2=37.7}
  if(extent=="Other"){
    lon1<-readline('Please enter the left boundary of the map [-180:180]: ')
    lon2<-readline('Please enter the right boundary of the map [-180:180]: ')
    lat1<-readline('Please enter the top boundary of the map [-90:90]: ')
    lat2<-readline('Please enter the bottom boundary of the map [-90:90]: ')
    lon1<-as.numeric(lon1); lon2<-as.numeric(lon2); lat1<-as.numeric(lat1); lat2<-as.numeric(lat2)
  }
  
  #Extract bathymetry data from NOAA (saves local file to speed future performance)
  bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)
  
  #Create color palettes
  blues<-c("royalblue4","royalblue3",
           "royalblue2","royalblue1")
  greys<-c(grey(0.8),grey(0.93),grey(0.99))
  dasbr.ptColor = "black"  # color for DASBR track and deployment points
  
  n.stations = length(station.numbers)
  
  #Load drifter time and position data
  setwd(GPSDir)
  gpsfiles<-dir(pattern="GPS")
  GPSData<-data.frame()
  for(g in 1:length(gpsfiles)){
    gps<-read.csv(gpsfiles[g])
    GPSData<-rbind(GPSData,gps)
  }
GPSData$UTC<-as.POSIXct(GPSData$UTC,tz='UTC')

  #Create Plot
    plotfile<-paste(MapDir,"/","Maps/",Sys.Date(),'/',outfilename,'.png', sep="")
    png(plotfile,width=5,height=5,units="in",res=300)
    plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0,
               main=figtitle, asp=NA)
    
    scaleBathy(bat, deg=0.225, x="bottomleft", inset=5)
    # plot(bat, deep=-200, shallow=-200, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
    plot(bat, deep=-1000, shallow=-1000, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
    
    #Import Sanctuary Boundaries
    dataPath <- 'C:/Users/anne.simonis.NMFS/Documents/code/Maps'
    mbnms <- readRDS(file.path(dataPath, 'MBNMS_Bounds.rda'))
    plot(mbnms$geometry, add=TRUE, border='gray60', lwd=1,lty=3)
    
    gfnms <- readRDS(file.path(dataPath, 'GFNMS_Bounds.rda'))
    plot(gfnms$geometry, add=TRUE, border='gray60', lwd=1,lty=3)
    text(-123.2, 37.9,labels='Greater Farallones \n National Marine Sanctuary', cex=.5,col='gray60')
    
    cbnms <- readRDS(file.path(dataPath, 'CBNMS_Bounds.rda'))
    plot(cbnms$geometry, add=TRUE, border='gray60', lwd=1,lty=3)
    text(-123.8, 38.25,labels='Cordell Bank \n National Marine Sanctuary', cex=.5,col='gray60')
    
    
    #Loop through data for each station and plot DASBR tracks
    drifterColors = c('#FCFDBFFF',"#9F2F7FFF")
    for(n in 1:n.stations){
      data.n.trunc<- filter(GPSData, DriftName==station.numbers[n])
      
      #Interpolate between SPOT GPS positions
      #Create vector of time
      DriftMin<-seq.POSIXt(data.n.trunc$UTC[1],data.n.trunc$UTC[nrow(data.n.trunc)],by="1 min")
      fLat<-approxfun(data.n.trunc$UTC,data.n.trunc$Latitude) #Interpolation function for latitudes
      fLong<-approxfun(data.n.trunc$UTC,data.n.trunc$Longitude) #Interpolation function for longitudes
      iLat<-fLat(DriftMin) 
      iLong<-fLong(DriftMin)
      Drift<-data.frame(Latitude = iLat, Longitude = iLong,UTC = DriftMin)  
      
      lines(Drift$Longitude,Drift$Latitude, col=drifterColors[n],lwd=2)  # plot the interpolated track
      points(Drift$Longitude[which.min(Drift$UTC)], 
             Drift$Latitude[which.min(Drift$UTC)], pch=15,cex=1)  # add points showing DASBR origins
      points(Drift$Longitude[nrow(Drift)],Drift$Latitude[nrow(Drift)], col=dasbr.ptColor, pch=17,cex=1)  # add points showing end time of DASBR recording
      
      # add station number labels
      #Offset parameter moves text label slightly away from deployment point; User may need to adjust offset for different map boundaries 
      offsetV<- 0
      offsetH<- -0.05
      # if(station.numbers[n] %in% c('ADRIFT_035')){offsetH<-.15}
      text(data.n.trunc$Longitude[2]+offsetH, data.n.trunc$Latitude[2]+offsetV,
           labels=gsub('ADRIFT_0','',station.numbers[n]), cex=1,col=drifterColors[n])
      
}
    
    legend('topright', legend=c("Start","Finish"),pch=c(15,17),cex=1, col=c("black","black"), bg="white")
    dev.off()
}


# SFmap(outfilename='ADRIFT 34_35',station.numbers =c('ADRIFT_034','ADRIFT_035'),
#       MapDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/ACCESS/',
#       GPSDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/ACCESS/GPS',
#       showStudyBound=FALSE,
#       extent="Bodega",
#       figtitle='ADRIFT 34 & 35')