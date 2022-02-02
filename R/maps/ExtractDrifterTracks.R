#Extract time and position info for each drift
#Save in a dataframe
#Anne Simonis

source('C:/Users/anne.simonis.NMFS/Documents/code/Drifter-master/cutoutliers.R')

station.numbers<-c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23)
MapDir='D:/code/CCE Map Data'
spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                     "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                     "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                     "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                     "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                     "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv"))
DriftFile='Drift_FileLookup.csv'
lookupfile="spotlookup_US&MX_RETRIEVED.csv"

setwd(MapDir)
#Load in lookup file
lookup = read.csv(lookupfile)
ncsvfiles = length(spotcsvfile[[1]])

# combine GPS locations from multiple csv files into a single table, with most recent data at the top
spotcsv = read.csv(spotcsvfile[[1]][ncsvfiles],header=FALSE)
if(ncsvfiles>1){
  for(p in (ncsvfiles-1):1){spotcsv = rbind(spotcsv,read.csv(spotcsvfile[[1]][p],header=FALSE))}
  if(!is.null(station.numbers)){  # if user has specified only a subset of DASBRs, this filters the data accordingly
    spotcsv = spotcsv[spotcsv$V2 %in% lookup$spot.number[lookup$station %in% station.numbers] , ]
  }}

colnames(spotcsv) = c("dateTime", "spotID", "readingType", "lat", "long")
dateTime = strptime(spotcsv$dateTime, "%m/%d/%Y %H:%M")
n.stations = length(station.numbers)

AllTracks<-data.frame()
for(n in 1:n.stations){
stationInd<-which(lookup$station==station.numbers[n])
startdate<-min(lookup$dateTimeStart[stationInd])
startdate<-as.Date(startdate,format="%m/%d/%Y %H:%M")

enddate<-max(lookup$dateTimeEnd[stationInd])
enddate<-as.Date(enddate,format="%m/%d/%Y %H:%M")

# spot data for station n
data.n <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==station.numbers[n]], ]  
# date-time info (in date time format) for station n
dateTime.n <- dateTime[spotcsv$spotID %in% lookup$spot.number[lookup$station==station.numbers[n]]]
data.n.trunc <- data.n[dateTime.n >= strptime(unique(lookup$dateTimeStart[lookup$station==station.numbers[n]]), "%m/%d/%Y %H:%M")
                       & dateTime.n <= strptime(unique(lookup$dateTimeEnd[lookup$station==station.numbers[n]]), "%m/%d/%Y %H:%M"), ]
data.n.trunc$dateTime<-strptime(data.n.trunc$dateTime,format="%m/%d/%Y %H:%M")
data.n.trunc<-select(data.n.trunc,dateTime,spotID,lat,long)

#Remove outliers based on speed between detections (set to 4 km/hour here)
data.n.trunc<-cutoutliers(data.n.trunc,4)
data.n.trunc<-arrange(data.n.trunc,dateTime)

data.n.trunc$station<-station.numbers[n]
AllTracks<-rbind(AllTracks,data.n.trunc)
}

save(AllTracks,file="DriftPositions.rda")
