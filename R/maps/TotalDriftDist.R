library(xlsx)

setwd('C:/Users/anne.simonis.NMFS/Documents/Tethys working folder')
buoyfiles<-dir(pattern='ADRIFT')
buoyfiles<-buoyfiles[2:6]

totaldistTop<-as.numeric()
totaldistBoth<-as.numeric()
for(b in 1:length(buoyfiles)){
BuoyTable<-read.xlsx(buoyfiles[b],sheetName='GPS')
totaldistBoth[b]<-sum(BuoyTable$Distance,na.rm=TRUE)/1000

TopSpot<-unique(BuoyTable$DeviceName)[1]
BuoyTable<-filter(BuoyTable,DeviceName==TopSpot)

totaldistTop[b]<-sum(BuoyTable$Distance)/1000
}
totaldistBoth
