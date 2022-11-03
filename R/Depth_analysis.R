

library(dplyr)
library(lubridate)
library(ggplot2)

##Opportunistic
# BottomFile<-'E:/Recordings/PAST2022/Depth Data/ArrayJ02/Bottom_U16639/U16639_20220218_2029.csv'
# TopFile<-'E:/Recordings/PAST2022/Depth Data/ArrayJ02/Top_U16549/U16549_20220218_2029.csv'

# #Ogger
# BottomFile<-'E:/Recordings/PAST2022/Depth Data/SU-16583.csv'
# TopFile<-'E:/Recordings/PAST2022/Depth Data/SU-16557.csv'

# #PIFSC
# BottomFile<-'E:/Recordings/PAST2022/Depth Data/SU-16597.csv'
# TopFile<-'E:/Recordings/PAST2022/Depth Data/SU-16656.csv'

#Telescope
BottomFile<-'E:/Recordings/PAST2022/Depth Data/SU-16647.csv'
TopFile<-'E:/Recordings/PAST2022/Depth Data/SU-16595.csv'

DepthB<-read.csv(BottomFile,header=FALSE)
DepthB<-rename(DepthB,year=V4,month=V5,day=V6,hour=V7,min=V8,sec=V9,Esec=V10,Pressure=V11,Temp=V12)
DepthB$StartTime<-as.POSIXct(paste(DepthB$year,DepthB$month,DepthB$day,DepthB$hour,DepthB$min,sep='-'),format='%Y-%m-%d-%H-%M',tz='UTC')
DepthB<-select(DepthB,Pressure,Temp,StartTime,Esec)
DepthB$dateTime<-as.POSIXct(DepthB$StartTime+DepthB$Esec,tz="UTC")
DepthB$Depth<- -DepthB$Pressure/100
DepthB$Position<-'Bottom'

DepthT<-read.csv(TopFile,header=FALSE)
DepthT<-rename(DepthT,year=V4,month=V5,day=V6,hour=V7,min=V8,sec=V9,Esec=V10,Pressure=V11,Temp=V12)
DepthT$StartTime<-as.POSIXct(paste(DepthT$year,DepthT$month,DepthT$day,DepthT$hour,DepthT$min,sep='-'),format='%Y-%m-%d-%H-%M',tz='UTC')
DepthT<-select(DepthT,Pressure,Temp,StartTime,Esec)
DepthT$dateTime<-as.POSIXct(DepthT$StartTime+DepthT$Esec,tz="UTC")
DepthT$Depth<- -DepthT$Pressure/100
DepthT$Position<-'Top'

Depth<-rbind(DepthB,DepthT)
Depth$Position<-as.factor(Depth$Position)
Depth<-filter(Depth,year(dateTime)==2022 & dateTime<as.POSIXct("2022-02-19 00:00:00",tz='UTC'))
Depth<-filter(Depth,Depth>-200)

# DepthSel_P<-filter(Depth,dateTime> as.POSIXct("2022-02-17 20:35:00",tz='UTC') & dateTime< as.POSIXct("2022-02-18 00:00:00",tz='UTC') )

ggplot(Depth,aes(dateTime,Depth,col=Position))+geom_point()+ggtitle('Telescope')
# ggplot(DepthSel,aes(dateTime,Depth,col=Position))+geom_point()

setwd('E:/Recordings/PAST2022/Depth Data')
ggsave(filename='PAST22_Telescope_Depth.jpeg',device='jpeg',height=4,width=10,units="in")


