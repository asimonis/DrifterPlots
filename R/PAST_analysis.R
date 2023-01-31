#Review GPS data from PAST deployments

# Plot speed of each deployment over time elapsed
# Plot distance of each deployment over time 


library(ggplot2)
library(lubridate)
library(dplyr)
library(xlsx)
library(gridExtra)
library(egg)
source('~/GitHub/DrifterPlots/R/maps/cutoutliers.R')


GPSfile<-'E:/Recordings/PAST2022/GPS Data/Past2022_GPS.csv'
gps<-read.csv(GPSfile)

gps$dateTime<-as.POSIXct(gps$UTC,tz="UTC")
gps$lat<-gps$Latitude
gps$long<-gps$Longitude

# Cut outliers
gpsMod<-cutoutliers(gps,3)
gpsMod<-filter(gpsMod,dist<0.75)

NoDrogue<-which(gpsMod$DriftName %in% c("MBTRIAL_001","MBTRIAL_002","MBTRIAL_003"))
Drogue<-which(gpsMod$DriftName %in% c("MBTRIAL_004","MBTRIAL_005","MBTRIAL_006"))
gpsMod$Treatment<-'NA'
gpsMod$Treatment[NoDrogue]<-'No Drogue'
gpsMod$Treatment[Drogue]<-'Drogue'


#Summary of total distance and average speeds
gpsSum<-gpsMod %>%
  group_by(DriftName) %>%
  summarize(TotalDist = sum(dist), MeanSpeed = mean(speed),
            Spot = unique(DeviceName),Start=min(dateTime),
            Stop=max(dateTime))

gpsDist<-gpsMod %>%
  group_by(DriftName)%>%
  summarize(TotalDist = cumsum(dist),dateTime=dateTime)

NoDrogue<-filter(gpsMod,Treatment=="No Drogue")
Drogue<-filter(gpsMod,Treatment=="Drogue")

#Plots
p1<-ggplot(NoDrogue,aes(dateTime,speed,col=DeviceName,fill=DeviceName))+geom_smooth()+
  scale_y_continuous(limits=c(0.2,1))+geom_point()+
  ggtitle('Past 2022: Buoy speed without drogue')+
  theme(plot.title = element_text(size=14))

p2<-ggplot(Drogue,aes(dateTime,speed,col=DeviceName,fill=DeviceName))+geom_smooth()+
  scale_y_continuous(limits=c(0.2,1))+geom_point()+
  ggtitle('Past 2022: Buoy speed with drogue')+
  theme(plot.title = element_text(size=14))

p3<-ggplot(NoDrogue,aes(DriftName,speed,fill=DeviceName))+geom_boxplot()+
  scale_y_continuous(limits=c(0,2))+
  ggtitle('Past 2022: Buoy speeds without drogue')+
  theme(plot.title = element_text(size=14))

p4<-ggplot(Drogue,aes(DriftName,speed,fill=DeviceName))+geom_boxplot()+
  scale_y_continuous(limits=c(0,2))+
  ggtitle('Past 2022: Buoy speeds with drogue')+
  theme(plot.title = element_text(size=14))

p5<-ggplot(gpsMod,aes(Treatment,speed))+geom_boxplot()+
  ggtitle('Past 2022: Combined Buoys with & without drogues')+
  theme(plot.title = element_text(size=14))

p6<-ggplot(gpsMod,aes(long,lat,color=speed))+geom_point()+
  facet_grid(~DeviceName)+scale_color_viridis()

p7<-ggplot(gpsDist,aes(TotalDist,dateTime,col=DriftName))+geom_point()+
  ggtitle('Past 2022: Cumulative Distance of Each Buoy')+
  theme(plot.title = element_text(size=14))


arrange<-ggarrange(p1,p2,p3,p4,p5,p7,ncol=2)
ggsave('PAST22_Buoy_comparison.jpeg',arrange,device = "jpeg",width=10,height=8,units = "in")


NoDrogue<-filter(Morning,Treatment=="No Drogue",dateTime<"2022-02-17 22:00:00" )
Drogue<-filter(Morning,Treatment=="Drogue",dateTime<"2022-02-18 22:00:00")

m1<-ggplot(NoDrogue,aes(dateTime,speed,col=DeviceName,fill=DeviceName))+geom_smooth()+
  scale_y_continuous(limits=c(0.2,1))+geom_point()+
  scale_x_datetime(date_labels = "%m/%d %H:%M")+
  ggtitle('Past 2022: Buoy speed without drogue')+
  theme(plot.title = element_text(size=14))

m2<-ggplot(Drogue,aes(dateTime,speed,col=DeviceName,fill=DeviceName))+geom_smooth()+
  scale_y_continuous(limits=c(0.2,1))+geom_point()+
  scale_x_datetime(date_labels = "%m/%d %H:%M")+
  ggtitle('Past 2022: Buoy speed with drogue')+
  theme(plot.title = element_text(size=14))
m5<-ggplot(Morning,aes(Treatment,speed))+geom_boxplot()+
  ggtitle('Past 2022: Combined Buoys with & without drogues')+
  theme(plot.title = element_text(size=14))

arrange<-ggarrange(m1,m2,m5,ncol=2)
ggsave('PAST22_Buoy_comparison_MidDay.jpeg',arrange,device = "jpeg",width=10,height=8,units = "in")
