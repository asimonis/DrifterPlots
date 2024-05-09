#Create KML file of GPS points to view with Windy.com or Google Earth
#PositionCSV = path to csv file with columns: Latitude, Longitude

library(readr)
library(dplyr)
library(sf)
library(mapview)

mkKML<-function(PositionCSV){
gps <- bind_rows(lapply(PositionCSV, read_csv,show_col_types = FALSE))
gps<-na.omit(gps)

spatial<-gps %>%
  st_as_sf(coords = c('Longitude','Latitude'),
           crs=4269,
           remove=FALSE)

#Verify positions are in the correct location
print(mapview(spatial))

KMLfilename<-paste0(substr(PositionCSV,1,nchar(PositionCSV)-3), 'kml')
st_write(spatial,KMLfilename,layer='MyData')
}
