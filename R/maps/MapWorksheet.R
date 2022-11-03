source('C:/Users/anne.simonis.NMFS/Documents/GitHub/DrifterPlots/R/maps/cutoutliers.R')
source('C:/Users/anne.simonis.NMFS/Documents/code/Drifter-master/map.comment.events.r')
source('C:/Users/anne.simonis.NMFS/Documents/code/Drifter-master/Drift.map.events.r')
source('C:/Users/anne.simonis.NMFS/Documents/code/Drifter-master/Drift.map.comment.events.r')
source('C:/Users/anne.simonis.NMFS/Documents/code/Drifter-master/map.events.r')

#Create a single map with multiple drifts and events
# map.events(outfilename = "DASBRmap_Alldrifts", station.numbers=c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23),
#            speciesID=list(c("ZC"),c("BB","MS"),c("BW43","BW37V","BWC"),c("BW"),c("?BW"),("PM")),
#            MapDir='D:/code/CCE Map Data',
#            DBDir='D:/CCES/CCES PAMGUARD Analyses 2_00_16/Databases/Final Databases',
#            DriftFile='Drift_FileLookup.csv',
#            extent="CCES",figtitle='Drifts 4-23')

map.events(outfilename = "EventMap", station.numbers=c(8),
           speciesID=list(c("GG","LO")),
           MapDir='D:/code/CCE Map Data',
           DBDir='D:/Click Analysis/Alex Databases',
           DriftFile='Drift_Dolphins_FileLookup.csv',DriftPositions='D:/code/CCE Map Data/DriftPositions.rda',
           extent="CCES",figtitle='Drifts 8',SpColor=c('red'))

map.eventsBp(outfilename = "EventMap", station.numbers=c(7,8,10,12,13,14,16,18,19,20,21,22,23),
           speciesID=list(c("fin whale")),
           MapDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/Fin whales/Maps',
           DBDir='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/Fin whales/Databases',
           DriftFile='FinWhale_FileLookup.csv',DriftPositions='C:/Users/anne.simonis.NMFS/Documents/ADRIFT/Fin whales/Maps/DriftPositions.rda',
           extent="CCES",figtitle='Fin Whales',SpColor=c('red'))



#Create map of all drifts without any events 
map.drifts(outfilename = "Alldrifts", station.numbers=c(1:23),
                 MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                 spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                      "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                      "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                      "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                      "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                      "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
                 shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_All.csv", showStudyBound=FALSE, extent="CCES",figtitle='All Drifts')


# Create maps for individual drifts using the same color scheme 
Drift.map.events(outfilename = "DASBRmap", station.numbers=c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23),
                       speciesID=list(c("ZC","BB","MS","BW43","BW37V","BWC","BW")),
                       MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                       spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                            "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                            "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                            "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                            "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                            "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv")),
                       DriftFile='Drift_FileLookup.csv',
                       shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv",figtitle='CCES:2018')

#Find event info for a species of interest
EventInfoZc<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("ZC")), 
                              DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                  spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                       "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                       "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                       "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                       "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                       "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                  lookupfile="spotlookup_US&MX_RETRIEVED.csv")


map.comment.events(outfilename = "NBHF_Alldrifts", station.numbers=c(7,8,10,12,13,14,16,18,19,20,21,22,23),
                 speciesID=list(c("NBHF")),
                 MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/NBHF Data',
                 spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                      "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                      "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                      "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                      "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                      "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","DASBRs - Sept 27 to Oct 28.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv")),
                 DriftFile='NBHF_FileLookup.csv',
                 shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", showStudyBound=FALSE, extent="CCES",figtitle='Drifts 7-23')

Drift.map.comment.events(outfilename = "NBHF", station.numbers=c(7),
                         speciesID=list(c("NBHF")),
                         MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/NBHF Data',
                         spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                              "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                              "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                              "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                              "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                              "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv")),
                         DriftFile='NBHF_FileLookup.csv',
                         shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", figtitle='NBHF')


###
outfilename = "DASBRmap"
station.numbers=c(14)
speciesID=list(c("ZC","BB","MS","BW43","BW37V","BWC","BW"))
MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data'
DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data'
spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                     "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                     "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                     "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                     "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                     "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv"))

DriftFile='Drift_FileLookup.csv'
shiptrack.xy=NULL
lookupfile="spotlookup_US&MX_RETRIEVED.csv"
figtitle='CCES:2018'
extent="CCES"
showStudyBound=FALSE

####
outfilename = "NBHF_Alldrifts"
station.numbers=c(14)
speciesID=list(c("NBHF"))
ClassLabels<-c('<110 kHz','114-124 kHz','125-129 kHz','130-139 kHz','140+ kHz')
MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data'
DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data/NBHF Data'
spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                     "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                     "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                     "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                     "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                     "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv","SPOT_Sep27-Oct28.csv","SPOT_Oct28-Nov14.csv"))

DriftFile='NBHF_FileLookup.csv'
shiptrack.xy=NULL
lookupfile="spotlookup_US&MX_RETRIEVED.csv"
showStudyBound=FALSE
extent="CCES"
figtitle='Drifts 7-23'
SpColor<-c('green','orange','cyan','yellow','red','pink','lightblue','magenta2','firebrick2')

