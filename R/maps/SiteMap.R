#Map out all of our sites
#Anne Simonis 19 Dec 2022

library(ggmap)
library(here)
library(dplyr)
library(ggplot2)

Sites<-read.csv(here('data','Site list.csv'),header = TRUE)
Sites$N.Border<-as.numeric(substr(Sites$N.Border,1,5))
Sites$S.Border<-as.numeric(substr(Sites$S.Border,1,5))

CCES <- c(left = -132, bottom = 27, right = -114, top = 50)
CCESm<-get_stamenmap(us, zoom = 5, maptype = "toner-lite")
  
#Plot
ggmap(CCESm) +  
  geom_hline(data=Sites,aes(yintercept=Sites$N.Border,colour=factor(Site)))+
  geom_hline(data=Sites,aes(yintercept=Sites$S.Border,colour=factor(Site)))+
  geom_text(data=Sites,aes(x=rep(-130,16),y=S.Border,label=Site),size=2,nudge_y =.1 )+
  theme(legend.position = 'none')


