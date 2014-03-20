#########
#Anchoring
#########

rm(list=ls())

use.points<-readOGR("Output","use.spdf.shapefile")
use.dat<-use.points@data
use.dat$Anchor<-as.character(use.dat$Anchor)

str(use.dat)
summary(use.dat$Anchor)
unique(use.dat$Anchor)

anc.dat<-subset(use.dat, use.dat$Anchor=='anc'|use.dat$Anchor=='Anchor')
str(anc.dat)
