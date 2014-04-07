################
#anchoring
###############

library(plyr)
library(xtable)
library(ggplot2)
library(rgdal)
library(spatstat)
library(maptools)
library(stringr)
library(xtable)
source('R/functions.R')

rm(list=ls())

#READ IN DATA

#est data for plotting
est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))

#use data from 'create_use_spatial_data.R'
use.dat.clean<-readOGR("Output","use.spdf.shapefile")
use.dat<-data.frame(use.dat.clean@data,stringsAsFactors=FALSE)
use.dat$Anchor<-as.character(use.dat$Anchor)
use.dat$Anchor[ is.na(use.dat$Anchor) ] <- 0
anc.dat<-use.dat[use.dat$Anchor!=0,]


#number of surveys conducted
a<-ddply(use.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1))
a$INT.rep<-remove.digits(a$INT)
survey.freq<-ddply(a, . (TrnsctT, DayType, Period ), summarise, Freq = length(INT.rep))
survey.freq

#freq ancoring activites surveys for when activities were found in a survey
b<-ddply(anc.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1))
b

#surveys where no anchoring acitivites found
no.anc.INT<-outersect(a$INT, b$INT)
no.anc.INT

#create zeros for this list 
no.anc.freq<-rep(0, length(no.anc.INT))
no.anc.df<-data.frame(INT=no.anc.INT, Freq=no.anc.freq)

anc<-data.frame(INT=b$INT,Freq=b$Freq)
anc.df<-rbind(anc,no.anc.df)

#create rest of dataframe
period<-grep.mult.choice(anc.df$INT,'Aft','Mid','Morn')
daytype<-grep.vectors(anc.df$INT, 'we','wk')
transect<-gsub("(we)|(wk)|(Mid)|(Morn)|(Aft)|[[:digit:]]", "", anc.df$INT)

anc.freq<-data.frame(Transect=transect,
                     DayType=daytype,
                     Period=period,
                     INT=anc.df$INT,
                     Freq=anc.df$Freq)

anc.summary<-ddply(anc.freq,.(Transect, DayType, Period), summarize, n=length(INT), Sum=sum(Freq), Mean=round(mean(Freq),1), Max=max(Freq)) 
xtable(anc.summary, digits=1)
