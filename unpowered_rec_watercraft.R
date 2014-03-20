############
#UNPOWERED RECREATIONAL CRAFT
###########

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
use.dat<-use.dat.clean@data

rec.dat<-use.dat[use.dat$ActvtyT=="Dive"|
                    use.dat$ActvtyT=="Dive boat" | 
                    use.dat$ActvtyT=="Dive Flag" |
                  use.dat$ActvtyT=="Divers"|
                    use.dat$ActvtyT=="Kayak"|
                    use.dat$ActvtyT=="Paddle Board" | 
                    use.dat$ActvtyT== "Wind Surfer" | 
                    use.dat$ActvtyT=="Swimmer",]

#number of surveys conducted
a<-ddply(use.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1))
a$INT.rep<-remove.digits(a$INT)
survey.freq<-ddply(a, . (TrnsctT, DayType, Period ), summarise, Freq = length(INT.rep))
survey.freq

#freq rec activites surveys for when activities were found in a survey
b<-ddply(rec.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1))
b

#surveys where no rec acitivites found
no.rec.INT<-outersect(a$INT, b$INT)
no.rec.INT

#create zeros for this list 
no.rec.freq<-rep(0, length(no.rec.INT))
no.rec.df<-data.frame(INT=no.rec.INT, Freq=no.rec.freq)

rec<-data.frame(INT=b$INT,Freq=b$Freq)
rec.df<-rbind(rec,no.rec.df)

#create rest of dataframe
period<-grep.mult.choice(rec.df$INT,'Aft','Mid','Morn')
daytype<-grep.vectors(rec.df$INT, 'we','wk')
transect<-gsub("(we)|(wk)|(Mid)|(Morn)|(Aft)|[[:digit:]]", "", rec.df$INT)

rec.freq<-data.frame(Transect=transect,
                     DayType=daytype,
                     Period=period,
                     INT=rec.df$INT,
                     Freq=rec.df$Freq)

###sums per transect, day, period
rec.summary<-ddply(rec.freq,.(Transect, DayType, Period), summarize, n=length(INT), Sum=sum(Freq), Mean=round(mean(Freq),1), Max=max(Freq))                    
xtable(rec.summary, digits=1)
