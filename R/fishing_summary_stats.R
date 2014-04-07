###########
#Fishing summary statistics and plotting
###########
library(plyr)
library(xtable)
library(ggplot2)
library(rgdal)
library(spatstat)
library(maptools)
library(grid)
source('R/functions.R')

rm(list=ls())

#READ IN DATA

#est data for plotting
est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))

#use data from 'create_use_spatial_data.R'
use.dat.clean<-readOGR("Output","use.spdf.shapefile")
use.dat<-use.dat.clean@data
fish.dat<-use.dat[use.dat$ActvtyT=='Fishing Boat',]
all.fish.dat<-use.dat[use.dat$ActvtyT=='Fishing Boat'| use.dat$ActvtyT=='Shore Fishing',]

####################################
#Summary Statistics Fishing Data -BOATS
####################################

#number of surveys conducted
a<-ddply(use.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1))
a
a<-a[,1:4]
a$INT.reps<-remove.digits(a$INT)
a<-a[order(a$INT.reps),]
a
survey.freq<-ddply(a, . (TrnsctT, DayType, Period ), summarise, Freq = length(INT.reps))
survey.freq

#mean fishing boats per survey
b<-ddply(fish.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1), Rods=sum(Rods))
b

#work out transects where no fisherman found
no.fish.INT<-outersect(a$INT, b$INT)

no.fish.freq<-rep(0.00000001, length(no.fish.INT))
no.fish.rods<-rep(0,00000001, length(no.fish.INT))
no.fish.df<-data.frame(INT=no.fish.INT, Freq=no.fish.freq, Rods=no.fish.rods)

fish.means.df<-data.frame(INT=b$INT,Freq=b$Freq, Rods=b$Rods)

##Created a dataframe all transects, not jsut those with fishing in them. This will allow us to take the means properly.
fish.means.df<-rbind(fish.means.df,no.fish.df)
fish.means.df

##Create the rest of the dataframe using custom functions

period<-grep.mult.choice(fish.means.df$INT,'Aft','Mid','Morn')
daytype<-grep.vectors(fish.means.df$INT, 'we','wk')
transect<-gsub("(we)|(wk)|(Mid)|(Morn)|(Aft)|[[:digit:]]", "", fish.means.df$INT)

fish.means<-data.frame(Transect=transect,DayType=daytype,Period=period,INT=fish.means.df$INT,Freq=fish.means.df$Freq, Rods=fish.means.df$Rods)

#####################
#MEAN FISHING PER TRANSECTS
######################

fish.mn<-ddply(fish.means, .(Transect, DayType,Period), summarise, mean=mean(Freq), sd=sd(Freq),SE=stdErr(Freq), Rods=mean(Rods))
fish.mn


##############
#SHORE BASED FISHING
#############

shore.dat<-use.dat[use.dat$ActvtyT=='Shore Fishing',]

# fishing shore per survey
shore<-ddply(shore.dat, .(Period, DayType, TrnsctT, INT), summarise, Freq = length(date_1), Rods=sum(Rods))

no.shore.INT<-outersect(a$INT, shore$INT)
no.shore.freq<-rep(0.00000001, length(no.shore.INT))
no.shore.rods<-rep(0.0000001, length(no.shore.INT))
no.shore.df<-data.frame(INT=no.shore.INT, Freq=no.shore.freq, Rods=no.shore.rods)
shore.means.df<-data.frame(INT=shore$INT,Freq=shore$Freq, Rods=shore$Rods)

##Created a dataframe all transects, not jsut those with shore fishing in them. This will allow us to take the means properly.
shore.means.df<-rbind(shore.means.df,no.shore.df)

##Create the rest of the dataframe using custom functions

shore.period<-grep.mult.choice(shore.means.df$INT,'Aft','Mid','Morn')
shore.daytype<-grep.vectors(shore.means.df$INT, 'we','wk')
shore.transect<-gsub("(we)|(wk)|(Mid)|(Morn)|(Aft)|[[:digit:]]", "", shore.means.df$INT)
shore.means<-data.frame(Transect=shore.transect,DayType=shore.daytype,Period=shore.period,INT=shore.means.df$INT,Freq=shore.means.df$Freq, Rods=shore.means.df$Rods)

shore.mn<-ddply(shore.means, .(Transect, DayType,Period), summarise, mean=mean(Freq), sd=sd(Freq), SE=stdErr(Freq))
shore.mn

######################
#TOTAL FISHING (SHORE AND BOAT)
#####################

fish.tot<-merge(fish.means, shore.means, by='INT', all.x=T,all.y=T, suffixes=c('.boat','.shore'))
tot<-fish.tot
total.fishing<-data.frame(Transect=tot$Transect.boat, DayType=tot$DayType.boat, Period=tot$Period.boat, Fishing=tot$Freq.boat+tot$Freq.shore, Rods=as.integer(round(tot$Rods.boat+tot$Rods.shore,2)))
total.fishing<-total.fishing[total.fishing$Period!='No Match',]

total.fishing$Transect<-revalue(total.fishing$Transect, c(ChowderBay='Chowder Bay',DarlingHarbour='Darling Harbour',LaneCove='Lane Cove',Inner = "Inner Middle", Innerdle = "Inner Middle", 
                                                          Outerdle="Outer Harbour", MilsonsPoint='Milsons Point',NeutralBay='Neutral Bay', RoseBay='Rose Bay',SouthHead='South Head'))
total.fishing$DayType<-revalue(total.fishing$DayType, c(we='Weekday',wk='Weekend'))
total.fishing$Period<-revalue(total.fishing$Period, c(Aft='Afternoon',Mid='Midday',Morn='Morning'))

total.mn<-ddply(total.fishing, .(Transect, DayType,Period), summarise, mean=round(mean(Fishing),2), sd=round(sd(Fishing),2), SE=stdErr(Fishing))


total.mn

###total fishing

fish.tot$tot.fish<-round(fish.tot$Freq.boat+fish.tot$Freq.shore, 2)


wk.tot.fish<-ddply(fish.tot, .(DayType.boat, Period.boat), summarize, mean.tot=mean(tot.fish), sum.tot=sum(tot.fish), mean.boat=mean(Freq.boat), sum.boat=sum(Freq.boat),
                   mean.shore=mean(Freq.shore), sum.shore=sum(Freq.shore), means.rods.shore=mean(Rods.shore), sum.rod.shore=sum(Rods.shore), mean.rod.boat=mean(Rods.boat),
                   sum.rods.boat=sum(Rods.boat))

xtable(wk.tot.fish)

trans.tot.fish<-ddply(fish.tot, .(DayType.boat, Transect.boat), summarize, Mean=round(mean(tot.fish),0), Sum=sum(tot.fish), St.Deviation=round(sd(tot.fish),1))
print(xtable(trans.tot.fish,digits=0,include.rownames=FALSE))

#########################
#PLOTTING SUMMARY STATS
##########################

##Total fishing
Total.fishing.contact.rates<-function(){
tot.p<-ggplot(total.fishing, aes(x=Transect, y=Fishing, fill=DayType))
tot.p+geom_boxplot(size=0.2,linetype=element_line(size=0.1),outlier.size=1)+facet_grid(Period~.)+theme_minimal()+scale_fill_manual('Day Type', values=c('turquoise1','turquoise4') )+
  scale_y_continuous(limits=c(0,15), 'Fishing Contact Rates (per survey)')+theme(axis.text.x=element_text(angle=90, size=6), 
                                                                                 axis.text.y=element_text(size=6),
                                                                                 axis.ticks=element_line(size=0.1),
                                                                                 legend.key.size=unit(5, units='mm'), 
                                                                                 legend.text=element_text(size=5),
                                                                                 legend.title=element_text(size=7),
                                                                                 axis.title.y=element_text(size=6), 
                                                                                 axis.title.x=element_text(size=6),
                                                                                 strip.text=element_text(size=7),
                                                                                 panel.grid.major=element_blank() ) 
}

pdf('Output/total_fishing_contant_rates.pdf', width=8, height=5)
Total.fishing.contact.rates()
dev.off()

#####
#number rods
###
Total.fishing.rods<-function(){
rods.p<-ggplot(total.fishing, aes(Rods, fill=DayType))
rods.p+geom_bar(binwidth=0.5,position='dodge')+facet_grid(Transect~Period)+scale_fill_manual('Day Type', values=c('turquoise1','turquoise4') )+theme_minimal()+theme(axis.text.x=element_text(angle=90, size=6), 
                                                                                 axis.text.y=element_text(size=5),
                                                                                 axis.ticks=element_line(size=0.1),
                                                                                 legend.key.size=unit(4, units='mm'), 
                                                                                 legend.text=element_text(size=5),
                                                                                 legend.title=element_text(size=5),
                                                                                 axis.title.y=element_text(size=6), 
                                                                                 axis.title.x=element_text(size=6),
                                                                                 strip.text.y=element_text(size=5, angle=0),
                                                                                 strip.text.x=element_text(size=6),
                                                                                 panel.grid.major=element_blank(),
                                                                                 legend.title=element_text(size=6))
}

pdf('Output/total_fishing_rods.pdf', width=5, height=8)
Total.fishing.rods()
dev.off()


#####
#INTENTSITY
####

use.points<-readOGR("Output","use.spdf.shapefile")
fish.points<-use.points[use.points$ActvtyT=='Fishing Boat',]

est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))


fish.points$INT_nonum<-as.factor(remove.digits(interaction(fish.points$DayType, fish.points$Period)))
fish.points$INT_nonum<-revalue(fish.points$INT_nonum, c(we.Aft='Weekday (1400-1800)', we.Mid='Weekday (1100-1400)', we.Morn='Weekday (0700-1100)', wk.Aft='Weekend (1400-1800)',
                                                        wk.Mid='Weekend (1100-1400)',wk.Morn='Weekend (0700-1100)'))
fish.points<-fish.points[order(fish.points$INT_nonum),]
fish.levels<-levels(fish.points$INT_nonum)

est<-as(est, "SpatialPolygons")
east.1<-as(est, "owin")

boat.fishing.intensity<-function(){
par(mfrow=c(2,3), mar=c(0,0,0,2), omi=c(0,0,0,0))
for(i in 1:length(levels(fish.points$INT_nonum))){
  rep<-levels(fish.points$INT_nonum)[i]
  subset<-subset(fish.points, fish.points$INT_nonum==paste(rep))
  x<- ppp(x=subset$coords.x1, y=subset$coords.x2, window=east.1)
  plot(density(x, sigma=200) ,main='' ,frame.plot=F, ribwid=0.01,useRaster=FALSE, cex.main=1, rib.args=list(cex.axis=0.1), ribbon=F)
  mtext(paste(fish.levels[i]), 3, line=-4,cex=0.4)
  plot(est,add=T)
}
}


pdf('Output/boat_fishing_intensity.pdf', width=8, height=5)
boat.fishing.intensity()
  dev.off()
