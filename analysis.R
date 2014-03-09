###Sydney Harbour Resource Use Survey
#LHedge began 5th March 2014

library(rgdal)
library(raster)
library(stringr)

rm(list=ls())
source("R/functions.R")

rec.df<-read.csv("Data/Harbour_Survey_combined_data_1.csv", header=T) ##complete dataset on all observations
fish.df<-readOGR("Data","Fishing_boats_Sh_clean",p4s= "+proj=longlat") ##from Arc; sanity check on fishing locations, removed bad points manually.
est<-readOGR("Data","SH_est_poly_clipped")
rep.stack<-raster()


###Number of fishing boats observed per transect/time/day/rep

totals<-ddply(fish.df, .(DayType, Period, Transect,date), summarise, freq=nrow)
means<-ddply(totals, .(Daytype, Period, Transect), summarise, mean=mean, sd=sd)



