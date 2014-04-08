###Preliminary data preperation for Harbour Use Survey

library(rgdal)
library(raster)
library(stringr)
library(xtable)
library(ggplot2)
library(plyr)
library(rasterVis)
library(colorspace)
library(SDMTools)

rm(list=ls())
source("R/functions.R")


#############################################
#reading in the data and cleaning it. Converting to spdf
##############################################

##read in the data
use.dat<-read.csv("Data/Harbour_Survey_combined_data_1.csv",header=T)
est<-readOGR("Data","SH_est_poly_clipped")

#need to convert GPS time stamp to POSIX then creat factor of Time Period (Morn, Mid, Aft)
use.dat$time_syd<-as.POSIXlt(use.dat$time_syd,format='%Y-%m-%d %H:%M:%S')
use.dat$hour<-use.dat$time_syd$hour #create new hour factor

#break the $hour in morn, mid, aft 
use.dat$Period<-cut(use.dat$hour, breaks=c(0,11,14,23),
                    labels=c("Morn","Mid","Aft"))

#need to convert back to POSIXct for ddply to work
use.dat$time_syd<-as.POSIXct(use.dat$time_syd,format='%Y-%m-%d %H:%M:%S')

use.dat$INT<-remove.punc(interaction(use.dat$DayType, use.dat$Transect, use.dat$Period, use.dat$date_1))
use.dat<-use.dat[order(use.dat$INT),]
head(use.dat)
##create a SpatialPointsDataFrame

#create coordinates
coords=cbind(use.dat$Targetlon, use.dat$TargetLat)
sp = SpatialPoints(coords)

#add data to creat SpatialPointsDataFrame sp object
use.spdf<-SpatialPointsDataFrame(coords,use.dat)
proj4string(use.spdf)<-"+proj=longlat"

#####################################
#read other useful data in and clean 'use' data
####################################
##read other data in
rec.df<-read.csv("Data/Harbour_Survey_combined_data_1.csv", header=T) ##complete dataset on all observations
est<-readOGR("Data","SH_est_poly_clipped")
use.spdf<-spTransform(use.spdf,CRS(proj4string(est)))

##only points with coords (data cleaning) + adding appropriate projections
use.spdf<-points.poly(est, use.spdf)

#both est and fish in appropriate projections
use.spdf<-spTransform(use.spdf,CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))

##########################
#write use.spdf to shapefile
writeOGR(use.spdf,'Output','use.spdf.shapefile','ESRI Shapefile')
###########################


