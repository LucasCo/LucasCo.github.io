##############
#Combining, Cleaning, and pre-processing of Summer and Winter Census
#8th April 2014
##############

library(rgdal)
library(stringr)
library(plyr)
library(data.table)
library(sp)
library(spatstat)
library(maptools)

rm(list=ls())
source("R/functions.R")


#############################################
#reading in the data and cleaning it. Converting to spdf
##############################################

winter_dat<-read.csv("Data/Harbour_Survey_combined_data_1.csv",header=T, stringsAsFactors=FALSE) ###already combined
winter_dat$Season<-"Winter"
summer_dat<-read.csv("Data/SH_survey_summer.csv", header=T, stringsAsFactors=FALSE) ###is combined in this script
est<-readOGR("Data","SH_est_poly_clipped") ###is clipped and cleaned in other script

###############
#combining, cleaning, and preprocessing of Summer Census
###############

####combine data
downloads.lst <- lapply(paste("Data/SH_census_summer_download_",1:4,".csv",sep=''), read.csv, header=TRUE, sep=',' )
names(downloads.lst) <- paste("summer_download_", 1:4, sep='') 

#####make a new variable in each dataset that is download.name for later merging with waypoints data
download.time<-c(1,2,3,4)
for (i in 1:4){
  downloads.lst[[i]]$download<-download.time[i]
  downloads.lst[[i]]$coordint<-interaction(downloads.lst[[i]]$download, downloads.lst[[i]]$name, sep=".")
}
summer_coords<-rbind.fill(downloads.lst)

####combining with the waypoint data
summer_dat$coordint<-interaction(summer_dat$GPS.Download, summer_dat$Waypoint, sep=".") #make a coordint variable in the waypoint data to use as an index for merging
summer_data<-merge(summer_dat,summer_coords,by="coordint")

####use new function created by Daniel Falstaff at Mac Uni. Found here "https://gist.github.com/dfalster/5589956". Uses lookup table to replace 
write.table(levels(summer_data$Target), "Data/summer_target_names.csv", sep=",")###To create a new NewLookupTable in excel. 
#values, in the same way as plyr
allowedVars<-c("Activity", "Day","DayPeriod", "Transect")
summer_data_cleaned<-addNewData("Data/harbour_use_lookuptable.csv", summer_data, allowedVars)
summer_data_cleaned$Season<-"Summer"


head(winter_dat)
head(summer_data_cleaned)
outersect(names(winter_dat),names(summer_data_cleaned))
colnames(winter_dat)[which(names(winter_dat) == "Coordint")] <- "coordint"


###convert and create proper time stamps
sum_dat<-summer_data_cleaned
sum_dat$time_syd<-gps.time(sum_dat$time)  ###custum function to change etrex20 GPS times into Sydney Time
sum_dat$date_1<-gps.date(sum_dat$time) ###custom function to get the date in a posix time stamp from an etrex garmin

###get target lats and longs- function returns a two column matrix of new lat and long. access like a list
newLatLong<-target.conversion(sum_dat$lat, sum_dat$lon, sum_dat$Bearing, sum_dat$Distance)
sum_dat$TargetLat<-newLatLong[[1]]
sum_dat$Targetlon<-newLatLong[[2]]


##reduce summer_dat to have same columns as winter_dat
drops<-outersect(colnames(sum_dat), colnames(winter_dat))
sum_dat<-sum_dat[,!(names(sum_dat) %in% drops)]

sum_dat$DayType[sum_dat$DayType=="Weekend"]<-"wk"
sum_dat$DayType[sum_dat$DayType=="Week"]<-"we"
sum_dat$Period[sum_dat$Period=="Morning"]<-"Morn"
sum_dat$Period[sum_dat$Period=="Midday"]<-"Mid"
sum_dat$Period[sum_dat$Period=="Afternoon"]<-"Aft"

#####################
####Joing the two surveys together into one dataset
##################
SH_census_dat<-rbind(winter_dat, sum_dat)

###############
#Clean up and standardise the variables accross the two seasons -i.e. add a new date_1 and a new DayTrans.int
###############
SH_census_dat$DayTrans.int<-interaction(SH_census_dat$DayType,SH_census_dat$Transect,SH_census_dat$Period)
SH_census_dat$date_1<-gps.date.syd(SH_census_dat$time_syd)

###redo the Period to standardize from the GPS times.
SH_census_dat$Period<-time.of.day(SH_census_dat$time_syd)

###create new Coordint to incorporate season
SH_census_dat$Coordint_season<-interaction(SH_census_dat$Season, SH_census_dat$Coordint, sep=".")


#################
#Create a SPDF from the Combined data
################

##list NA's and 0's in SH_census data
SH_census_dat<-SH_census_dat[!is.na(SH_census_dat$TargetLat),]
SH_census_dat<-SH_census_dat[SH_census_dat$TargetLat!=0,]

#create coords and make spdf
coords<-as.data.frame(cbind(SH_census_dat$Targetlon, SH_census_dat$TargetLat))
SH_census_spdf<-SpatialPointsDataFrame(coords=coords,data=SH_census_dat)

proj4string(SH_census_spdf)<-proj4string(est)

###########
#move fishing points just out of Estuary polygon onto the shoreline- fishing boats
##########

fish.spdf<-SH_census_spdf[SH_census_spdf@data$Activity=="Fishing Boat",]
fish.spdf.1<-outside.points.move(fish.spdf,"Data/SH_est_poly_clipped.shp")
SH_census_spdf_nofish<-SH_census_spdf[SH_census_spdf@data$Activity!="Fishing Boat",]
SH_census_spdf<-rbind(SH_census_spdf_nofish,fish.spdf.1)

shorefish.spdf<-SH_census_spdf[SH_census_spdf@data$Activity=="Shore Fishing",]
shorefish.spdf.1<-all.points.move(shorefish.spdf,"Data/SH_est_poly_clipped.shp")

