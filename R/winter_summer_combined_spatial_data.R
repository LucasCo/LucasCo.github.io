##############
#Combining, Cleaning, and pre-processing of Summer and Winter Census
#8th April 2014
##############

library(rgdal)
library(stringr)
library(plyr)
library(data.table)

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

write.table(levels(summer_data$Target), "Data/summer_target_names.csv", sep=",")###To create a new NewLookupTable in excel. 

####use new function created by Daniel Falstaff at Mac Uni. Found here "https://gist.github.com/dfalster/5589956". Uses lookup table to replace 
#values, in the same way as plyr
allowedVars<-c("Activity")
summer_data_cleaned<-addNewData("Data/summer_target_lookuptable.csv", summer_data, allowedVars)
summer_data_cleaned$Season<-"Summer"

##clean the area variable 
write.table(levels(summer_data_cleaned$Area), "Data/summer_area_names.csv", sep=",")
allowedVars<-c("Transect")
summer_data_cleaned<-addNewData("Data/summer_area_lookuptable.csv", summer_data_cleaned, allowedVars)
summer_data_cleaned$Coordint<-summer_data_cleaned$coordint

###convert and create proper time stamps
sum_dat<-summer_data_cleaned
sum_dat$time_syd<-gps.time(sum_dat$time)  ###custum function to change etrex20 GPS times into Sydney Time
sum_dat$date_1<-gps.date(sum_dat$time) ###custom function to get the date in a posix time stamp from an etrex garmin

###get target lats and longs- function returns a two column matrix of new lat and long. access like a list
newLatLong<-target.conversion(sum_dat$lat, sum_dat$lon, sum_dat$Bearing, sum_dat$Distance)
sum_dat$TargetLat<-newLatLong[[1]]
sum_dat$Targetlon<-newLatLong[[2]]

sum_dat$DayTrans.int<-interaction(sum_dat$DayType,sum_dat$Transect,sum_dat$Period)

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
SH_census_dat<-rbind.fill(winter_dat, sum_dat)
head(SH_census_dat)

tail(SH_census_dat)

