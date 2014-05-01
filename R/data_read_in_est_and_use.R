####
#Read in the data
#####
library(rgdal)
source("R/functions.R")

est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
use_spdf<-readOGR("Output","SH_census_spdf")
use_spdf<-spTransform(use_spdf, CRS(proj4string(est)))


i <- sapply(use_spdf@data, is.factor)###convert factors to characters
use_spdf@data[i] <- lapply(use_spdf@data[i], as.character)
rm(i)

names(use_spdf@data)[names(use_spdf@data)=="date_1"] <- "Date"



######
#Create new entries for the value of 'fleet size'
#####
use_df$Flet_sz<-character.to.num.NA.to.one(use_df$Flet_sz) #convert fleet size to numeric and NA's to 0
use_df$Flet_sz[use_df$Flet_sz==0]<-1 ##all zeros to One

use_df.expanded <- use_df[rep(row.names(use_df), use_df$Flet_sz), ] #create a new df for row X fleet size
use_df<-use_df.expanded
rm(use_df.expanded)
use_df$INT<-as.character(interaction(use_df$Season,use_df$DyTrns_, use_df$date_1))

###convert date to proper date format
use_spdf$date_1<-as.Date(use_spdf$date_1,format='%Y-%m-%d')
use_df$date_1<-as.Date(use_df$date_1,format='%Y-%m-%d')


#######
#RAINFALL DATA
#######
#From BOM.

#read in rainfall dat
rain<-read.csv("Data/rainfall_data.csv", header=T, stringsAsFactors=FALSE)
rain<-rain[,3:6]
colnames(rain)[4]<-"Rainfall"

#subset to year 2013:2013
rain<-rain[rain$Year>=2013,]

rain$Date<-as.character(interaction(rain$Day,rain$Month,rain$Year,sep=":"))
rain$Date<-as.Date(rain$Date,format='%d:%m:%Y')

######MERGE rainfall and use_spdf
use_spdf@data<-merge(use_spdf@data,rain)


#####
#TIDE DATA
#####

