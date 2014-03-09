###Preliminary data preperation for Harbour Use Survey


library(plyr)
library(rgdal)

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

#number of surveys conducted
a<-ddply(use.dat, .(Period, DayType, Transect, date_1), summarise, Freq = length(date_1))
survey.freq<-ddply(a, . (Transect, DayType, Period ), summarise, Freq = length(date_1))

##create a SpatialPointsDataFrame- only boat fishing

fish.dat<-use.dat[use.dat$ActivityType=='Fishing Boat',]
coords=cbind(fish.dat$Targetlon, fish.dat$TargetLat)
sp = SpatialPoints(coords)
fish.spdf<-SpatialPointsDataFrame(coords,fish.dat)

proj4string(fish.spdf)<-proj4string(est) ##set CRS to sydney appropriate
fish.spdf<-points.poly(est,fish.spdf) ##take only points found in Sydney Harbour


writeOGR(fish.spdf, "Data", "fish.spdf", "ESRI Shapefile")
