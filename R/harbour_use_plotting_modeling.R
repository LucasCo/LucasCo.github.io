####PLOTTING FISHING DATA

library(rgdal)
library(maptools)
library(ggplot2)
library(ggmap)
library(rgdal)
library(plyr)
library(maps)
library(maptools)
library(sp)
library(spatstat)
library(splancs)

setwd("~/Dropbox/SH Risk Assessment/Harbour_use_survey/Harbour Use Survey Data")

surv<-read.csv("Harbour_Survey_combined_data_1.csv",header=T)
head(surv)

# points from data
coords <- cbind(surv$Targetlon, surv$TargetLat)
coords<-coords[surv$Targetlon !=0 & surv$TargetLat!=0,]
surv.sp <- SpatialPoints(coords)
plot(coords, pch=16, cex=0.5)

#data from data
colnames(surv)
surv.data<-surv[surv$Targetlon !=0 & surv$TargetLat!=0,c(2,4,5,6,10,11,13,14,15,19)]
head(surv.data)

# make spatial data frame
surv.spdf <- SpatialPointsDataFrame(coords, surv.data)
summary(surv.spdf)
proj4string(surv.spdf)<-"+proj=longlat +ellps=GRS80 +no_defs"
setwd("~/Dropbox/SH Risk Assessment/Harbour_use_survey/Harbour Use Survey Data")
writeOGR(surv.spdf, ".", "harbour_use_01", driver="ESRI Shapefile")
proj4string(surv.spdf)

###get estuary polygon
est <- readOGR (".", "SH_est_poly") 
proj4string(est)
plot(est)
plot(surv.spdf[],add=T,pch=16, cex=0.05)
