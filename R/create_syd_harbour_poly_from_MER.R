###creating a single sydney harbour polygon
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
library(PBSmapping)
library(rgeos)

setwd("~/Dropbox/SHRP/DPI Scoping Study/GISData/Catchment Boundary GIS")

ogrInfo(".", "Sydney_harbour_estuaries_catchments_4") ##check the properties of the shapefile
est <- readOGR (".", "Sydney_harbour_estuaries_catchments_4") ##read in the Shapefile
summary(est)

levels(est$NAMETYPE)
est.parra <- est[est$NAMETYPE=="PARRAMATTA RIVER ESTUARY",]
est.pj<-est[est$NAMETYPE=="PORT JACKSON ESTUARY",]
est.lc<-est[est$NAMETYPE=="LANE COVE RIVER ESTUARY",]
est.mh<-est[est$NAMETYPE=="MIDDLE HARBOUR CREEK ESTUARY",]
est.com<-rbind(est.parra,est.pj,est.lc,est.mh)
plot(est.com)

dat<-"water"
est.poly<-gUnionCascaded(est.com)
est.spp <-SpatialPolygonsDataFrame(est.poly,data=as.data.frame(dat))
plot(est.spp)
summary(est.spp@data)
str(est.spp,2)

writeOGR(est.spp, ".", "SH_est_poly", driver="ESRI Shapefile")
