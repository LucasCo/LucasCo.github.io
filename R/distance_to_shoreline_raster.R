#######
#Distance to shoreline
#######

rm(list=ls())

library(sp)
library(rgdal)
library(RColorBrewer)
library(raster)
library(spatstat)
library(maptools)
library(RColorBrewer)


source("R/functions.R")
source("R/data_read_in_est_and_use.R")

####
#Need to read in est using ReadShapeSpatial
###
est.shape<-readShapeSpatial("Data/SH_est_poly_clipped.shp", proj4string=CRS("+proj=longlat +ellps=GRS80"))
est.1<-spTransform(est.shape, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
est.sldf <- as( est.1 , "SpatialLinesDataFrame")



####
#Rasterize the shoreline
####

#create a blank raster
rast<-blank.raster(est.sldf,10)

#rasterise
shore_rast<-rasterize(est.sldf,rast)
distance<-distance(shore_rast)

shore_distance<-mask(distance, est)
plot(shore_distance)
