#######
#Distance to public wharf or jetty
#######

rm(list=ls())

library(sp)
library(rgdal)
library(RColorBrewer)


source("R/functions.R")
source("R/data_read_in_est_and_use.R")

facilities<-readOGR("Data","BoatingFacility")
facilities<-spTransform(facilities,CRS(proj4string(est)))
est_whole<-readOGR("Data","SH_est_poly")
est_whole<-spTransform(est_whole, CRS(proj4string(est)))


syd_facilities<-facilities[!is.na(over(facilities, as(est_whole, "SpatialPolygons"))),]
wharfs<-syd_facilities[syd_facilities@data$TYPE=="Public Wharf Jetty or Landing" , ]


#######
#rasterise facilities 
#######
wharf_distance_rast<-distance.rast.points(50,wharfs,est)

######
#Plotting
#######
colors <- brewer.pal(9, "BuPu")
pal <- colorRampPalette(colors)

plot(wharf_distance_rast,col=pal(30),main="Distance to Public Wharf or Jetty")
plot(est,add=T)

rm(use_df,colors, est, est_whole, facilities,use_spdf, syd_facilities)
