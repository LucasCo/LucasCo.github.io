#########
#Distance to boat ramp rasterisation
########

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


facilities<-facilities[facilities@data$TYPE=="Formed Surface Launching Ramp" |facilities@data$TYPE=="Launching Ramp Parking & Facilities" , ]
syd_facilities<-facilities[!is.na(over(facilities, as(est_whole, "SpatialPolygons"))),]


#######
#rasterise facilities, create distance raster
#######

fac_distance_rast<-distance.rast.points(resolution=50, points=syd_facilities,polygon.mask=est)


######
#Plotting
#######
colors <- brewer.pal(9, "BuPu")
pal <- colorRampPalette(colors)

plot(fac_distance_rast,col=pal(30),main="Distance to Boat Launching Facility")
plot(est,add=T)


rm(use_df,colors,est,est_whole,facilities,use_spdf)
