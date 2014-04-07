####Convert shore fishing points to points on a line

library(sp)
library(spatstat)
library(rgdal)
library(maptools)
library(raster)

####read in Estuary poly and convert to owin format for spatstat
est<-readShapeSpatial("Data/SH_est_poly_clipped.shp", proj4string=CRS("+proj=longlat +ellps=GRS80"))
est.1<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
est.owin<-as.owin(est.1)


###Read points in and convert to PPP format for spatstat
points <- readShapePoints("Data/SH_shore_fishing_cleaned.shp")
proj4string(points)<-"+proj=lcc +lat_1=-30.75 +lat_2=-35.75 +lat_0=-33.25 +lon_0=147 +x_0=9300000 +y_0=4500000 +ellps=GRS80 +units=m +no_defs"
points.1<-spTransform(points,CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
points.sp <- as(points.1, "SpatialPoints")
points.ppp <- as(points.sp, "ppp")



###convert est.owin to poly lines
est.psp<-as.psp(est.owin)


###move points onto closest shoreline from est.psp
##this function returns a list of 1) Xproj =the projected points onto the nearest psp line 2)mapXY the nearest segment to each point 3) d= the numeric vector of distances from each point 
#of x to the corresponding projected point and 4)tp=Numeric vector giving the scaled parametric coordinate 0 <= tp <= 1 of the position of the projected point along the segment. 

points.shore<-project2segment(points.ppp, est.psp)
points.shore.ppp<-as.ppp(points.shore$Xproj) ####just takes the projected points 
points.shore.ppp<-points.shore.ppp[est.owin]


#convert points.shore.ppp to a spdf for later rasterization.Also create a new shapefile that can be used in other gis if needed.
shore.x<-points.shore.ppp$x
shore.y<-points.shore.ppp$y
coords<-cbind(shore.x,shore.y)
data<-data.frame(Rods=points$Rods,Date=points$date_1,DateTime=points$tim_syd)
shore.points.spdf<-SpatialPointsDataFrame(coords=coords, data=data)
proj4string(shore.points.spdf)<-"+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"
writeOGR(shore.points.spdf,driver="ESRI Shapefile", layer="SH_shore_fishing_linear", dsn="Output")

###crete a raster of 5 x 5 metre cells for rasterising shoreline of sydney harbour
shore_rast<-raster()
extent(shore_rast)<-extent(points.sp)
res(shore_rast)<-10
proj4string(shore_rast)<-proj4string(points.sp)

est.sldf <- as( est.1 , "SpatialLinesDataFrame") ###rasterisation can't work on an owin or ppp

shore_raster<-rasterize(est.sldf, shore_rast)
writeRaster(shore_raster,"Output/SH_shoreline_raster_5m.grd")

plot(shorefish_raster)


