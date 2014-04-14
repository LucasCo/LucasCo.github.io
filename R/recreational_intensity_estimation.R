######
#Recreational Use Kernel Density Smoothing
#####

library(raster)
library(sp)
library(KernSmooth)
library(rgdal)

rm(list=ls())

source("R/functions.R")
source('R/data_read_in_est_and_use.R')

#########
#subset by activity to only include recreational activities
#########

rec_activities<-c("Kayak","Shore Recreation","Paddle Board","Wind Surfer","Dive","Dive boat","Swimmers","Dive Flag","Paddleboard","Shore People")

rec_df<-subset(use_df, use_df$Activty  %in% rec_activities)
rec_df$rasterIndex<-1 ##for later usein rasterisation (summation of this index, rather than counting, which returns a rasterbrick!)

rec_df$INT.2<-as.character(interaction(rec_df$Season,rec_df$DayType, rec_df$Period))

###list of subsetted daytype,transect,season data

rec_list<-rec_df$INT.2
rec_subsets<-lapply(unique(rec_list), function(x) rec_df[rec_df$INT.2==x,])
rm(rec_list)

coords_list<-list()
for(i in 1:length(rec_subsets)){
  rep<-rec_subsets[[i]]
  coords<-cbind(rep$Targtln, rep$TargtLt)
  coords_list<-lappend(coords_list,coords)
}
rm(i,coords,rep)

#####
#turn coords list into spatial points to convert to same CRS as est
####

coords_sp_list<-list()
for(i in 1:length(coords_list)){
  rep<-coords_list[[i]]
  coords.sp<-SpatialPoints(rep)
  proj4string(coords.sp)<-"+proj=longlat"
  coords.sp.trans<-spTransform(coords.sp,CRS(proj4string(est)))
  coords<-cbind(coords.sp.trans$coords.x1,coords.sp.trans$coords.x2)
  coords_sp_list<-lappend(coords_sp_list,coords)
}
rm(i,coords,rep)

######
#Create a raster to put smoothing kernel into
#####
rast<-raster()
projection(rast)<-proj4string(est)
extent(rast)<-extent(est)
res(rast)<-50
rast




#####
#Kernel Density Estimation using KernSmooth
#####
smooth_list<-list()
for(i in 1:length(coords_sp_list)){
  rep<-coords_sp_list[[i]]
  k_smooth<-bkde2D(rep, bandwidth=c(100,100), gridsize = c(237, 380))  
  smooth_list<-lappend(smooth_list,k_smooth)
}
rm(i,rep)




####
#Kernel Rasterisation
#####
kernel_rasters<-stack()

for(i in 1:length(smooth_list)){
  rep<-smooth_list[[i]]
  raster<-raster(list(x=rep$x1,y=rep$x2,z=rep$fhat))
  extent(raster)<-extent(est)
  projection(raster)<-projection(est)
  raster<-mask(raster,est)
  kernel_rasters<-stack(kernel_rasters,raster, quick=FALSE, RAT=TRUE)
}

names_list<-list()
for(i in 1:length(kernel_rasters@layers)){
  rep<-rec_subsets[[i]]
  name<-rep$INT.2[1]
  names_list<-append(names_list,name)
}
names(kernel_rasters)<-names_list
kernel_rasters_masked<-mask(kernel_rasters,est)

max<-max(sapply(smooth_list, function(x) max(x$fhat)))
max

brks <- seq(0,max,by=0.00000001)
plot(kernel_rasters,breaks=brks , legend=FALSE)



