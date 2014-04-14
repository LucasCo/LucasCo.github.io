######
#Recreational Use Raster
######

library(sp)
library(rgdal)
library(raster)

rm(list=ls())
source('R/functions.R')
source('R/data_read_in_est_and_use.R')


#########
#subset by activity to only include recreational activities
#########
rec_activities<-c("Kayak","Shore Recreation","Paddle Board","Wind Surfer","Dive","Dive boat","Swimmers","Dive Flag","Paddleboard","Shore People")
rec_df<-subset(use_df, use_df$Activty  %in% rec_activities)
rec_df$rasterIndex<-1 ##for later usein rasterisation (summation of this index, rather than counting, which returns a rasterbrick!)



##########
##Create a list comprising of individual transects 
#######
rec_list<-rec_df$INT
rec_subsets<-lapply(unique(rec_list), function(x) rec_df[rec_df$INT==x,])
rm(rec_list) 



#####
##Make each season/transect/time/daytype/replicate into a seperate raster baster on simple counts 
#####
rec_subsets_spdf_list<-list()

for(i in 1:length(rec_subsets)){
  rep<-rec_subsets[[i]]
  coords<-data.frame(x=rep$Targtln, y=rep$TargtLt)
  data<-rep
  sp<-SpatialPoints(coords)
  spdf<-SpatialPointsDataFrame(coords,data)
  proj4string(spdf)<-"+proj=longlat"
  spdf<-spTransform(spdf, CRS(proj4string(est)))
  rec_subsets_spdf_list<-lappend(rec_subsets_spdf_list, spdf)
  rm(coords,rep,data,sp,spdf,i)
}  


######################
#Rasterisation process- results to Output
#######################

#create new 50m raster
rast<-raster()
projection(rast)<-proj4string(rec_subsets_spdf_list[[1]])
extent(rast)<-extent(est)
res(rast)<-50

#create new rasters for each element of the subsets list above (i.e. each transect is rasterised individually with a 'count' per cell)
rep.stack<-stack()
for(i in 1:length(rec_subsets_spdf_list)){
  rep<-rec_subsets_spdf_list[[i]]
  rast.1<-rasterize(rep, rast,'rasterIndex', fun='sum', silent=FALSE)
  rep.stack<-stack(rep.stack,rast.1, quick=TRUE, RAT=TRUE)
}
rm(i,rast,rast.1,rast.2,rep,rep1,name)

##create the right names for the layers
names_list<-NULL
for(i in 1:length(rec_subsets_spdf_list)){
  name<-rec_subsets_spdf_list[[i]]@data$INT[1]
  names_list<-append(names_list,name)
}

names(rep.stack)<-names_list
rm(names_list)

writeRaster(rep.stack, filename="Output/recreational_use.grd")
