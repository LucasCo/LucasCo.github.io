
###############################
#BOAT FISHING 
##############################
rm(list=ls())
source('R/functions.R')


###working with fish only and subsetting at the df stage, not spdf, as subsetting
#at the spdf stage is problematic

#est data for plotting
est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))

#use data from 'create_use_spatial_data.R'
use.dat.clean<-readOGR("Output","use.spdf.shapefile")
use.dat<-use.dat.clean@data
fish.dat<-use.dat[use.dat$ActvtyT=='Fishing Boat',]

##create a list of each subset of the dataframe corresponding to repliacte transcects
fish.list<-fish.dat$INT
fish.subsets<-lapply(fish.list, function(x) fish.dat[fish.dat$INT==x,])

#turn each element of the list to a spdf in a for-loop

fish.subsets.spdf.list<-list()
for(i in 1:length(fish.subsets)){
  rep<-fish.subsets[[i]]
  coords<-data.frame(x=rep$Targtln, y=rep$TargtLt)
  data<-rep
  sp<-SpatialPoints(coords)
  spdf<-SpatialPointsDataFrame(coords,data)
  proj4string(spdf)<-"+proj=longlat"
  spdf<-spTransform(spdf, CRS(proj4string(est)))
  fish.subsets.spdf.list<-lappend(fish.subsets.spdf.list, spdf)
}  

######################
#Rasterisation process- results in /Output/means_raster_winter
#######################

#rasterise each element of the list and add to a rasterstack called rep.stack
rast<-point.rast(rast, use.spdf, 50, p4s=proj4string(est))

rep.stack<-stack()
for(i in 1:length(fish.subsets.spdf.list)){
  rep<-fish.subsets.spdf.list[[i]]
  rast.1<-rasterize(rep, rast, 'Rods',sum)
  rep.stack<-stack(rep.stack,rast.1, quick=TRUE, RAT=TRUE)
  print(fish.subsets[[i]])
}
rep.stack.1<-subset(rep.stack,c(1:370))
names(rep.stack.1)<-fish.dat$INT

####Mean observed fishing boats per pixel cell useing stackapply

#create an index to use in stackapply
fish.reps<-as.numeric(as.factor(remove.digits(fish.list)))
fish.reps

means.stack<-stackApply(rep.stack.1, indices=fish.reps, fun=mean, na.rm=TRUE)
sd.stack<-stackApply(rep.stack.1, indices=fish.reps, fun=sd, na.rm=TRUE)

##name the raster layers by their interactions
means.names<-remove.digits(fish.dat$INT)
means.names<-unique(means.names)
names(means.stack)<-means.names

###combine the transects in estuary wide collations
means.names[58]<-"wkParramattaMorn"

#creating an index for stack apply
daytype<-grep.vectors(means.names,"we","wk")
period<-grep.mult.choice(means.names,"Morn","Mid","Aft")
estuary.index<-as.numeric(as.factor(interaction(daytype, period)))
index<-as.numeric(as.factor(paste(daytype,period,sep="")))

##means over whole estuary
whole.estuary.means.stack<-stackApply(means.stack, indices=index, fun=sum, na.rm=TRUE)
whole.est.means.stack<-mask(whole.estuary.means.stack,est)

est.names<-levels(as.factor(paste(daytype,period,sep="")))
est.names<-c('Weekday 1400-1800', 'Weekday 1100-1400','Weekday 0700-1100',
             'Weekend 1400-1800','Weekend 1100-1400','Weekend 0700-1100')
names(whole.est.means.stack)<-est.names

writeRaster(whole.est.means.stack, filename="Output/means_raster_winter.grd")

