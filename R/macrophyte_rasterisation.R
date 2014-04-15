#####
#Habitat Rasters
#####

rm(list=ls())

library(sp)
library(rgdal)
library(maptools)
library(raster)

source('R/functions.R')
source('R/data_read_in_est_and_use.R')

######
#Read in macrophyte data
#######
macro<-readOGR("Data","NSW_DPI_Estuarine_Macrophytes")

####Create distance raster if intertidal (Mangroves and Saltmarsh), Percentage coverage if Subtidal Macrophyte (Seagrass)

macro_stack<-stack()
macrophytes<-c("Seagrass", "Not Seagrass")
for(i in 1:length(macrophytes)){
  macrophyte<-macrophytes[i]
  if (macrophyte=="Seagrass"){
    macro_poly<-macro[macro@data$HABITAT=="Seagrass",]
    seagrass_rast<-pixel.cover(50,macro_poly,est)
    macro_stack<-stack(macro_stack,seagrass_rast)
  } else{
    macro_poly.1<-macro[macro@data$HABITAT!="Seagrass",]
    inter_macro_distance_rast<-distance.rast.points(resolution=50, points=macro_poly.1,polygon.mask=est)
    macro_stack<-stack(macro_stack, inter_macro_distance_rast)
  }
}
names(macro_stack)<-c("Percentage pixel covered by seagrass","distance to intertidal vegetation")

#####
#WRITE TO FILE
#####
unstack_macro_stack<-unstack(macro_stack)
outputnames <- paste("Output/",names(macro_stack), ".grd",sep="")
for(i in seq_along(unstack_macro_stack)){writeRaster(unstack_macro_stack[[i]], file=outputnames[i])}

