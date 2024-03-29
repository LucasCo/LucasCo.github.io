MODELING THE DISTRIBUTION OF RECREATIONAL FISHING IN COMPLEX URBANISED ESTUARIES
========================================================
HEDGE LJ, GARCIA PIZA M, ASTLES K, JOHNSTON EL
2014
------------------------

Supplemental Material
-------
**Libraries**

```r
library(sp)
library(rgdal)
library(RColorBrewer)
library(raster)
library(KernSmooth)
rm(list = ls())
```


Estuary polygon and 'Harbour Survey' data
--------
An estuary polygon was adapted from OEH mapping according their MER program. The estuary shoreline was supplied as an ESRI Shapefile with no projection (+proj=longlat). It was clipped to the extent of the harbour survey; namely Middle Harbour and the Upper Parramatta were removed. Note that an 'unclipped' version was also retiained for later derivation of distances to various features in the landscape.

Survey data was preprocessed to include the target latitude and longitude, number of people, rods, dogs and swimmers, Fleet size (a numeric vector deprecated, as it was used to mulitply row entries by the observed number of targets e.g. a fishing fleet of five vessels), target activity (e.g. motor boat, shore fishing, fishing boat),  transect name (e.g.Chowder Bay, Rose Bay), Sydney Time (calculated using the timestamp on the Garmin etrex 20 GPS used during the surveys), season (winter or summer) and a date (again, calculated from the etrex 20)




```r
source("R/functions.R")
source("R/data_read_in_est_and_use.R")
```

```
OGR data source with driver: ESRI Shapefile 
Source: "Data", layer: "SH_est_poly_clipped"
with 1 features and 1 fields
Feature type: wkbMultiPolygon with 2 dimensions
OGR data source with driver: ESRI Shapefile 
Source: "Output", layer: "SH_census_spdf"
with 10831 features and 18 fields
Feature type: wkbPoint with 2 dimensions
```



```r
str(use_df)
```

```
## 'data.frame':	12806 obs. of  19 variables:
##  $ Coordnt: chr  "1.1002" "1.1003" "1.1004" "1.1005" ...
##  $ DayType: chr  "wk" "wk" "wk" "wk" ...
##  $ Period : chr  "Morn" "Morn" "Morn" "Morn" ...
##  $ People : chr  "0" "0" "0" "0" ...
##  $ Rods   : chr  "0" "0" "0" "0" ...
##  $ Anchor : chr  NA NA NA NA ...
##  $ Dogs   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ swimmer: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Flet_sz: num  1 1 1 1 1 1 1 1 1 1 ...
##  $ Activty: chr  "Kayak" "Kayak" "Work Boat" "Motor Boat" ...
##  $ Transct: chr  "Neutral Bay" "Neutral Bay" "Neutral Bay" "Neutral Bay" ...
##  $ tim_syd: chr  "2013-08-25 09:59:00" "2013-08-25 10:00:00" "2013-08-25 10:00:00" "2013-08-25 10:05:00" ...
##  $ TargtLt: num  -33.8 -33.8 -33.8 -33.8 -33.8 ...
##  $ Targtln: num  151 151 151 151 151 ...
##  $ Season : chr  "Winter" "Winter" "Winter" "Winter" ...
##  $ DyTrns_: chr  "wk.Neutral Bay.Morn" "wk.Neutral Bay.Morn" "wk.Neutral Bay.Morn" "wk.Neutral Bay.Morn" ...
##  $ date_1 : chr  "2013-08-24" "2013-08-25" "2013-08-25" "2013-08-25" ...
##  $ Crdnt_s: chr  "Winter.1.1002" "Winter.1.1003" "Winter.1.1004" "Winter.1.1005" ...
##  $ INT    : chr  "Winter.wk.Neutral Bay.Morn.2013-08-24" "Winter.wk.Neutral Bay.Morn.2013-08-25" "Winter.wk.Neutral Bay.Morn.2013-08-25" "Winter.wk.Neutral Bay.Morn.2013-08-25" ...
```


Note that some points at this stage fall outside the Estuary polygon. These are shore fishermen. These points are dealt with later.

```r
use_spdf <- function() {
    plot(use_spdf, cex = 0.05, main = "All points in Sydney Harbour Survey 2013-2014")
    plot(est, add = T)
}
jpeg("README_images/use_spdf.jpg")
use_spdf()
```

```
## Error: unused argument (x)
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
use_spdf()
```

```
## Error: unused argument (x)
```


HABITAT RASTERS AND DISTANCE TO INTERTIDAL HABITAT
-----------

NSW DPI supplied several spatial datasets on the distribution of sub tidal and intertidal macrophytes, and the distribution of sub tidal rocky reef. This data is rasterised onto a 50m x 50m grid. 

Where data related to sub-tidal macrophytes or sub-tidal reef, rasterisation resulted in the percentage of each pixel covered by the the macrophyte. Where data pertained to intertidal macrophytes, rasters represent the distance (in metres) from a pixel to the nearest intertidal vegetation.


```r
###### Read in macrophyte data
macro <- readOGR("Data", "NSW_DPI_Estuarine_Macrophytes")

#### Create distance raster if intertidal (Mangroves and Saltmarsh), Percentage
#### coverage if Subtidal Macrophyte (Seagrass)

macro_stack <- stack()
macrophytes <- c("Seagrass", "Not Seagrass")
for (i in 1:length(macrophytes)) {
    macrophyte <- macrophytes[i]
    if (macrophyte == "Seagrass") {
        macro_poly <- macro[macro@data$HABITAT == "Seagrass", ]
        seagrass_rast <- pixel.cover(50, macro_poly, est)
        macro_stack <- stack(macro_stack, seagrass_rast)
    } else {
        macro_poly.1 <- macro[macro@data$HABITAT != "Seagrass", ]
        inter_macro_distance_rast <- distance.rast.points(resolution = 50, points = macro_poly.1, 
            polygon.mask = est)
        macro_stack <- stack(macro_stack, inter_macro_distance_rast)
    }
}
names(macro_stack) <- c("Percentage pixel covered by seagrass", "distance to intertidal vegetation")


##### WRITE TO FILE
unstack_macro_stack <- unstack(macro_stack)
outputnames <- paste("Output/", names(macro_stack), ".grd", sep = "")
for (i in seq_along(unstack_macro_stack)) {
    writeRaster(unstack_macro_stack[[i]], file = outputnames[i])
}
```






```r
plot(macro_stack)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r

```




RECREATIONAL USE (NON FISHING) DENSITY ESTIMATION
------
We used the data generated during the Sydney Harbour Survey to generate an intensity map using a kernel density estimator. The output is a `smoothed' raster of recreational  intensity. A bandwidth of 100 was used as this produced a result that was observed to be appropriate for the data. 

I first subset the use_df by all the recreational activities noted during the surveys. I use a vector of these activities and use %in% to subset by that vector.


```r
######### subset by activity to only include recreational activities

rec_activities <- c("Kayak", "Shore Recreation", "Paddle Board", "Wind Surfer", 
    "Dive", "Dive boat", "Swimmers", "Dive Flag", "Paddleboard", "Shore People")

rec_df <- subset(use_df, use_df$Activty %in% rec_activities)
rec_df$rasterIndex <- 1  ##for later usein rasterisation (summation of this index, rather than counting, which returns a rasterbrick!)

rec_df$INT.2 <- as.character(interaction(rec_df$Season, rec_df$DayType, rec_df$Period))
```


Using lappy I then create a list where each element of the list represents the targets observed over a weekend and weekday, over summer or winter in each Period (Morning-Afternoon, INT.2= Season, DayType, Period)


```r
### list of subsetted daytype,transect,season data

rec_list <- rec_df$INT.2
rec_subsets <- lapply(unique(rec_list), function(x) rec_df[rec_df$INT.2 == x, 
    ])
rm(rec_list)
```


Target coordinates are in LongLat (TargtLt and Targtln). These are converted to the appropriate UTM projection using a for loop to UTM. 

```r
coords_sp_list <- list()
for (i in 1:length(rec_subsets)) {
    rep <- rec_subsets[[i]]  #subset to each list element
    coords <- cbind(rep$Targtln, rep$TargtLt)  #create a coords df for conversion
    coords.sp <- SpatialPoints(coords)  # convert to spatial points (sp)
    proj4string(coords.sp) <- "+proj=longlat"  #assign CRS
    coords.sp.trans <- spTransform(coords.sp, CRS(proj4string(est)))  # convert the spatialpoints to UTM (the prj4string of est is appropriate UTM)
    coords <- cbind(coords.sp.trans$coords.x1, coords.sp.trans$coords.x2)  #convert back to dataframe
    coords_sp_list <- lappend(coords_sp_list, coords)  #make a new list of converted coords
}

```


A generic 50m x 50m resolution raster is created that covers whole estuary, not including Middle Harbour or Upper Parramatta River. It is created here as the raster allows us to calculate the gridsize (the x and y resolution) we need to the proceeding kernel estimation.

```r
###### Create a raster to put smoothing kernel into
rast <- raster()
projection(rast) <- proj4string(est)
extent(rast) <- extent(est)
res(rast) <- 50
```


A sperate smoothing estimate is generated for each period in Summer and Winter over the whole estuary for each day type. bkde2d generates a 3 element list of x and y coordinates, and the associated kernel esitmate at that point. The gridsize of 237 by 380 was chosed as this approximates the 50m x 50m raster that will be generated by this data (i.e. it is the same resolution, such that there is an estimate in each pixel of the raster generated later)


```r
##### Kernel Density Estimation using KernSmooth
smooth_list <- list()
for (i in 1:length(coords_sp_list)) {
    rep <- coords_sp_list[[i]]  #subset 
    k_smooth <- bkde2D(rep, bandwidth = c(100, 100), gridsize = c(237, 380))  # part of the kernsmooth package, bandwith of 100 was chosen as this was most appropriate.
    smooth_list <- lappend(smooth_list, k_smooth)  #output of bkde2d is a three element list of x and y coordinates and associate Kernel estimate.
}
rm(i, rep)

```


After kernel estimation (above), the estimates were rasterised elementwise into the 50m x 50m raster generated earlier. Note that extent, resolution and projection are all standardised to the generic raster in this loop.


```r
#### Kernel Rasterisation

kernel_rasters <- stack()
for (i in 1:length(smooth_list)) {
    rep <- smooth_list[[i]]
    raster <- raster(list(x = rep$x1, y = rep$x2, z = rep$fhat))
    extent(raster) <- extent(est)
    projection(raster) <- projection(est)
    raster <- mask(raster, est)
    kernel_rasters <- stack(kernel_rasters, raster, quick = FALSE, RAT = TRUE)
}
```


Note that the names of the rasters are un-informative and are replaced elementwise in a for-loop with the unique INT.2 value. These names, however, are also abbreviated and so replced manually.


```r
names_list <- list()
for (i in 1:length(kernel_rasters@layers)) {
    rep <- rec_subsets[[i]]
    name <- rep$INT.2[1]
    names_list <- append(names_list, name)
}
names(kernel_rasters) <- names_list
kernel_rasters_masked <- mask(kernel_rasters, est)

max <- max(sapply(smooth_list, function(x) max(x$fhat)))
brks <- seq(0, max, by = 1e-08)
names(kernel_rasters) <- c("Winter Weekend Morning", "Winter Weekend Midday", 
    "Winter Weekend Afternoon", "Winter Weekday Morning", "Winter Weekday Midday", 
    "Winter Weekday Afternoon", "Summer Weekend Midday", "Summer Weekend Morning", 
    "Summer Weekend Afernoon", "Summer Weekday Morning", "Summer Weekday Midday", 
    "Summer Weekday Afternoon")

```


The rasterstack is written to file in the native .grd format.

```r
# WRITE TO FILE
unstack_rasters <- unstack(kernel_rasters)
outputnames <- paste("Output/SH_Survey_Recreational_Use_Rasters/", names(kernel_rasters), 
    ".grd", sep = "")
for (i in seq_along(unstack_rasters)) {
    writeRaster(unstack_rasters[[i]], file = outputnames[i])
}
```



```r
##### To read back in
rasterfiles <- list.files("Output/SH_Survey_Recreational_Use_Rasters", "*.grd", 
    full.names = TRUE)
kernel_rasters <- stack(rasterfiles)
names(kernel_rasters) <- rasterfiles
```



```r

max <- max(values(max(kernel_rasters)), na.rm = T)
brks <- seq(0, max, by = 1e-08)
plot(kernel_rasters, breaks = brks, legend = FALSE)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 

```r

```
