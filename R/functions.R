###Sydney Harbour Use Survey Functions
#Luke Hedge began 5th March 2014


##create a raster of set resolution to the extent of a series of points from a .shp file
point.rast<-function(name, points, res, p4s){
  name<-raster()
  extent(name)<-extent(points)
  res(name)<-res
  projection(name)<-p4s
  return(name)
}


##remove all puncutation and spaces in factors
collapse.strings<-function(x){
  x<-as.factor(gsub("[[:punct:]]", "", x))
  x<-as.factor(gsub(" ","",x))
}

##remove all punctuation and spaces

remove.punc<-function(x){
  x<-str_replace_all(x, "[^[:alnum:]]", "")
  x<-str_replace_all(x, "[[:punct:]]", "")
  x<-str_replace_all(x, " ", "")
}

remove.digits<-function(x){
  x<-str_replace_all(x,"[[:digit:]]", "" )
}

crop.and.mask<-function(rast, poly){
  rast.crop<-crop(rast, extent(poly))
  rast.mask<-mask(rast.crop, poly)
  return(rast.mask)
}

points.poly<-function(polygon,points){
  inside.poly <- !is.na(over(points, as(polygon, "SpatialPolygons")))
  point.inside<-points[inside.poly, ]
  return(point.inside)
}

raster.and.mask<-function(points, rast, z, fun, poly){
  rast.full<-rasterize(points, rast, z, fun)
  rast.crop<-crop(rast.full, extent(poly))
  rast.mask<-mask(rast.crop, poly)
  return(rast.mask)
}

##returns the elements of two vectors that do not overlap

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

grep.vectors<-function(x, pattern1, else1){
  GrepValue<-NULL
  for (i in 1:length(x)){
    if (grepl(pattern1, x[i],perl=TRUE)==TRUE){
      GrepValue<-append(GrepValue,pattern1)
    } else GrepValue<-append(GrepValue,else1)
  }
  return(GrepValue)
}

grep.mult.choice<-function(x, pattern1,pattern2,pattern3){
  GrepValue<-NULL
  for (i in 1:length(x)){
    if(grepl(pattern1, x[i],perl=TRUE)==TRUE){
      GrepValue<-append(GrepValue,pattern1)
    } else if(grepl(pattern2, x[i], perl=TRUE)==TRUE){
      GrepValue<-append(GrepValue,pattern2)
    } else if(grepl(pattern3, x[i], perl=TRUE)==TRUE) {
      GrepValue<-append(GrepValue,pattern3)
    } else {
      GrepValue<-append(GrepValue, 'No Match')
    }
  }
return(GrepValue)
}
    
 
      
to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}     

raw.dat.plot <- function(rasterstack,extent,time, location,sub) {
  col<-colorRampPalette(c("white", "darkturquoise","darkviolet","deeppink4"))( 100 )
  breaks<-seq(1,10,0.1)
  sub.crop<-crop(rasterstack,extent)
  plot(sub.crop,time, axes=F, main="", colNA='white', col=col, breaks=breaks)
  title(location,cex.main=0.7, line=1, adj=0,sub=sub,
        cex.sub=0.8, line.sub=-1)
  plot(est,lwd=1,add=T, extent=extent)  
}

stdErr=function(x) {sd(x)/ sqrt(length(x))}


##' Modifies 'data' by adding new values supplied in newDataFileName
##'
##' newDataFileName is expected to have columns
##' c(lookupVariable,lookupValue,newVariable,newValue,source)
##'
##' Within the column 'newVariable', replace values that
##' match 'lookupValue' within column 'lookupVariable' with the value
##' newValue'. If 'lookupVariable' is NA, then replace *all* elements
##' of 'newVariable' with the value 'newValue'.
##'
##' Note that lookupVariable can be the same as newVariable.
##'
##' @param newDataFileName name of lookup table
##' @param data existing data.frame
##' @param allowedVars vector of permissible variable names for newVariable
##' @return modified data.frame
addNewData <- function(newDataFileName, data, allowedVars){
  
  import <- readNewData(newDataFileName, allowedVars)
  
  if( !is.null(import)){
    for(i in seq_len(nrow(import))){ #Make replacements
      col.to <- import$newVariable[i]
      col.from <- import$lookupVariable[i]
      if(is.na(col.from)){ # apply to whole column
        data[col.to] <- import$newValue[i]
      } else { # apply to subset
        rows <- data[[col.from]] == import$lookupValue[i]
        data[rows,col.to] <- import$newValue[i]
      }
    }
  }
  data
}

##' Utility function to read/process newDataFileName for addNewData
##'
##' @param newDataFileName name of lookup table
##' @param allowedVars vector of permissible variable names for newVariable
##' @return data.frame with columns c(lookupVariable,lookupValue,newVariable,newValue,source)
readNewData <- function(newDataFileName, allowedVars){
  if( file.exists(newDataFileName)){
    import <- read.csv(newDataFileName, header=TRUE, stringsAsFactors=FALSE,
                       strip.white=TRUE)
    if( nrow(import)> 0 ){
      #Check columns names for import are right
      expectedColumns<- c("lookupVariable","lookupValue","newVariable","newValue")
      nameIsOK <- expectedColumns %in% names(import)
      if(any(!nameIsOK))
        stop("Incorrect name in lookup table for ",
             newDataFileName, "--> ", paste(expectedColumns[!nameIsOK],
                                            collapse=", "))
      #Check values of newVariable are in list of allowed variables
      import$lookupVariable[import$lookupVariable == ""] <- NA
      nameIsOK <- import$newVariable %in% allowedVars
      if(any(!nameIsOK))
        stop("Incorrect name(s) in newVariable column of ",
             newDataFileName, "--> ", paste(import$newVariable[!nameIsOK],
                                            collapse=", "))
    } else {
      import <- NULL
    }
  } else {
    import <- NULL
  }
  import
}

##create proper time stamps from garmin etrex20 output

gps.time<-function(timeVector){
  time.char<-as.character(timeVector)#convert gps time to character for conversion to timestamp proper
  timeP<-strptime(time.char,format='%Y-%m-%dT%H:%M:%S') #convert to a timestamp in right format
  pb.date<-as.POSIXct(timeP, tz="GMT") #convert to POSIX timestamp in GMT time
  time_syd<-format(pb.date, tz="Australia/Sydney")#convert to POSIX timestamp in Sydney Time
  return(time_syd)
}

####strip the date only from a POSIX timestamp, such as created above
gps.date<-function(timeVector){
  time.char<-as.character(timeVector)#convert gps time to character for conversion to timestamp proper
  timeP<-strptime(time.char,format='%Y-%m-%dT%H:%M:%S') #convert to a timestamp in right format
  pb.date<-as.POSIXct(timeP, tz="GMT") #convert to POSIX timestamp in GMT time
  time_syd<-format(pb.date, tz="Australia/Sydney")#convert to POSIX timestamp in Sydney Time
  date<-as.POSIXct(time_syd, format="%Y-%m-%d %H:%M:%S")#convert time to POSIXCT timestamp
  date_1<-as.Date(date,format='%d-%m-%y')
  date_1<-as.factor(date_1)
  return(date_1)
}


gps.date.syd<-function(timeSyd){
  date<-as.POSIXct(timeSyd, format="%Y-%m-%d %H:%M:%S")#convert time to POSIXCT timestamp
  date_1<-as.Date(date,format='%d-%m-%y')
  date_1<-as.factor(date_1)
  return(date_1)
}


#########
#CONvert distance and bearing to new locations
########

target.conversion<-function(currentLat, currentLong, Bearing, DistanceMetres){
  latR<-currentLat*pi/180 #convert everything to radians
  lonR<-currentLong*pi/180 #convert everything to radians
  BearR<-Bearing*pi/180 #convert everything to radians
  radiusEarth<-6371 #convert everything to radians
  distKM<-DistanceMetres/1000 #convert everything to radians
  distR<-distKM/radiusEarth #convert everything to radians
  lat2<-asin(sin(latR)*cos(distR) + cos(latR)*sin(distR)*cos(BearR)) #convert to new point
  lon2<-lonR + atan2(sin(BearR)*sin(distR)*cos(latR), cos(distR)-sin(latR)*sin(latR)) #convert to new point
  TargetLat<-lat2 * 180/pi
  TargetLon<-lon2 * 180/pi
  return(list(TargetLat=TargetLat, TargetLon=TargetLon))
}


########
####create Time of Day vector from a POSIX time stamp
######
##takes a sydney time POSIX stamp and converts to midday, morning, afternoon

time.of.day<-function(time){
  time_syd<-as.POSIXlt(time,format='%Y-%m-%d %H:%M:%S')
  hour<-time_syd$hour
  Period<-cut(hour, breaks=c(0,11,14,23),labels=c("Morn","Mid","Aft")) #break the $hour in morn, mid, aft
  return(Period)
}
  

##checking for non-finite elements
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}


##removing na's and zeros
remove.na.0s<-function(data, variable1){ 
  dat<-data
  dat<-dat[!is.na(variable1),]
  dat<-dat[variable1!=0,]
  return(dat)
}


###convert spdf data, subset to those points outside a polygon and concert to ppp and project onto lines of polygon. Used for moving points just outside the
#estuary polygon to the shoreline. Points and poly's must be in same CRS

outside.points.move<-function(data, polygon.file){
  est.1<-readShapePoly(paste(polygon.file))
  proj4string(est.1)<-proj4string(data) 
  outside.est <- data[is.na(over(data, as(est.1, "SpatialPolygons"))),] #subset the data to only those points outside the estuary polygon 
  est.owin<-as(as(est.1, "SpatialPolygons"), "owin")
  points.sp <- as(outside.est, "SpatialPoints")
  points.ppp <- as(points.sp, "ppp")
  est.psp<-as.psp(est.owin)
  points.shore<-project2segment(points.ppp, est.psp)
  points.shore.ppp<-as.ppp(points.shore$Xproj) ####just takes the projected points
  shore.x<-points.shore.ppp$x
  shore.y<-points.shore.ppp$y
  coords<-cbind(shore.x,shore.y)
  outside.est@coords<-coords
  data.notoutside<-data[!is.na(over(data, as(est.1, "SpatialPolygons"))),]
  new.data<-rbind(data.notoutside,outside.est)
  return(new.data)
}


###move all points to the shoreline e.g for shore fishing

all.points.move<-function(data,polygon.file){
  data<-data
  est.1<-readShapePoly(paste(polygon.file))
  proj4string(est.1)<-proj4string(data) 
  est.owin<-as(as(est.1, "SpatialPolygons"), "owin")
  points.sp <- as(data, "SpatialPoints")
  points.ppp <- as(points.sp, "ppp")
  est.psp<-as.psp(est.owin)
  points.shore<-project2segment(points.ppp, est.psp)
  shore.x<-points.shore.ppp$x
  shore.y<-points.shore.ppp$y
  coords<-cbind(shore.x,shore.y)
  data@coords<-coords
  return(data)
}

