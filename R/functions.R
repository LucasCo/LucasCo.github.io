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





