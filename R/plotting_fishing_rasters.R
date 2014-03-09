#PLOTTTING RAW FISH DATA RASTERS

#read in data

whole.est.means.stack<-raster("Output/means_raster_winter.grd")
est<-readOGR("Data","SH_est_poly_clipped")

#####
#SET EXTENTS
#####
chowder.extent<-extent(338000, 342000, 6252000, 6256000)
manly.extent<-extent(338590.1, 341953.4, 6256429, 6259100)
parra.extent<-extent(326472.3,331912.9,6252472,6255093)
neutral.extent<-extent(334484.8,337600.9,6253066,6254500)

rstack<-whole.est.means.stack
breaks<-seq(0,10,2)
col<-colorRampPalette(c("white", "darkturquoise","darkviolet","deeppink4"))( 5)


raw.plot.manly<-function(){  
  par(mfrow=c(2,3), mar=c(1,1,3,4))  
  sub.crop<-crop(rstack,manly.extent)
  
  #plot 1    
  plot(sub.crop,"Weekday.0700.1100", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=F)
  title('Manly Area',cex.main=0.7, line=1, adj=0,sub='Weekday (0700-1800)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent) 
  
  #plot 2    
  plot(sub.crop,"Weekday.1100.1400", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=F)
  title('',cex.main=0.7, line=1, adj=0,sub='Weekday (1100-1400)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent)
  
  #plot 3
  plot(sub.crop,"Weekday.1400.1800", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=T)
  title('',cex.main=0.7, line=1, adj=0,sub='Weekday (1400-1800)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent)
  
  #plot 4
  plot(sub.crop,"Weekend.0700.1100", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=F)
  title('',cex.main=0.7, line=1, adj=0,sub='Weekend (0700-1100)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent)
  
  #plot 5
  plot(sub.crop,"Weekend.1100.1400", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=F)
  title('',cex.main=0.7, line=1, adj=0,sub='Weekend (1100-1400)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent)
  
  #plot 6
  plot(sub.crop,"Weekend.1400.1800", axes=F, main="", colNA='white', col=col, breaks=breaks,
       legend=T)
  title('',cex.main=0.7, line=1, adj=0,sub='Weekend (1400-1800)',
        cex.sub=0.8, line.sub=-1)  
  plot(est,lwd=1,add=T, extent=extent)
}

to.pdf(raw.plot.manly(), 'Output/manly_test.pdf', width=8, height=5)