####
#Read in the data
#####
est<-readOGR("Data","SH_est_poly_clipped")
est<-spTransform(est, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
use_spdf<-readOGR("Output","SH_census_spdf")
use_df<-use_spdf@data

i <- sapply(use_df, is.factor)###convert factors to characters
use_df[i] <- lapply(use_df[i], as.character)
rm(i)
######
#Create new entries for the value of 'fleet size'
#####
use_df$Flet_sz<-character.to.num.NA.to.one(use_df$Flet_sz) #convert fleet size to numeric and NA's to 0
use_df$Flet_sz[use_df$Flet_sz==0]<-1 ##all zeros to One

use_df.expanded <- use_df[rep(row.names(use_df), use_df$Flet_sz), ] #create a new df for row X fleet size
use_df<-use_df.expanded
rm(use_df.expanded)
use_df$INT<-as.character(interaction(use_df$Season,use_df$DyTrns_, use_df$date_1))