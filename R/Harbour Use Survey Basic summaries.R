### Harbour Use Survey




###Combining waypoints with GPS coordinates

#read in data
down1<-read.csv('Harbour_survey_download_1.csv', header=T)
down2<-read.csv('Harbour_Survey_dataDownload2.csv', header=T)
down3<-read.csv('Harbour_Survey_dataDownload3.csv', header=T )
down4<-read.csv('harbour_survey_datadownload_4_yellowGPS.csv', header=T )
down5<-read.csv('harbour_survey_datadownload_5_1.csv')
waypoints<-read.csv('Harbour_survey_waypointdetails_master_1.csv',header=T)

##create unique ID key for merging datasets by Download time
down1$Coordint<-interaction(down1$download,down1$name, sep=".")
down2$Coordint<-interaction(down2$download,down2$name, sep=".")
down3$Coordint<-interaction(down3$download,down3$name, sep=".")
down4$Coordint<-interaction(down4$download,down4$name, sep=".")
down5$Coordint<-interaction(down5$download,down5$name, sep=".")
waypoints$Coordint<-interaction(waypoints$download, waypoints$Waypoint, sep=".")
tail(waypoints)

#Join the GPS download datasets into one big dataset
coords<-rbind(down1,down2,down3,down4, down5)
write.csv(coords,file="Waypoint_coordinates_master.csv")
str(coords)



head(coords)
tail(coords)
str(coords)

##merge the two datasets by each ones unique ID ("Coordint")
dat<-merge(waypoints,coords,by="Coordint")
tail(dat)
head(dat)
length(dat$Coordint)
summary(dat)

###relevel target code into correct notation (spelling errors etc)
levels(dat$TargetCode)
library(plyr)
dat$Activity<-revalue(dat$TargetCode, c("db"="Dive boat", "Dive"="Dive","dive flag"="Dive", "diveboat"="Dive boat",
                                        "dive"="Divers",                                
                          "df"="Dive Flag",              
                          "f"="Fishing Boat",
                          "fb"="Fishing Boat",
                          "fk"="Fishing Kayak",
                          "fn"="Fishing Boat",
                          "fs"="Fishing Boat",
                          "k"="Kayak",
                          "m"="Motor Boat",
                          "mb"="Motor Boat",
                          "n"="Motor Boat",
                          "navy" = "Navy",   
                          "p"   = "Paddle Board",       
                          "paddle"= "Paddle Board",     
                          "police"="Police",     
                          "r"="Kayak",          
                          "rm5"="RMS",
                          "rms" = "RMS",      
                          "row" ="Kayak",      
                          "s" ="Sail Boat",        
                          "sb"  ="Sail Boat",     
                          "sc"  ="Sail Boat",      
                          "sf"  = "Shore Fishing",      
                          "sfleet"  ="Sail Fleet",  
                          "snorkeller" = "Swimmer",
                          "sp"  ="Shore Recreation",       
                          "swim"="Swimmers",       
                          "t" ="Tourist Boat",
                          "tac" ="Taxi",      
                          "tax" ="Taxi",      
                          "taxi"="Taxi",      
                          "ts"  ="Toursit Jet Boat",      
                          "w"    = "Work Boat",      
                          "war" = "Warship",  
                          "wind"= "Wind Surfer",       
                          "windsurf"  = "Wind Surfer", 
                          "ws"  = "Wind Surfer",       
                          "wt"  = "Taxi"   ,    
                          "wtaxi"= "Taxi"))
levels(dat$Activity)
levels(dat$Activity)[1] <- NA
levels(dat$Activity)

#Change the transects to correct notation
levels(dat$Area)

dat$Transect<-revalue(dat$Area, c(               
  "chowder"  ="Chowder Bay",   
  "Chowder"   = "Chowder Bay",     
  "Darling"     = "Darling Harbour",   
  "darling harbour" = "Darling Harbour",
  "Darling Harbour" = "Darling Harbour",
  "Drommoyne"      = "Drummoyne",
  "Drum" = "Drummoyne",
  "drummoyne"    = "Drummoyne",
  "Drummoyne"   = "Drummoyne",
  "Glades"      = "Gladesville",
  "gladesville" = "Gladesville",
  "inner"        = "Inner Middle",
  "Inner"       = "Inner Middle", 
  "inner middle"  = "Inner Middle",
  "Inner Middle" = "Inner Middle",
  "Lane"   ="Lane Cove",
  "Lane "       = "Lane Cove",
  "Manly"   = "Manly",      
  "Middle Outer"  = "Outer Middle",
  "Milson"       = "Milsons Point",
  "milsons"       = "Milsons Point", 
  "Milsons"     = "Milsons Point", 
  "neutral" = "Neutral Bay",
  "Neutral"     = "Neutral Bay",
  "Neutral Bay"  = "Neutral Bay",
  "New"         ="Neutral Bay", 
  "outer"         ="Outer Middle",
  "Outer"     = "Outer Middle",    
  "Outer Middle"  ="Outer Middle",
  "Para"        = "Parramatta", 
  "parra" = "Parramatta",
  "Parra"  = "Parramatta",         
  "Parramatta"     = "Parramatta",
  "Rose"        ="Rose Bay",
  "Rose "        ="Rose Bay",
  "rose bay"    ="Rose Bay" , 
  "Rose Bay"     = "Rose Bay",
  "RoseBay"     ="Rose Bay",
  "South" = "South Head",
  "south head" = "South Head",
  "South Head"    ="South Head",
  "Southhead"= "South Head"))


####Convert the df variables to correct formats. Also convert GPS Coord Timestamps to Sydney Local time-From UTC(Zulu)

dat$People<-as.integer(as.character(dat$People))
dat$Rods<-as.integer(as.character(dat$Rods))
dat$ActivityType<-as.character(dat$Activity)
dat$TransectType<-as.character(dat$Transect)
dat$Coordint<-as.factor(dat$Coordint)
dat$time<-as.character(dat$time)#convert gps time to character for conversion to timestamp proper
dat$timeP <- strptime(dat$time,format='%Y-%m-%dT%H:%M:%S') #convert to a timestamp in right format
dat$pb.date <- as.POSIXct(dat$timeP, tz="GMT") #convert to POSIX timestamp in GMT time
dat$time_syd<-format(dat$pb.date, tz="Australia/Sydney") #convert to POSIX timestamp in Sydney Time
str(dat)


##convert loc, lat, long, dist, bearing to NEW LOCATION###

#convert everything to radians
dat$latR = dat$lat * pi/180 
dat$lonR = dat$lon * pi/180
dat$BearR = dat$Bearing * pi/180
radiusEarth = 6371
dat$distKM= dat$Distance/1000
dat$distR = dat$distKM/radiusEarth 

#convert to new point
dat$lat2 = asin(sin(dat$latR)*cos(dat$distR) + cos(dat$latR)*sin(dat$distR)*cos(dat$BearR))
dat$lon2 = dat$lonR + atan2(sin(dat$BearR)*sin(dat$distR)*cos(dat$latR), cos(dat$distR)-sin(dat$latR)*sin(dat$latR))

#back transform to degrees
dat$TargetLat = dat$lat2 * 180/pi
dat$Targetlon = dat$lon2 * 180/pi

##Need to find out unique transect reps i.e. create interaction term between Transect and Time, work out how many there are
dat$DayTrans.int<-interaction(dat$DayType,dat$Transect,dat$Period)
str(dat)

###Strip the timestamp to just the date
dat$date<-as.POSIXct(dat$time_syd, format="%Y-%m-%d %H:%M:%S") #convert time to POSIXCT timestamp
dat$date_1<-as.Date(dat$date,format='%d-%m-%y') #strip the time only leave date
dat$date_1<-as.factor(dat$date_1) #convert Timestamp to a factor for manipulation
levels(dat$date_1)
dat$Period<-revalue(dat$Period, c("FALSE"="Mid")) #for some reason there's wierd cleaning issue
levels(dat$Period)
levels(dat$Transect)
levels(dat$DayType)
levels(dat$Period)
levels(dat$date_1)


#cleaning out unused columns that were used to work out new lats and longs
colnames(dat)
new.dat<-dat[,c(1,3,4,5,13,14,15,16,17,18,25,26,27,28,31,39,40,41,43)]
new.dat[is.na(new.dat)] <- 0
str(new.dat)
new.dat$TransectType<-as.factor(new.dat$TransectType)
write.table(new.dat, file = "Harbour_Survey_combined_data_1.csv", append = FALSE, sep = ",", row.names=FALSE, col.names=TRUE, quote=TRUE)


str(new.dat)


###CREATING NEW SUMMARIES OF RODS AND PEOPLE PER TRANSECT, DAY AND PERIOD.
library(reshape2)
library(reshape2)
#melt data for reshaping and aggregating
melt.dat<-melt(new.dat, id=c("DayType","Period","TransectType","date_1"))
rods.people<-melt.dat[melt.dat$variable=='Rods' | melt.dat$variable=='People',] #just take data for Rods and People
rods.people$value<-as.numeric(rods.people$value) #revalue value as a numeric vector

rod.peep.sum<-dcast(rods.people,TransectType+DayType+Period+date_1~variable, fun.aggregate=sum) #sum the rods for each time period
str(rod.peep.sum)

##new summary df for Transect, Day and Period, means over Replicate numbers (represented by timestamps)
rod.peep.summary <- ddply(rod.peep.sum, c("TransectType", "DayType","Period"), summarize,mean.rods = round(mean(Rods),digits=2),sd.rods=round(sd(Rods),2), mean.peop=round(mean(People),2), sd.peop=round(sd(People),2))
write.csv(rod.peep.summary,"rod_people_wideform.csv")
rod.peep.summary

#####NEW FLAT TABLE FOR OTHER HARBOUR ACTIVITIES NOT FISHING OR PEOPLE

colnames(new.dat)
new.dat.5<-new.dat[c(2,4,13,14,19)]
activity.melt<-melt(new.dat.5,id=c('TransectType','DayType','Period','date_1'))
activity.sum<-dcast(activity.melt, TransectType+DayType+Period+date_1~value, fun.aggregate=length)
head(activity.sum)














##PLAY AREA BELOW###############
##################################################

ft <- ftable(ActivityType~TransectType+DayType+Period+date_1, data=new.dat) 
ft<-dcast(as.data.frame(ft), as.formula(paste(paste(names(attr(ft, "row.vars")), collapse="+"), "~", paste(names(attr(ft, "col.vars"))))))
ft<-rename(ft,c("Motor Boat"="MotorBoat", "Sail Boat"="SailBoat", "Fishing Boat"="FishingBoat", "Paddle Board"="PaddleBoard", "Sail Fleet"="SailFleet"))
ft$BoatTot<-ft$FishingBoat+ft$MotorBoat+ ft$SailBoat+ft$SailFleet ##total boats (i.e. fishing and motoring, not including sail boats)
head(ft)

ft[1:100,]


##work out how many reps were done for each tansect,day,time interaction
dat3<-read.csv('Harbour_Survey_combined_data_withReplicates.csv', header=T)####Note this datframe was manipulated in Excel (Rep added, which I couln't do in R)
head(dat3)
str(dat3)


levels(dat$rep.cod)
levels(dat$DayType)
levels(dat$)
##create new flat table of frequeny counts for each activity recorded during the survey
ft <- ftable(ActivityType~TransectType+DayType+Period+date_1, data=new.dat) 
ft<-dcast(as.data.frame(ft), as.formula(paste(paste(names(attr(ft, "row.vars")), collapse="+"), "~", paste(names(attr(ft, "col.vars"))))))
ft<-rename(ft,c("Motor Boat"="MotorBoat", "Sail Boat"="SailBoat", "Fishing Boat"="FishingBoat", "Paddle Board"="PaddleBoard", "Sail Fleet"="SailFleet"))
ft$BoatTot<-ft$FishingBoat+ft$MotorBoat+ ft$SailBoat+ft$SailFleet ##total boats (i.e. fishing and motoring, not including sail boats)
head(ft)
str(ft)
write.table(ft,"SH_Winter_Census_ANOVA_format_count_data.csv", sep=",", col.names=TRUE, row.names=FALSE)
levels(dat$rep.cod)


dat3$rods.int<-as.numeric(dat3$Rods)
dat3$people.int<-as.numeric(dat3$People)
dat4<-ddply(dat3, .(TransectType, DayType,Period,rep), summarize,
      NumRods = round(sum(rods.int), 2),
      NumPeople=round(sum(people.int),2))

head(dat4)

byday<-ddply(dat4, .(TransectType, DayType,Period), summarize,
             NumRods = round(mean(NumRods),2),
             NumPeople=round(mean(NumPeople),2))
byday

byday[which(byday$NumRods==max(byday$NumRods)),]
byday[which(byday$NumRods==min(byday$NumRods)),]

##BOATS
boats<-ddply(ft, .(TransectType, DayType,Period,rep), summarize, MotSum = round(sum(BoatTot),2),SailSum=round(sum(SailBoat),2))
head(boats)

boats[which(boats$MotSum==max(boats$MotSum)),]
byday[which(byday$NumRods==min(byday$NumRods)),]

###Totals

sum(ft$FishingBoat)
sum(ft$Kayak)
sum(ft$PaddleBoard)
sum(ft$BoatTot)
sum(dat4$NumPeople)
sum(dat4$NumRods)

a####END OF DATA MANIPULATION
#end result is a dataframe of marked points in space and time. Then go to Excel and put in replicate numbers. Resulting data below


str(dat3)


table.1<-table(dat3$TransectType,dat3$rep, dat3$ActivityType)



t1<-ftable(table.1)

data.frame(expand.grid(rev(attr(t1, "row.vars"))), unclass(t1))
library(reshape2)
dcast(as.data.frame(t1), as.formula(paste(paste(names(attr(t1, "row.vars")), collapse="+"),"~", paste(names(attr(t1, "col.vars"))))))


####PLAY AREA

library(plyr)
ddply(dat, .(Transect, DayType), summarise, 
      Total_Item = sum(Item_Qty), 
      Avg_Item_Price = mean(Item_Price),
      Penetration = length(unique(Cust_Num))/length(unique(dat$Cust_Num)))

tapply(dat3$ActivityType, c(dat3$DayType), sum) 



length(dat3$TransectType)
length(dat3$DayType)
length(dat3$Period)
length(dat3$rep)
#remove NA from number of rods
dat$Rods[is.na(dat$Rods)] <- 0


###Summary Activity Frequencies
table.1<-table(dat$TransectType, dat$DayType,dat$ActivityType)
ftable(table.1)
table.1
str(dat)

###Summary Number of Fishing Rods seen
tapply(dat$Activity,dat$Transect,mean)

fishdat<-dat[which(Activity=='Fishing Boat'),]
str(fishdat)


library(doBy)
summaryBy(Activity[which(Activity=='Fishing Boat'),]~Transect+DayType, data=dat, FUN=mean)

##summary of total rods per area and daytype
summaryBy(Rods~Area+DayType, data=dat, FUN=sum)

