#####
#Rainfall data for Sydney Harbour- creating a look up table
###

rm(list=ls())

source("R/functions.R")
source("R/data_read_in_est_and_use.R")

#read in rainfall dat
dat<-read.csv("Data/rainfall_data.csv", header=T, stringsAsFactors=FALSE)
dat<-dat[,3:6]
colnames(dat)[4]<-"Rainfall"

#subset to year 2013:2013
dat<-dat[dat$Year>=2013,]

dat$Date<-as.character(interaction(dat$Day,dat$Month,dat$Year,sep=":"))
dat$Date<-as.Date(dat$Date,format='%d:%m:%Y')
dat$newVariable<-"rainfall"
dat$lookupVariable<-"date_1"
dat$newValue<-dat$Rainfall

colnames(dat)[4:5]<-c("newValue","lookupValue")

write.table(dat, "Data/rainfall_lookup_table.csv", sep=",")

len<-length(dat)
rainfall_lookup<-data.frame(lookupVariable=dat$date_1, newValue=rep("rainfall",len))
