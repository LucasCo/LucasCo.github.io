####Converting decimal, degree, seconds, to Decimals

library(sp)

##Converting ridiculous Eastings and Northings to Decimals
down5<-read.csv('harbour_survey_dataDownload_5.csv', header=T)
str(down5)
head(down5)


down5$lat<-gsub("o","d",down5$lat)
down5$lon<-gsub("o","d",down5$lon)


down5$lat<-as.numeric(char2dms(down5$lat))
down5$lon<-as.numeric(char2dms(down5$lon))

head(down5)

write.csv(down5,'harbour_survey_datadownload_5_1.csv', row.names=FALSE)