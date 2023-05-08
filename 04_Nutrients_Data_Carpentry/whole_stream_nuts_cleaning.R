#Whole Stream Nutrient Data Cleaning + getting all year 2 data into one csv - CSC 07 Dec 2020
##UPDATED 12 January 2021 using updated landscape data file for year 2

library(dplyr)
library(lubridate)

#####
#read in the master data, date stuff, change stream names, create column for the nutrient concentrations in mg/L
whole_stream_nutrients_master <- read.csv("whole_stream_nutrients_master.csv")
whole_stream_nutrients_master <- whole_stream_nutrients_master %>% mutate(sample.date=mdy(sample.date))

whole_stream_nutrients_master[whole_stream_nutrients_master$stream == "towr", "stream"] <- "TOWR"
whole_stream_nutrients_master[whole_stream_nutrients_master$stream == "ws55", "stream"] <- "WS55"
whole_stream_nutrients_master[whole_stream_nutrients_master$stream == "ref", "stream"] <- "WS55"
whole_stream_nutrients_master[whole_stream_nutrients_master$stream == "warm", "stream"] <- "TOWR"
whole_stream_nutrients_master$convert.concentration.mgl <- (whole_stream_nutrients_master$convert.concentration.ugl/1000)

#get a subset of these data for just SI YR 2, delete unnecessary columns, get only flume data
whole_stream_nutrients_master_SIYR2 <- whole_stream_nutrients_master[which(whole_stream_nutrients_master$sample.date>"2018-08-31"),]
whole_stream_nutrients_master_SIYR2 <- whole_stream_nutrients_master_SIYR2[which(whole_stream_nutrients_master_SIYR2$sample.date<"2019-08-30"),]
whole_stream_nutrients_master_SIYR2 <- whole_stream_nutrients_master_SIYR2[c("analyte","sample.date","stream","stream.location","convert.concentration.mgl")]
whole_stream_nutrients_master_SIYR2 <- whole_stream_nutrients_master_SIYR2[which(whole_stream_nutrients_master_SIYR2$stream.location=="flume"),]

#subset by analyte, change colnames to match landscape data, drop more unnecessary columns
whole_stream_nutrients_master_SIYR2_NH4 <- whole_stream_nutrients_master_SIYR2[which(whole_stream_nutrients_master_SIYR2$analyte=="NH4"),]
whole_stream_nutrients_master_SIYR2_NO3 <-whole_stream_nutrients_master_SIYR2[which(whole_stream_nutrients_master_SIYR2$analyte=="NO3"),]
whole_stream_nutrients_master_SIYR2_PO4 <- whole_stream_nutrients_master_SIYR2[which(whole_stream_nutrients_master_SIYR2$analyte=="SRP"),]

colnames(whole_stream_nutrients_master_SIYR2_NH4)[colnames(whole_stream_nutrients_master_SIYR2_NH4)=="convert.concentration.mgl"]<-"nh4.mgl"
colnames(whole_stream_nutrients_master_SIYR2_NO3)[colnames(whole_stream_nutrients_master_SIYR2_NO3)=="convert.concentration.mgl"]<-"no3.mgl"
colnames(whole_stream_nutrients_master_SIYR2_PO4)[colnames(whole_stream_nutrients_master_SIYR2_PO4)=="convert.concentration.mgl"]<-"po4.mgl"

whole_stream_nutrients_master_SIYR2_NH4 <- whole_stream_nutrients_master_SIYR2_NH4[-c(1,4)]
whole_stream_nutrients_master_SIYR2_NO3 <- whole_stream_nutrients_master_SIYR2_NO3[-c(1,4)]
whole_stream_nutrients_master_SIYR2_PO4 <- whole_stream_nutrients_master_SIYR2_PO4[-c(1,4)]

#get averages by stream and date so that any duplicates will be averaged
averages_NH4 <- aggregate(nh4.mgl~sample.date+stream,data=whole_stream_nutrients_master_SIYR2_NH4,mean)
averages_NO3 <- aggregate(no3.mgl~sample.date+stream,data=whole_stream_nutrients_master_SIYR2_NO3,mean)
averages_PO4 <- aggregate(po4.mgl~sample.date+stream,data=whole_stream_nutrients_master_SIYR2_PO4,mean)

#merge the "averages" dataframes to get the data into the same format as the landscape data!
merge1<-merge(averages_NH4, averages_NO3, by=c("stream", "sample.date"))
merge2<-merge(merge1, averages_PO4, by=c("stream", "sample.date"))
colnames(merge2)[colnames(merge2)=="sample.date"] <- "date"
merge2$din.mgl<-(merge2$nh4.mgl+merge2$no3.mgl)

#write a csv just for the whole stream data
write.csv(merge2, "whole_stream_nutrients_master_CSC_SIYR2.csv")

#read in the landscape data, merge the whole stream data with the landscape data to create one big df for year 2!
landscape_nutrients_master_SIYR2<-read.csv("landscape_nutrients_master_CSC_SIYR2.csv")
landscape_nutrients_master_SIYR2 <- landscape_nutrients_master_SIYR2 %>% mutate(date=ymd(date))
landscape_nutrients_master_SIYR2$X <- NULL

bind_landscape_whole<-rbind(landscape_nutrients_master_SIYR2, merge2)

#write a csv for all the nutrient data for YR2
write.csv(bind_landscape_whole, "all_nutrients_master_CSC_SIYR2.csv")
