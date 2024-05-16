#Modeling the missing temperature data based on air temps
library(Rmisc)
library(lubridate)
library(dplyr)

#Watershed 1
ws.1<-read.csv("WS1_thrumarch_apr-oct_2018.csv", header=T)
colnames(ws.1)[colnames(ws.1)=="X"] <- "date.time"
colnames(ws.1)[colnames(ws.1)=="X.1"] <- "temp.celsius"
ws.1 <- ws.1[2:33048,]
ws.1<- ws.1 %>% mutate(Date=mdy_hm(date.time))
ws.1$Date <- as.Date(ws.1$Date)
ws.1$temp.celsius <- as.numeric(as.character(ws.1$temp.celsius))
ws.1$Date <- as.factor(ws.1$Date)
ws.1.dailies <- aggregate(ws.1$temp.celsius,by=list(ws.1$Date),FUN=mean,na.rm=T)
colnames(ws.1.dailies)[colnames(ws.1.dailies)=="x"] <- "daily.mean.temp"

ave.air.temps<-read.csv("ave.daily.temps.csv", header=T)
ave.air.temps <- ave.air.temps %>% mutate(yyyy.mm.dd=ymd(yyyy.mm.dd))
ave.air.temps$yyyy.mm.dd <- as.factor(ave.air.temps$yyyy.mm.dd)
colnames(ave.air.temps)[colnames(ave.air.temps)=="yyyy.mm.dd"] <- "Group.1"
merged.ws1<-merge(ws.1.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws1)[colnames(merged.ws1)=="TAVG"] <- "t.avg.air"
colnames(merged.ws1)[colnames(merged.ws1)=="daily.mean.temp"] <- "t.avg.stream"
ave.air.temps.marapr<-read.csv("ave.daily.temps.mar.apr.csv")
ave.air.temps.marapr <- ave.air.temps.marapr %>% mutate(yyyy.mm.dd=mdy(yyyy.mm.dd))
ave.air.temps.marapr$yyyy.mm.dd <- as.factor(ave.air.temps.marapr$yyyy.mm.dd)

mod.ws1<-lm(t.avg.stream~t.avg.air, data=merged.ws1)
summary(mod.ws1)
#Watershed1 slope = 0.50799, intercept = 5.41235
#R^2=0.88

modeled.temps.ws1<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws1)[colnames(modeled.temps.ws1)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws1)[colnames(modeled.temps.ws1)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws1$avg.stream.temp<-(((modeled.temps.ws1$avg.air.temp)*(0.50799))+5.41235)

ws.1.marchapril.average<-mean(modeled.temps.ws1$avg.stream.temp)
ws.1.marchapril.average

#Watershed 2
ws.2<-read.csv("WS2_thrumarch_apr-oct_2018.csv", header=T)
colnames(ws.2)[colnames(ws.2)=="Date.Time..GMT.04.00"] <- "date.time"
colnames(ws.2)[colnames(ws.2)=="temp.C"] <- "temp.celsius"
ws.2 <- ws.2 %>% mutate(Date=mdy_hm(date.time))
ws.2$Date <- as.Date(ws.2$Date)
ws.2$temp.celsius <- as.numeric(as.character(ws.2$temp.celsius))
ws.2$Date <- as.factor(ws.2$Date)
ws.2.dailies <- aggregate(ws.2$temp.celsius,by=list(ws.2$Date),FUN=mean,na.rm=T)
colnames(ws.2.dailies)[colnames(ws.2.dailies)=="x"] <- "daily.mean.temp"
merged.ws2<-merge(ws.2.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws2)[colnames(merged.ws2)=="TAVG"] <- "t.avg.air"
colnames(merged.ws2)[colnames(merged.ws2)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws2<-lm(t.avg.stream~t.avg.air, data=merged.ws2)
summary(mod.ws2)
#WS2 slope = 0.4432, intercept=7.0962
#R^2=0.86

modeled.temps.ws2<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws2)[colnames(modeled.temps.ws2)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws2)[colnames(modeled.temps.ws2)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws2$avg.stream.temp<-(((modeled.temps.ws2$avg.air.temp)*(0.4432))+7.0962)

ws.2.marchapril.average<-mean(modeled.temps.ws2$avg.stream.temp)
ws.2.marchapril.average

#Watershed 6
ws.6<-read.csv("WS6_thrumarch_apr-oct_2018.csv", header=T)
colnames(ws.6)[colnames(ws.6)=="Date.Time..GMT.04.00"] <- "date.time"
ws.6 <- ws.6 %>% mutate(Date=mdy_hm(date.time))
ws.6$Date <- as.Date(ws.6$Date)
ws.6$temp.celsius <- as.numeric(as.character(ws.6$temp.celsius))
ws.6$Date <- as.factor(ws.6$Date)
ws.6.dailies <- aggregate(ws.6$temp.celsius,by=list(ws.6$Date),FUN=mean,na.rm=T)
colnames(ws.6.dailies)[colnames(ws.6.dailies)=="x"] <- "daily.mean.temp"
merged.ws6<-merge(ws.6.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws6)[colnames(merged.ws6)=="TAVG"] <- "t.avg.air"
colnames(merged.ws6)[colnames(merged.ws6)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws6<-lm(t.avg.stream~t.avg.air, data=merged.ws6)
summary(mod.ws6)
#WS6 slope = 0.65862, intercept=3.44360
#R^2=0.88

modeled.temps.ws6<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws6)[colnames(modeled.temps.ws6)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws6)[colnames(modeled.temps.ws6)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws6$avg.stream.temp<-(((modeled.temps.ws6$avg.air.temp)*(0.65862))+3.44360)

ws.6.marchapril.average<-mean(modeled.temps.ws6$avg.stream.temp)
ws.6.marchapril.average

#Watershed 7
ws.7<-read.csv("WS7_thrumarch_apr-oct_2018.csv", header=T)
ws.7 <- ws.7 %>% mutate(Date=mdy_hm(date.time))
ws.7$Date <- as.Date(ws.7$Date)
ws.7$temp.celsius <- as.numeric(as.character(ws.7$temp.celsius))
ws.7$Date <- as.factor(ws.7$Date)
ws.7.dailies <- aggregate(ws.7$temp.celsius,by=list(ws.7$Date),FUN=mean,na.rm=T)
colnames(ws.7.dailies)[colnames(ws.7.dailies)=="x"] <- "daily.mean.temp"
merged.ws7<-merge(ws.7.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws7)[colnames(merged.ws7)=="TAVG"] <- "t.avg.air"
colnames(merged.ws7)[colnames(merged.ws7)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws7<-lm(t.avg.stream~t.avg.air, data=merged.ws7)
summary(mod.ws7)
#WS7 slope = 0.477799 intercept=6.164554
#R^2=0.88

modeled.temps.ws7<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws7)[colnames(modeled.temps.ws7)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws7)[colnames(modeled.temps.ws7)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws7$avg.stream.temp<-(((modeled.temps.ws7$avg.air.temp)*(0.477799))+6.164554)

ws.7.marchapril.average<-mean(modeled.temps.ws7$avg.stream.temp)
ws.7.marchapril.average

#Watershed 13
ws.13<-read.csv("WS13_thrumarch_apr-oct_2018.csv", header=T)
ws.13 <- ws.13 %>% mutate(Date=mdy_hm(date.time))
ws.13$Date <- as.Date(ws.13$Date)
ws.13$temp.celsius <- as.numeric(as.character(ws.13$temp.celsius))
ws.13$Date <- as.factor(ws.13$Date)
ws.13.dailies <- aggregate(ws.13$temp.celsius,by=list(ws.13$Date),FUN=mean,na.rm=T)
colnames(ws.13.dailies)[colnames(ws.13.dailies)=="x"] <- "daily.mean.temp"
merged.ws13<-merge(ws.13.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws13)[colnames(merged.ws13)=="TAVG"] <- "t.avg.air"
colnames(merged.ws13)[colnames(merged.ws13)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws13<-lm(t.avg.stream~t.avg.air, data=merged.ws13)
summary(mod.ws13)
#WS13 slope = 0.5326 intercept=5.1208
#R^2=0.87

modeled.temps.ws13<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws13)[colnames(modeled.temps.ws13)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws13)[colnames(modeled.temps.ws13)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws13$avg.stream.temp<-(((modeled.temps.ws13$avg.air.temp)*(0.5326))+5.1208)

ws.13.marchapril.average<-mean(modeled.temps.ws13$avg.stream.temp)
ws.13.marchapril.average

#Watershed 14
ws.14<-read.csv("WS14_thrumarch_apr-oct_2018.csv", header=T)
ws.14 <- ws.14 %>% mutate(Date=mdy_hm(date.time))
ws.14$Date <- as.Date(ws.14$Date)
ws.14$temp.celsius <- as.numeric(as.character(ws.14$temp.celsius))
ws.14$Date <- as.factor(ws.14$Date)
ws.14.dailies <- aggregate(ws.14$temp.celsius,by=list(ws.14$Date),FUN=mean,na.rm=T)
colnames(ws.14.dailies)[colnames(ws.14.dailies)=="x"] <- "daily.mean.temp"
merged.ws14<-merge(ws.14.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws14)[colnames(merged.ws14)=="TAVG"] <- "t.avg.air"
colnames(merged.ws14)[colnames(merged.ws14)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws14<-lm(t.avg.stream~t.avg.air, data=merged.ws14)
summary(mod.ws14)
#WS14 slope = 0.59332 intercept=4.17439
#R^2=0.88

modeled.temps.ws14<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws14)[colnames(modeled.temps.ws14)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws14)[colnames(modeled.temps.ws14)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws14$avg.stream.temp<-(((modeled.temps.ws14$avg.air.temp)*(0.59332))+4.17439)

ws.14.marchapril.average<-mean(modeled.temps.ws14$avg.stream.temp)
ws.14.marchapril.average

#Watershed 18
ws.18<-read.csv("WS18_thrumarch_apr-oct_2018.csv", header=T)
ws.18 <- ws.18 %>% mutate(Date=mdy_hm(date.time))
ws.18$Date <- as.Date(ws.18$Date)
ws.18$temp.celsius <- as.numeric(as.character(ws.18$temp.celsius))
ws.18$Date <- as.factor(ws.18$Date)
ws.18.dailies <- aggregate(ws.18$temp.celsius,by=list(ws.18$Date),FUN=mean,na.rm=T)
colnames(ws.18.dailies)[colnames(ws.18.dailies)=="x"] <- "daily.mean.temp"
merged.ws18<-merge(ws.18.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws18)[colnames(merged.ws18)=="TAVG"] <- "t.avg.air"
colnames(merged.ws18)[colnames(merged.ws18)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws18<-lm(t.avg.stream~t.avg.air, data=merged.ws18)
summary(mod.ws18)
#WS18 slope = 0.57982 intercept=4.35862
#R^2=0.87

modeled.temps.ws18<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws18)[colnames(modeled.temps.ws18)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws18)[colnames(modeled.temps.ws18)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws18$avg.stream.temp<-(((modeled.temps.ws18$avg.air.temp)*(0.57982))+4.35862)

ws.18.marchapril.average<-mean(modeled.temps.ws18$avg.stream.temp)
ws.18.marchapril.average

#Watershed 27
ws.27<-read.csv("WS27_thrumarch_apr-oct_2018.csv", header=T)
ws.27 <- ws.27 %>% mutate(Date=mdy_hm(date.time))
ws.27$Date <- as.Date(ws.27$Date)
ws.27$temp.celsius <- as.numeric(as.character(ws.27$temp.celsius))
ws.27$Date <- as.factor(ws.27$Date)
ws.27.dailies <- aggregate(ws.27$temp.celsius,by=list(ws.27$Date),FUN=mean,na.rm=T)
colnames(ws.27.dailies)[colnames(ws.27.dailies)=="x"] <- "daily.mean.temp"
merged.ws27<-merge(ws.27.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws27)[colnames(merged.ws27)=="TAVG"] <- "t.avg.air"
colnames(merged.ws27)[colnames(merged.ws27)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws27<-lm(t.avg.stream~t.avg.air, data=merged.ws27)
summary(mod.ws27)
#WS27 slope = 0.53149 intercept=3.89423
#R^2=0.83

modeled.temps.ws27<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws27)[colnames(modeled.temps.ws27)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws27)[colnames(modeled.temps.ws27)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws27$avg.stream.temp<-(((modeled.temps.ws27$avg.air.temp)*(0.53149))+3.89423)

ws.27.marchapril.average<-mean(modeled.temps.ws27$avg.stream.temp)
ws.27.marchapril.average

#Watershed 31
ws.31<-read.csv("WS31_thrumarch_apr-oct_2018.csv", header=T)
ws.31 <- ws.31 %>% mutate(Date=mdy_hm(date.time))
ws.31$Date <- as.Date(ws.31$Date)
ws.31$temp.celsius <- as.numeric(as.character(ws.31$temp.celsius))
ws.31$Date <- as.factor(ws.31$Date)
ws.31.dailies <- aggregate(ws.31$temp.celsius,by=list(ws.31$Date),FUN=mean,na.rm=T)
colnames(ws.31.dailies)[colnames(ws.31.dailies)=="x"] <- "daily.mean.temp"
merged.ws31<-merge(ws.31.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws31)[colnames(merged.ws31)=="TAVG"] <- "t.avg.air"
colnames(merged.ws31)[colnames(merged.ws31)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws31<-lm(t.avg.stream~t.avg.air, data=merged.ws31)
summary(mod.ws31)
#WS31 slope = 0.439256 intercept=6.52829
#R^2=0.88

modeled.temps.ws31<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws31)[colnames(modeled.temps.ws31)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws31)[colnames(modeled.temps.ws31)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws31$avg.stream.temp<-(((modeled.temps.ws31$avg.air.temp)*(0.439256))+6.52829)

ws.31.marchapril.average<-mean(modeled.temps.ws31$avg.stream.temp)
ws.31.marchapril.average

#Watershed 32
ws.32<-read.csv("WS32_thrumarch_apr-oct_2018.csv", header=T)
ws.32 <- ws.32 %>% mutate(Date=mdy_hm(date.time))
ws.32$Date <- as.Date(ws.32$Date)
ws.32$temp.celsius <- as.numeric(as.character(ws.32$temp.celsius))
ws.32$Date <- as.factor(ws.32$Date)
ws.32.dailies <- aggregate(ws.32$temp.celsius,by=list(ws.32$Date),FUN=mean,na.rm=T)
colnames(ws.32.dailies)[colnames(ws.32.dailies)=="x"] <- "daily.mean.temp"
merged.ws32<-merge(ws.32.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws32)[colnames(merged.ws32)=="TAVG"] <- "t.avg.air"
colnames(merged.ws32)[colnames(merged.ws32)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws32<-lm(t.avg.stream~t.avg.air, data=merged.ws32)
summary(mod.ws32)
#WS32 slope = 0.477192 intercept=5.513446
#R^2=0.87

modeled.temps.ws32<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws32)[colnames(modeled.temps.ws32)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws32)[colnames(modeled.temps.ws32)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws32$avg.stream.temp<-(((modeled.temps.ws32$avg.air.temp)*(0.477192))+5.513446)

ws.32.marchapril.average<-mean(modeled.temps.ws32$avg.stream.temp)
ws.32.marchapril.average

#Watershed 36
ws.36<-read.csv("WS36_thrumarch_apr-oct_2018.csv", header=T)
ws.36 <- ws.36 %>% mutate(Date=mdy_hm(date.time))
ws.36$Date <- as.Date(ws.36$Date)
ws.36$temp.celsius <- as.numeric(as.character(ws.36$temp.celsius))
ws.36$Date <- as.factor(ws.36$Date)
ws.36.dailies <- aggregate(ws.36$temp.celsius,by=list(ws.36$Date),FUN=mean,na.rm=T)
colnames(ws.36.dailies)[colnames(ws.36.dailies)=="x"] <- "daily.mean.temp"
merged.ws36<-merge(ws.36.dailies,ave.air.temps,by="Group.1")
colnames(merged.ws36)[colnames(merged.ws36)=="TAVG"] <- "t.avg.air"
colnames(merged.ws36)[colnames(merged.ws36)=="daily.mean.temp"] <- "t.avg.stream"

mod.ws36<-lm(t.avg.stream~t.avg.air, data=merged.ws36)
summary(mod.ws36)
#WS36 slope = 0.45853 intercept=5.59441
#R^2=0.85
modeled.temps.ws36<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.ws36)[colnames(modeled.temps.ws36)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.ws36)[colnames(modeled.temps.ws36)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.ws36$avg.stream.temp<-(((modeled.temps.ws36$avg.air.temp)*(0.45853))+5.59441)

ws.36.marchapril.average<-mean(modeled.temps.ws36$avg.stream.temp)
ws.36.marchapril.average

#WSLSF
ws.LSF<-read.csv("WSLSF_thrumarch_apr-oct_2018.csv", header=T)
ws.LSF <- ws.LSF %>% mutate(Date=mdy_hm(date.time))
ws.LSF$Date <- as.Date(ws.LSF$Date)
ws.LSF$temp.celsius <- as.numeric(as.character(ws.LSF$temp.celsius))
ws.LSF$Date <- as.factor(ws.LSF$Date)
ws.LSF.dailies <- aggregate(ws.LSF$temp.celsius,by=list(ws.LSF$Date),FUN=mean,na.rm=T)
colnames(ws.LSF.dailies)[colnames(ws.LSF.dailies)=="x"] <- "daily.mean.temp"
merged.wsLSF<-merge(ws.LSF.dailies,ave.air.temps,by="Group.1")
colnames(merged.wsLSF)[colnames(merged.wsLSF)=="TAVG"] <- "t.avg.air"
colnames(merged.wsLSF)[colnames(merged.wsLSF)=="daily.mean.temp"] <- "t.avg.stream"

mod.wsLSF<-lm(t.avg.stream~t.avg.air, data=merged.wsLSF)
summary(mod.wsLSF)
#WSLSF slope = 0.56573 intercept=4.52781
#R^2=0.89

modeled.temps.wsLSF<-data.frame(ave.air.temps.marapr$yyyy.mm.dd, ave.air.temps.marapr$TAVG)
colnames(modeled.temps.wsLSF)[colnames(modeled.temps.wsLSF)=="ave.air.temps.marapr.TAVG"] <- "avg.air.temp"
colnames(modeled.temps.wsLSF)[colnames(modeled.temps.wsLSF)=="ave.air.temps.marapr.yyyy.mm.dd"] <- "date"
modeled.temps.wsLSF$avg.stream.temp<-(((modeled.temps.wsLSF$avg.air.temp)*(0.56573))+4.52781)

ws.LSF.marchapril.average<-mean(modeled.temps.wsLSF$avg.stream.temp)
ws.LSF.marchapril.average


############
#31 March 2022 -- need to model 8/31/18-9/28/18 for LSF

#1.0 - get air temps for this period from CS01
ave.air.temps.augsep <- ave.air.temps[c(318:346),]
modeled.temps.wsLSF.augsep <- data.frame(ave.air.temps.augsep$Group.1, ave.air.temps.augsep$TAVG)
colnames(modeled.temps.wsLSF.augsep)[colnames(modeled.temps.wsLSF.augsep)=="ave.air.temps.augsep.Group.1"] <- "date"
colnames(modeled.temps.wsLSF.augsep)[colnames(modeled.temps.wsLSF.augsep)=="ave.air.temps.augsep.TAVG"] <- "avg.air.temp"
modeled.temps.wsLSF.augsep$avg.stream.temp <- (((modeled.temps.wsLSF.augsep$avg.air.temp)*(0.56573))+4.52781)

wsLSF.augsep.dailies <- data.frame("date"=modeled.temps.wsLSF.augsep$date, "temp.celsius"=modeled.temps.wsLSF.augsep$avg.stream.temp)
wsLSF.augsep.dailies$stream <- "WS08"
write.csv(wsLSF.augsep.dailies, "WS08_dailies_31Aug-29Sep_2018.csv")
############


######################################################################################################
#making new dataframes with just date and mean temp for all the streams for the modeled data from march-april of 2018

#WS1
ws.1.marapr.dailies<-data.frame("date" = modeled.temps.ws1$date, "temp.celsius"=modeled.temps.ws1$avg.stream.temp)
colnames(ws.1.marapr.dailies)[colnames(ws.1.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.1.marapr.dailies)[colnames(ws.1.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.1.marapr.dailies$stream<-"WS01"

#WS2
ws.2.marapr.dailies<-data.frame("date" = modeled.temps.ws2$date, "temp.celsius"=modeled.temps.ws2$avg.stream.temp)
colnames(ws.2.marapr.dailies)[colnames(ws.2.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.2.marapr.dailies)[colnames(ws.2.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.2.marapr.dailies$stream<-"WS02"

#WS6
ws.6.marapr.dailies<-data.frame("date" = modeled.temps.ws6$date, "temp.celsius"=modeled.temps.ws6$avg.stream.temp)
colnames(ws.6.marapr.dailies)[colnames(ws.6.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.6.marapr.dailies)[colnames(ws.6.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.6.marapr.dailies$stream<-"WS06"

#WS7
ws.7.marapr.dailies<-data.frame("date" = modeled.temps.ws7$date, "temp.celsius"=modeled.temps.ws7$avg.stream.temp)
colnames(ws.7.marapr.dailies)[colnames(ws.7.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.7.marapr.dailies)[colnames(ws.7.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.7.marapr.dailies$stream<-"WS07"

#WS13
ws.13.marapr.dailies<-data.frame("date" = modeled.temps.ws13$date, "temp.celsius"=modeled.temps.ws13$avg.stream.temp)
colnames(ws.13.marapr.dailies)[colnames(ws.13.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.13.marapr.dailies)[colnames(ws.13.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.13.marapr.dailies$stream<-"WS13"

#WS14
ws.14.marapr.dailies<-data.frame("date" = modeled.temps.ws14$date, "temp.celsius"=modeled.temps.ws14$avg.stream.temp)
colnames(ws.14.marapr.dailies)[colnames(ws.14.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.14.marapr.dailies)[colnames(ws.14.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.14.marapr.dailies$stream<-"WS14"

#WS18
ws.18.marapr.dailies<-data.frame("date" = modeled.temps.ws18$date, "temp.celsius"=modeled.temps.ws18$avg.stream.temp)
colnames(ws.18.marapr.dailies)[colnames(ws.18.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.18.marapr.dailies)[colnames(ws.18.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.18.marapr.dailies$stream<-"WS18"

#WS27
ws.27.marapr.dailies<-data.frame("date" = modeled.temps.ws27$date, "temp.celsius"=modeled.temps.ws27$avg.stream.temp)
colnames(ws.27.marapr.dailies)[colnames(ws.27.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.27.marapr.dailies)[colnames(ws.27.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.27.marapr.dailies$stream<-"WS27"

#WS31
ws.31.marapr.dailies<-data.frame("date" = modeled.temps.ws31$date, "temp.celsius"=modeled.temps.ws31$avg.stream.temp)
colnames(ws.31.marapr.dailies)[colnames(ws.31.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.31.marapr.dailies)[colnames(ws.31.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.31.marapr.dailies$stream<-"WS31"

#WS32
ws.32.marapr.dailies<-data.frame("date" = modeled.temps.ws32$date, "temp.celsius"=modeled.temps.ws32$avg.stream.temp)
colnames(ws.32.marapr.dailies)[colnames(ws.32.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.32.marapr.dailies)[colnames(ws.32.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.32.marapr.dailies$stream<-"WS32"

#WS36
ws.36.marapr.dailies<-data.frame("date" = modeled.temps.ws36$date, "temp.celsius"=modeled.temps.ws36$avg.stream.temp)
colnames(ws.36.marapr.dailies)[colnames(ws.36.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.36.marapr.dailies)[colnames(ws.36.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.36.marapr.dailies$stream<-"WS36"

#WSLSF
ws.LSF.marapr.dailies<-data.frame("date" = modeled.temps.wsLSF$date, "temp.celsius"=modeled.temps.wsLSF$avg.stream.temp)
colnames(ws.LSF.marapr.dailies)[colnames(ws.LSF.marapr.dailies)=="date"] <- "Group.1"
colnames(ws.LSF.marapr.dailies)[colnames(ws.LSF.marapr.dailies)=="temp.celsius"] <- "daily.mean.temp"
ws.LSF.marapr.dailies$stream<-"WS08"

#one big dataframe with all the modeled data
all.modeled.data<-data.frame(bind_rows(ws.1.marapr.dailies, ws.2.marapr.dailies, ws.6.marapr.dailies, ws.7.marapr.dailies,
          ws.13.marapr.dailies, ws.14.marapr.dailies, ws.18.marapr.dailies, ws.27.marapr.dailies,
          ws.31.marapr.dailies, ws.32.marapr.dailies, ws.36.marapr.dailies, ws.LSF.marapr.dailies))

colnames(all.modeled.data)[colnames(all.modeled.data)=="Group.1"] <- "date"
colnames(all.modeled.data)[colnames(all.modeled.data)=="daily.mean.temp"] <- "temp.celsius"


write.csv(all.modeled.data, "all.modeled.data.csv")




