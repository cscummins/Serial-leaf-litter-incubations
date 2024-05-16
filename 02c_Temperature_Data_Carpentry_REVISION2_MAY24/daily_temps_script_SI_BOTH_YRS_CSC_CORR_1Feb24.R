#Getting temp data into daily means - Serial incubations - YR1 & YR2 CSC 16 Nov 2020
#UPDATED 1 Feb 24 after taking out the implausibly high temperature values on 4/5 and 4/6 
#(these values were likely due to logger being in the car at room temperature instead of in the stream) 
#Then we fixed the regressions for the modeled data
#We will now update the daily data

library(Rmisc)
library(lubridate)
library(dplyr)
#############
#YEAR 1#
#############

#Get individual dataframes for each stream for the month of modeled data
all.modeled.data<-read.csv("all.modeled.data.1Feb24.csv")
all.modeled.data$X<-NULL

#
modeled_WS01<-all.modeled.data[which(all.modeled.data$stream=="WS01"),]
modeled_WS01 <- modeled_WS01 %>% mutate(date=ymd(date))
#
modeled_WS02<-all.modeled.data[which(all.modeled.data$stream=="WS02"),]
modeled_WS02 <- modeled_WS02 %>% mutate(date=ymd(date))

#
modeled_WS06<-all.modeled.data[which(all.modeled.data$stream=="WS06"),]
modeled_WS06 <- modeled_WS06 %>% mutate(date=ymd(date))

#
modeled_WS07<-all.modeled.data[which(all.modeled.data$stream=="WS07"),]
modeled_WS07 <- modeled_WS07 %>% mutate(date=ymd(date))

#
modeled_WS08<-all.modeled.data[which(all.modeled.data$stream=="WS08"),]
modeled_WS08 <- modeled_WS08 %>% mutate(date=ymd(date))

#
modeled_WS13<-all.modeled.data[which(all.modeled.data$stream=="WS13"),]
modeled_WS13 <- modeled_WS13 %>% mutate(date=ymd(date))

#
modeled_WS14<-all.modeled.data[which(all.modeled.data$stream=="WS14"),]
modeled_WS14 <- modeled_WS14 %>% mutate(date=ymd(date))

#
modeled_WS18<-all.modeled.data[which(all.modeled.data$stream=="WS18"),]
modeled_WS18 <- modeled_WS18 %>% mutate(date=ymd(date))

#
modeled_WS27<-all.modeled.data[which(all.modeled.data$stream=="WS27"),]
modeled_WS27 <- modeled_WS27 %>% mutate(date=ymd(date))

#
modeled_WS31<-all.modeled.data[which(all.modeled.data$stream=="WS31"),]
modeled_WS31 <- modeled_WS31 %>% mutate(date=ymd(date))

#
modeled_WS32<-all.modeled.data[which(all.modeled.data$stream=="WS32"),]
modeled_WS32 <- modeled_WS32 %>% mutate(date=ymd(date))

#
modeled_WS36<-all.modeled.data[which(all.modeled.data$stream=="WS36"),]
modeled_WS36 <- modeled_WS36 %>% mutate(date=ymd(date))


#################################################################################################

#Get daily files for all the streams and bind them in the correct order with the modeled data...
#WS01
WS01_thrumarch<-read.csv("WS01_10951962_thrumarch2018.csv", header=TRUE)
WS01_AprOct<-read.csv("WS01_20195069_Apr-Oct18.csv", header=TRUE)

WS01_thrumarch <- WS01_thrumarch %>% mutate(date=mdy(date))
WS01_AprOct <- WS01_AprOct %>% mutate(date=mdy(date))

#WS01 dailies
WS01_dailies_thrumarch<-summarySE(WS01_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS01_dailies_thrumarch$stream <- "WS01"
WS01_dailies_thrumarch$N<-NULL
WS01_dailies_thrumarch$sd<-NULL
WS01_dailies_thrumarch$se<-NULL
WS01_dailies_thrumarch$ci<-NULL

WS01_dailies_AprOct<-summarySE(WS01_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS01_dailies_AprOct$stream <- "WS01"
WS01_dailies_AprOct$N<-NULL
WS01_dailies_AprOct$sd<-NULL
WS01_dailies_AprOct$se<-NULL
WS01_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS01_dailies_AprOct <- WS01_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))


bind_WS01_1<-rbind(WS01_dailies_thrumarch, modeled_WS01)
bind_WS01_all<-rbind(bind_WS01_1, WS01_dailies_AprOct)


#WS02
WS02_thrumarch<-read.csv("WS02_10951968_thrumarch2018.csv", header=TRUE)
WS02_AprOct<-read.csv("WS02_20195065_Apr-Oct18.csv", header=TRUE)

WS02_thrumarch <- WS02_thrumarch %>% mutate(date=mdy(date))
WS02_AprOct <- WS02_AprOct %>% mutate(date=mdy(date))

#WS02 dailies
WS02_dailies_thrumarch<-summarySE(WS02_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS02_dailies_thrumarch$stream <- "WS02"
WS02_dailies_thrumarch$N<-NULL
WS02_dailies_thrumarch$sd<-NULL
WS02_dailies_thrumarch$se<-NULL
WS02_dailies_thrumarch$ci<-NULL

WS02_dailies_AprOct<-summarySE(WS02_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS02_dailies_AprOct$stream <- "WS02"
WS02_dailies_AprOct$N<-NULL
WS02_dailies_AprOct$sd<-NULL
WS02_dailies_AprOct$se<-NULL
WS02_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS02_dailies_AprOct <- WS02_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS02_1<-rbind(WS02_dailies_thrumarch, modeled_WS02)
bind_WS02_all<-rbind(bind_WS02_1, WS02_dailies_AprOct)

#WS06
WS06_thrumarch<-read.csv("WS06_10951957_thrumarch2018.csv", header=TRUE)
WS06_AprOct<-read.csv("WS06_20195071_Apr-Oct18.csv", header=TRUE)

WS06_thrumarch <- WS06_thrumarch %>% mutate(date=mdy(date))
WS06_AprOct <- WS06_AprOct %>% mutate(date=mdy(date))

#WS06 dailies
WS06_dailies_thrumarch<-summarySE(WS06_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS06_dailies_thrumarch$stream <- "WS06"
WS06_dailies_thrumarch$N<-NULL
WS06_dailies_thrumarch$sd<-NULL
WS06_dailies_thrumarch$se<-NULL
WS06_dailies_thrumarch$ci<-NULL

WS06_dailies_AprOct<-summarySE(WS06_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS06_dailies_AprOct$stream <- "WS06"
WS06_dailies_AprOct$N<-NULL
WS06_dailies_AprOct$sd<-NULL
WS06_dailies_AprOct$se<-NULL
WS06_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS06_dailies_AprOct <- WS06_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS06_1<-rbind(WS06_dailies_thrumarch, modeled_WS06)
bind_WS06_all<-rbind(bind_WS06_1, WS06_dailies_AprOct)

#WS07
WS07_thrumarch<-read.csv("WS07_10951956_thrumarch2018.csv", header=TRUE)
WS07_AprOct<-read.csv("WS07_20195066_Apr-Oct18.csv", header=TRUE)

WS07_thrumarch <- WS07_thrumarch %>% mutate(date=mdy(date))
WS07_AprOct <- WS07_AprOct %>% mutate(date=mdy(date))

#WS07 dailies
WS07_dailies_thrumarch<-summarySE(WS07_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS07_dailies_thrumarch$stream <- "WS07"
WS07_dailies_thrumarch$N<-NULL
WS07_dailies_thrumarch$sd<-NULL
WS07_dailies_thrumarch$se<-NULL
WS07_dailies_thrumarch$ci<-NULL

WS07_dailies_AprOct<-summarySE(WS07_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS07_dailies_AprOct$stream <- "WS07"
WS07_dailies_AprOct$N<-NULL
WS07_dailies_AprOct$sd<-NULL
WS07_dailies_AprOct$se<-NULL
WS07_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS07_dailies_AprOct <- WS07_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS07_1<-rbind(WS07_dailies_thrumarch, modeled_WS07)
bind_WS07_all<-rbind(bind_WS07_1, WS07_dailies_AprOct)

#WS08
WS08_thrumarch<-read.csv("WSLSF_10951965_thrumarch2018.csv", header=TRUE)
WS08_AprSept<-read.csv("WSLSF_20195070_Apr-Sept18.csv", header=TRUE)

WS08_thrumarch <- WS08_thrumarch %>% mutate(date=mdy(date))
WS08_AprSept <- WS08_AprSept %>% mutate(date=mdy(date))

#WS08 dailies
WS08_dailies_thrumarch<-summarySE(WS08_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS08_dailies_thrumarch$stream <- "WS08"
WS08_dailies_thrumarch$N<-NULL
WS08_dailies_thrumarch$sd<-NULL
WS08_dailies_thrumarch$se<-NULL
WS08_dailies_thrumarch$ci<-NULL

WS08_dailies_AprSept<-summarySE(WS08_AprSept, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS08_dailies_AprSept$stream <- "WS08"
WS08_dailies_AprSept$N<-NULL
WS08_dailies_AprSept$sd<-NULL
WS08_dailies_AprSept$se<-NULL
WS08_dailies_AprSept$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS08_dailies_AprSept <- WS08_dailies_AprSept %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS08_1<-rbind(WS08_dailies_thrumarch, modeled_WS08)
bind_WS08_2<-rbind(bind_WS08_1, WS08_dailies_AprSept)
bind_WS08_2<-bind_WS08_2[which(bind_WS08_2$date<"2018-08-31"),]

#WS13
WS13_thrumarch<-read.csv("WS13_10951970_thrumarch2018.csv", header=TRUE)
WS13_AprOct<-read.csv("WS13_20195054_Apr-Oct18.csv", header=TRUE)

WS13_thrumarch <- WS13_thrumarch %>% mutate(date=mdy(date))
WS13_AprOct <- WS13_AprOct %>% mutate(date=mdy(date))

#WS13 dailies
WS13_dailies_thrumarch<-summarySE(WS13_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS13_dailies_thrumarch$stream <- "WS13"
WS13_dailies_thrumarch$N<-NULL
WS13_dailies_thrumarch$sd<-NULL
WS13_dailies_thrumarch$se<-NULL
WS13_dailies_thrumarch$ci<-NULL

WS13_dailies_AprOct<-summarySE(WS13_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS13_dailies_AprOct$stream <- "WS13"
WS13_dailies_AprOct$N<-NULL
WS13_dailies_AprOct$sd<-NULL
WS13_dailies_AprOct$se<-NULL
WS13_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS13_dailies_AprOct <- WS13_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS13_1<-rbind(WS13_dailies_thrumarch, modeled_WS13)
bind_WS13_all<-rbind(bind_WS13_1, WS13_dailies_AprOct)

#WS14
WS14_thrumarch<-read.csv("WS14_10951966_thrumarch2018.csv", header=TRUE)
WS14_AprOct<-read.csv("WS14_20195072_Apr-Oct18.csv", header=TRUE)

WS14_thrumarch <- WS14_thrumarch %>% mutate(date=mdy(date))
WS14_AprOct <- WS14_AprOct %>% mutate(date=mdy(date))

#WS14 dailies
WS14_dailies_thrumarch<-summarySE(WS14_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS14_dailies_thrumarch$stream <- "WS14"
WS14_dailies_thrumarch$N<-NULL
WS14_dailies_thrumarch$sd<-NULL
WS14_dailies_thrumarch$se<-NULL
WS14_dailies_thrumarch$ci<-NULL

WS14_dailies_AprOct<-summarySE(WS14_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS14_dailies_AprOct$stream <- "WS14"
WS14_dailies_AprOct$N<-NULL
WS14_dailies_AprOct$sd<-NULL
WS14_dailies_AprOct$se<-NULL
WS14_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS14_dailies_AprOct <- WS14_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS14_1<-rbind(WS14_dailies_thrumarch, modeled_WS14)
bind_WS14_all<-rbind(bind_WS14_1, WS14_dailies_AprOct)

#WS18
WS18_thrumarch<-read.csv("WS18_10951963_thrumarch2018.csv", header=TRUE)
WS18_AprOct<-read.csv("WS18_20195061_Apr-Oct18.csv", header=TRUE)

WS18_thrumarch <- WS18_thrumarch %>% mutate(date=mdy(date))
WS18_AprOct <- WS18_AprOct %>% mutate(date=mdy(date))

#WS18 dailies
WS18_dailies_thrumarch<-summarySE(WS18_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS18_dailies_thrumarch$stream <- "WS18"
WS18_dailies_thrumarch$N<-NULL
WS18_dailies_thrumarch$sd<-NULL
WS18_dailies_thrumarch$se<-NULL
WS18_dailies_thrumarch$ci<-NULL

WS18_dailies_AprOct<-summarySE(WS18_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS18_dailies_AprOct$stream <- "WS18"
WS18_dailies_AprOct$N<-NULL
WS18_dailies_AprOct$sd<-NULL
WS18_dailies_AprOct$se<-NULL
WS18_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS18_dailies_AprOct <- WS18_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS18_1<-rbind(WS18_dailies_thrumarch, modeled_WS18)
bind_WS18_all<-rbind(bind_WS18_1, WS18_dailies_AprOct)

#WS27
WS27_thrumarch<-read.csv("WS27_10951961_thrumarch2018.csv", header=TRUE)
WS27_AprOct<-read.csv("WS27_20195068_Apr-Oct18.csv", header=TRUE)

WS27_thrumarch <- WS27_thrumarch %>% mutate(date=mdy(date))
WS27_AprOct <- WS27_AprOct %>% mutate(date=mdy(date))

#WS27 dailies
WS27_dailies_thrumarch<-summarySE(WS27_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS27_dailies_thrumarch$stream <- "WS27"
WS27_dailies_thrumarch$N<-NULL
WS27_dailies_thrumarch$sd<-NULL
WS27_dailies_thrumarch$se<-NULL
WS27_dailies_thrumarch$ci<-NULL

WS27_dailies_AprOct<-summarySE(WS27_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS27_dailies_AprOct$stream <- "WS27"
WS27_dailies_AprOct$N<-NULL
WS27_dailies_AprOct$sd<-NULL
WS27_dailies_AprOct$se<-NULL
WS27_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS27_dailies_AprOct <- WS27_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS27_1<-rbind(WS27_dailies_thrumarch, modeled_WS27)
bind_WS27_all<-rbind(bind_WS27_1, WS27_dailies_AprOct)

#WS31
WS31_thrumarch<-read.csv("WS31_10951960_thrumarch2018.csv", header=TRUE)
WS31_AprOct<-read.csv("WS31_20195067_Apr-Oct18.csv", header=TRUE)

WS31_thrumarch <- WS31_thrumarch %>% mutate(date=mdy(date))
WS31_AprOct <- WS31_AprOct %>% mutate(date=mdy(date))

#WS31 dailies
WS31_dailies_thrumarch<-summarySE(WS31_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS31_dailies_thrumarch$stream <- "WS31"
WS31_dailies_thrumarch$N<-NULL
WS31_dailies_thrumarch$sd<-NULL
WS31_dailies_thrumarch$se<-NULL
WS31_dailies_thrumarch$ci<-NULL

WS31_dailies_AprOct<-summarySE(WS31_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS31_dailies_AprOct$stream <- "WS31"
WS31_dailies_AprOct$N<-NULL
WS31_dailies_AprOct$sd<-NULL
WS31_dailies_AprOct$se<-NULL
WS31_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS31_dailies_AprOct <- WS31_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS31_1<-rbind(WS31_dailies_thrumarch, modeled_WS31)
bind_WS31_all<-rbind(bind_WS31_1, WS31_dailies_AprOct)

#WS32
WS32_thrumarch<-read.csv("WS32_10951959_thrumarch2018.csv", header=TRUE)
WS32_AprOct<-read.csv("WS32_20195058_Apr-Oct18.csv", header=TRUE)

WS32_thrumarch <- WS32_thrumarch %>% mutate(date=mdy(date))
WS32_AprOct <- WS32_AprOct %>% mutate(date=mdy(date))

#WS32 dailies
WS32_dailies_thrumarch<-summarySE(WS32_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS32_dailies_thrumarch$stream <- "WS32"
WS32_dailies_thrumarch$N<-NULL
WS32_dailies_thrumarch$sd<-NULL
WS32_dailies_thrumarch$se<-NULL
WS32_dailies_thrumarch$ci<-NULL

WS32_dailies_AprOct<-summarySE(WS32_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS32_dailies_AprOct$stream <- "WS32"
WS32_dailies_AprOct$N<-NULL
WS32_dailies_AprOct$sd<-NULL
WS32_dailies_AprOct$se<-NULL
WS32_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS32_dailies_AprOct <- WS32_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS32_1<-rbind(WS32_dailies_thrumarch, modeled_WS32)
bind_WS32_all<-rbind(bind_WS32_1, WS32_dailies_AprOct)

#WS36
WS36_thrumarch<-read.csv("WS36_10951964_thrumarch2018.csv", header=TRUE)
WS36_AprOct<-read.csv("WS36_20195052_Apr-Oct18.csv", header=TRUE)

WS36_thrumarch <- WS36_thrumarch %>% mutate(date=mdy(date))
WS36_AprOct <- WS36_AprOct %>% mutate(date=mdy(date))

#WS36 dailies
WS36_dailies_thrumarch<-summarySE(WS36_thrumarch, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS36_dailies_thrumarch$stream <- "WS36"
WS36_dailies_thrumarch$N<-NULL
WS36_dailies_thrumarch$sd<-NULL
WS36_dailies_thrumarch$se<-NULL
WS36_dailies_thrumarch$ci<-NULL

WS36_dailies_AprOct<-summarySE(WS36_AprOct, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS36_dailies_AprOct$stream <- "WS36"
WS36_dailies_AprOct$N<-NULL
WS36_dailies_AprOct$sd<-NULL
WS36_dailies_AprOct$se<-NULL
WS36_dailies_AprOct$ci<-NULL
#take out the data we don't trust from 4/5 and 4/6
WS36_dailies_AprOct <- WS36_dailies_AprOct %>% filter(!date %in% c("2018-04-05", "2018-04-06"))

bind_WS36_1<-rbind(WS36_dailies_thrumarch, modeled_WS36)
bind_WS36_all<-rbind(bind_WS36_1, WS36_dailies_AprOct)

###
#31 March 2022 - add modeled data for Aug 31- Sep 28 to the WS08 df
WS08_augsep <- read.csv("WS08_dailies_31Aug-29Sep_2018_1Feb24.csv")
WS08_augsep <- WS08_augsep %>% mutate(date=ymd(date))
WS08_augsep$X <- NULL
bind_WS08_all <- rbind(bind_WS08_2, WS08_augsep)
####################################################################################################################
#Make one big summary dataframe - YR1
all_daily_temps_SIYR1 <- do.call("rbind", list(bind_WS01_all, bind_WS02_all, bind_WS06_all, bind_WS07_all, bind_WS08_all, bind_WS13_all, bind_WS14_all, bind_WS18_all, bind_WS27_all, 
                      bind_WS31_all, bind_WS32_all, bind_WS36_all))
#Here's the csv :)
write.csv(all_daily_temps_SIYR1, "landscape_dailytemp_masterlong_SIyr1.1Feb24.csv")
#NOTE THAT MARCH - APRIL ARE MODELED FOR ALL STREAMS!!!!!!!!!!!!!!!!!!!!



#########
#YEAR 2#
#########

#Get daily files for all the streams and bind them together

#CWCR
CWCR<-read.csv("CWCR_10951959_all.csv", header=TRUE)

CWCR <- CWCR %>% mutate(date=mdy(date))


CWCR_dailies<-summarySE(CWCR, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
CWCR_dailies$stream <- "CWCR"
CWCR_dailies$N<-NULL
CWCR_dailies$sd<-NULL
CWCR_dailies$se<-NULL
CWCR_dailies$ci<-NULL

#HNCR
HNCR<-read.csv("HNCR_9736060_all.csv", header=TRUE)

HNCR <- HNCR %>% mutate(date=mdy(date))

HNCR_dailies<-summarySE(HNCR, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
HNCR_dailies$stream <- "HNCR"
HNCR_dailies$N<-NULL
HNCR_dailies$sd<-NULL
HNCR_dailies$se<-NULL
HNCR_dailies$ci<-NULL

#LCCR
LCCR<-read.csv("LCCR_10951956_10684938_all.csv", header=TRUE)

LCCR <- LCCR %>% mutate(date=mdy(date))

LCCR_dailies<-summarySE(LCCR, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
LCCR_dailies$stream <- "LCCR"
LCCR_dailies$N<-NULL
LCCR_dailies$sd<-NULL
LCCR_dailies$se<-NULL
LCCR_dailies$ci<-NULL

#USHF
USHF<-read.csv("USHF_10951957_9736055_all.csv", header=TRUE)

USHF <- USHF %>% mutate(date=mdy(date))

USHF_dailies<-summarySE(USHF, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
USHF_dailies$stream <- "USHF"
USHF_dailies$N<-NULL
USHF_dailies$sd<-NULL
USHF_dailies$se<-NULL
USHF_dailies$ci<-NULL

#WS09
WS09<-read.csv("WS09_10951961_all.csv", header=TRUE)

WS09 <- WS09 %>% mutate(date=mdy(date))

WS09_dailies<-summarySE(WS09, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS09_dailies$stream <- "WS09"
WS09_dailies$N<-NULL
WS09_dailies$sd<-NULL
WS09_dailies$se<-NULL
WS09_dailies$ci<-NULL

#WS17
WS17<-read.csv("WS17_10951962_all.csv", header=TRUE)

WS17 <- WS17 %>% mutate(date=mdy(date))

WS17_dailies<-summarySE(WS17, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS17_dailies$stream <- "WS17"
WS17_dailies$N<-NULL
WS17_dailies$sd<-NULL
WS17_dailies$se<-NULL
WS17_dailies$ci<-NULL

#WS34
WS34<-read.csv("WS34_10951965_all.csv", header=TRUE)

WS34 <- WS34 %>% mutate(date=mdy(date))

WS34_dailies<-summarySE(WS34, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS34_dailies$stream <- "WS34"
WS34_dailies$N<-NULL
WS34_dailies$sd<-NULL
WS34_dailies$se<-NULL
WS34_dailies$ci<-NULL

#WS37
WS37<-read.csv("WS37_10951966_all.csv", header=TRUE)

WS37 <- WS37 %>% mutate(date=mdy(date))

WS37_dailies<-summarySE(WS37, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS37_dailies$stream <- "WS37"
WS37_dailies$N<-NULL
WS37_dailies$sd<-NULL
WS37_dailies$se<-NULL
WS37_dailies$ci<-NULL

#WS55
WS55<-read.csv("20288753_C55_82mtemp_SIyr2.csv", header=TRUE)

WS55 <- WS55 %>% mutate(date=mdy(date))

WS55_dailies<-summarySE(WS55, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
WS55_dailies$stream <- "WS55"
WS55_dailies$N<-NULL
WS55_dailies$sd<-NULL
WS55_dailies$se<-NULL
WS55_dailies$ci<-NULL

#TOWR
TOWR<-read.csv("20288759_Tower83mtemp_SIyr2.csv", header=TRUE)

TOWR <- TOWR %>% mutate(date=mdy(date))

TOWR_dailies<-summarySE(TOWR, measurevar="temp.celsius", groupvars="date", na.rm=TRUE)
TOWR_dailies$stream <- "TOWR"
TOWR_dailies$N<-NULL
TOWR_dailies$sd<-NULL
TOWR_dailies$se<-NULL
TOWR_dailies$ci<-NULL


####################################################################################################################
#Make one big summary dataframe - YR2
all_daily_temps_SIYR2<-do.call("rbind", list(CWCR_dailies, HNCR_dailies, LCCR_dailies, USHF_dailies, WS09_dailies, WS17_dailies,
                                             WS34_dailies, WS37_dailies, TOWR_dailies, WS55_dailies))

#Here's the csv for yr 2
write.csv(all_daily_temps_SIYR2, "landscape_dailytemp_masterlong_SIyr2.csv")
