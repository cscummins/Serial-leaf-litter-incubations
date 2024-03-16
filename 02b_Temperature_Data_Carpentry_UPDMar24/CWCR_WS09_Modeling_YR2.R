#Modeling CWCR and WS09 off of each other to create regression


CWCR_data <- read.csv("CWCR_7Feb19_thru_29Aug19.csv", header = TRUE)
WS09_data <- read.csv("WS09_7Feb19_thru_29Aug19.csv", header = TRUE)

colnames(CWCR_data)[colnames(CWCR_data)=="Date.Time..GMT.04.00"] <- "date.time"
colnames(CWCR_data)[colnames(CWCR_data)=="Temp...C..LGR.S.N..10951959..SEN.S.N..10951959."] <- "temp.celsius"

colnames(WS09_data)[colnames(WS09_data)=="Date.Time..GMT.04.00"] <- "date.time"
colnames(WS09_data)[colnames(WS09_data)=="Temp...C..LGR.S.N..10951961..SEN.S.N..10951961."] <- "temp.celsius"

CWCR_data$date<-as.Date(strptime(CWCR_data$date.time,format=("%m/%d/%y")))
WS09_data$date<-as.Date(strptime(WS09_data$date.time,format=("%m/%d/%y")))

CWCR_data$temp.celsius <- as.numeric(as.character(CWCR_data$temp.celsius))
WS09_data$temp.celsius <- as.numeric(as.character(WS09_data$temp.celsius))

CWCR_data$n<-seq(1,nrow(CWCR_data), by=1)
WS09_data$n<-seq(1,nrow(WS09_data),by=1)

colnames(CWCR_data)[colnames(CWCR_data)=="temp.celsius"] <- "temp.celsius.CWCR"
colnames(WS09_data)[colnames(WS09_data)=="temp.celsius"] <- "temp.celsius.WS09"

library(dplyr)
library(lubridate)

#NOTES:
#had to to figure out where CWCR logger skipped - figured it out with code that is now deleted...
#Found out that CWCR logger skipped one temp recording on 6/5/19 at 14:19. We added a row to the dataframe for this time with "NA" as the temp
#WS09 1 minute ahead of CWCR so added 1 minute to CWCR to make the times match (mostly just for funsies -- need to remember that CWCR was originally 1 min behind WS09)
#turns out that starting on 8/7/19 at 10:56, CWCR is 6 minutes ahead of WS09 (likely because of a logger download)
#still the same # of observations but from this point to the end it's 6 minutes off instead of 1
#made a new dataframe for the regression with these observations (the ones that are 6 min ahead for CWCR) left out for consistency. See below.

#for some reason I have to run the code below one pipe at a time...it works fine if I do this but not if I run it all at once.
CWCR_data_2 <- CWCR_data %>% add_row(date.time = "6/5/19 14:19", temp.celsius.CWCR=NA, date=as.Date("2019-06-05")) %>%
  mutate(date.time.dt=mdy_hm(date.time), date.time.dt.p1=date.time.dt + dminutes(1)) %>% 
  arrange(date.time.dt.p1) %>% 
  rename(date.time.CWCR=date.time, date.time.dt.CWCR=date.time.dt, date.time.dt.p1.CWCR = date.time.dt.p1) %>% 
  select(-date) %>% 
  mutate(n=seq(1, nrow(.), by=1))

WS09_data_2 <- WS09_data %>% rename(date.time.09=date.time) %>%
  mutate(date.time.dt.09=mdy_hm(date.time.09)) %>% select(-date)

both3 <- full_join(CWCR_data_2, WS09_data_2, by="n") 

#this line below shows where the CWCR logger starts being 6 minutes ahead
both3 %>% filter((date.time.dt.p1.CWCR==date.time.dt.09)==F) %>% head()

#deleting those observations where CWCR logger is 6 minutes ahead (for consistency)
reg_data<-both3[which(both3$n<17420),]

#REGRESSION!
mod.1<-lm(temp.celsius.CWCR~temp.celsius.WS09, data=reg_data)
summary(mod.1)
#regression equation - predict CWCR: TempCWCR = 0.9644186(TempWS09) + 0.7093227

mod.2<-lm(temp.celsius.WS09~temp.celsius.CWCR, data = reg_data)
summary(mod.2)
#regression equation - predict WS09: TempWS09 = 1.0332395(TempCWCR) - 0.6855092
#both R^2s are 0.9965
