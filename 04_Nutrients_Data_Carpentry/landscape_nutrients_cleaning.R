#Cleaning up the landscape nutrient data and getting landscape files for SI YR1 and YR2 - CSC 12 January 2021
#read in the master data, date stuff, change stream names

library(dplyr)
library(lubridate)

landscape_nutrients_master_11jan2021 <- read.csv("landscape_nutrients_master_11jan2021.csv")

landscape_nutrients_master_11jan2021 <- landscape_nutrients_master_11jan2021 %>% mutate(date=mdy(date))

landscape_nutrients_master_11jan2021[landscape_nutrients_master_11jan2021$stream == "cwcr", "stream"] <- "CWCR"
landscape_nutrients_master_11jan2021[landscape_nutrients_master_11jan2021$stream == "hncr", "stream"] <- "HNCR"
landscape_nutrients_master_11jan2021[landscape_nutrients_master_11jan2021$stream == "lccr", "stream"] <- "LCCR"
landscape_nutrients_master_11jan2021[landscape_nutrients_master_11jan2021$stream == "ushf", "stream"] <- "USHF"

#get subsets of these data for the 2 different years, delete unnecessary columns
landscape_nutrients_master_CSC_SIYR1 <- landscape_nutrients_master_11jan2021[which(landscape_nutrients_master_11jan2021$date>"2017-09-19"),]
landscape_nutrients_master_CSC_SIYR1 <- landscape_nutrients_master_CSC_SIYR1[which(landscape_nutrients_master_CSC_SIYR1$date<"2018-10-02"),]

landscape_nutrients_master_CSC_SIYR2 <- landscape_nutrients_master_11jan2021[which(landscape_nutrients_master_11jan2021$date>"2018-08-31"),]
landscape_nutrients_master_CSC_SIYR2 <- landscape_nutrients_master_CSC_SIYR2[which(landscape_nutrients_master_CSC_SIYR2$date<"2019-08-30"),]

landscape_nutrients_master_CSC_SIYR1 <- landscape_nutrients_master_CSC_SIYR1[c("stream","date","nh4.mgl","no3.mgl", "po4.mgl", "din.mgl")]
landscape_nutrients_master_CSC_SIYR2 <- landscape_nutrients_master_CSC_SIYR2[c("stream","date","nh4.mgl","no3.mgl", "po4.mgl", "din.mgl")]

#these files replace the old landscape master files
write.csv(landscape_nutrients_master_CSC_SIYR1, "landscape_nutrients_master_CSC_SIYR1.csv")
write.csv(landscape_nutrients_master_CSC_SIYR2, "landscape_nutrients_master_CSC_SIYR2.csv")
