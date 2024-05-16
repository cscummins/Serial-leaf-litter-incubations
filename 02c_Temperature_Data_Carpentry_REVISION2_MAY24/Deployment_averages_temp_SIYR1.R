#Getting Deployment-Level Data from the daily data for temp - Serial Incubations YEAR 1 - CSC 18 Nov 2020

#YEAR 1

#deployments csv
dates<-read.csv("SI_yr1_deployments.csv")
dates <- dates %>% mutate(date_dep=mdy(date_dep))
dates <- dates %>% mutate(date_coll=mdy(date_coll))


#read in the yr1 temp data
all_temp_YR1<-read.csv("landscape_dailytemp_masterlong_SIyr1.1Feb24.csv", header = TRUE)
all_temp_YR1 <- all_temp_YR1 %>% mutate(date=ymd(date))

streams<-levels(as.factor(all_temp_YR1$stream))

all<-data.frame()

for (i in 1:length(streams)){
  
  stream<-streams[i]
  data1<-all_temp_YR1[all_temp_YR1$stream==streams[i],] 
  
  for (j in 1:(length(dates$date_dep))){
    start_date<-dates$date_dep[j]
    end_date<-dates$date_coll[j]
    
    subset_data_2<-data1[data1$date>=start_date,]
    subset_data_1<-subset_data_2[subset_data_2$date<=end_date,]
    
    mean_temp<-mean(subset_data_1$temp.celsius,na.rm=TRUE)
    sd_temp<-sd(subset_data_1$temp.celsius,na.rm=TRUE)
    days_in_record<-length(subset_data_1$temp.celsius)
    
    
    returned_data<-data.frame(stream,start_date,end_date,mean_temp,sd_temp,deployment=dates$deployment[j],days_in_record)
    all<-rbind(all,returned_data)
    
    
  }
  
  
  
  
}

##################################################
write.csv(all, "landscape_deployment_temp_masterlong_SIyr1.1Feb24.csv")

