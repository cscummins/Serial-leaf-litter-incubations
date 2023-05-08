#Getting Deployment-Level Data from the daily data for temp - Serial Incubations YEAR 2 - CSC 18 Nov 2020

#YEAR 2

#deployments csv
dates<-read.csv("SI_yr2_deployments_UPD23Feb23.csv")
dates <- dates %>% mutate(date_dep=mdy(date_dep))
dates <- dates %>% mutate(date_coll=mdy(date_coll))


#read in the yr2 temp data
all_temp_YR2<-read.csv("landscape_dailytemp_masterlong_SIyr2.csv", header = TRUE)
all_temp_YR2 <- all_temp_YR2 %>% mutate(date=ymd(date))


streams<-levels(as.factor(all_temp_YR2$stream))

all<-data.frame()

for (i in 1:length(streams)){
  
  stream<-streams[i]
  data1<-all_temp_YR2[all_temp_YR2$stream==streams[i],] 
  
  for (j in 1:(length(dates$date_dep))){
    start_date<-dates$date_dep[j]
    end_date<-dates$date_coll[j]
    
    subset_data_2<-data1[data1$date>=start_date,]
    subset_data_1<-subset_data_2[subset_data_2$date<=end_date,]
    
    mean_temp<-mean(subset_data_1$temp.celsius,na.rm=TRUE)
    sd_temp<-sd(subset_data_1$temp.celsius,na.rm=TRUE)
    days_in_record<-length(subset_data_1$temp.celsius)
    
    
    returned_data<-data.frame(stream,start_date,end_date,mean_temp,sd_temp,deployment=j,days_in_record)
    all<-rbind(all,returned_data)
    
    
  }
  
  
  
  
}


##################################################
write.csv(all, "landscape_deployment_temp_masterlong_SIyr2_UPD23Feb23.csv")




