library(lubridate)

#Summarizing flow data

#YEAR 1

#deployments csv
dates<-read.csv("SI_yr1_deployments.csv")
dates <- dates %>% mutate(date_dep=mdy(date_dep))
dates <- dates %>% mutate(date_coll=mdy(date_coll))


#read in the discharge data
discharge<-read.csv("landscape_dailydischarge_masterlong_SIyr1_UPD23Feb23.csv")
discharge <- discharge %>% mutate(date=ymd(date))

streams<-levels(as.factor(discharge$variable))

all<-data.frame()

for (i in 1:length(streams)){
  
  stream<-streams[i]
  data1<-discharge[discharge$variable==streams[i],] 
  
  for (j in 1:(length(dates$date_dep))){
    start_date<-dates$date_dep[j]
    end_date<-dates$date_coll[j]
    
    subset_data_2<-data1[data1$date>=start_date,]
    subset_data_1<-subset_data_2[subset_data_2$date<=end_date,]
    
    mean_flow<-mean(subset_data_1$discharge_ls,na.rm=TRUE)
    sd_flow<-sd(subset_data_1$discharge_ls,na.rm=TRUE)
    days_in_record<-length(subset_data_1$discharge_ls)
    
    
    returned_data<-data.frame(stream,start_date,end_date,mean_flow,sd_flow,deployment=dates$deployment[j],days_in_record)
    all<-rbind(all,returned_data)
    
    
  }

  
  
  
}

write.csv(all, "landscape_deployment_discharge_masterlong_SIyr1_UPD23Feb23.csv")
