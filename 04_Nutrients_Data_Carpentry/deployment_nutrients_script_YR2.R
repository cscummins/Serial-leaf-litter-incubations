#Getting Deployment-Level Data from the weekly nutrient data - Serial Incubations - CSC 8 Dec 2020

#UPDATED 12 January 2021 with new landscape data files

#YEAR 2

#deployments csv
dates<-read.csv("SI_yr2_deployments_UPD23Feb23.csv")
dates <- dates %>% mutate(date_dep=mdy(date_dep))
dates <- dates %>% mutate(date_coll=mdy(date_coll))


#read in the yr2 nutrients data
all_nuts_YR2<-read.csv("all_nutrients_master_CSC_SIYR2.csv", header = TRUE)
all_nuts_YR2 <- all_nuts_YR2 %>% mutate(date=ymd(date))

streams<-levels(as.factor(all_nuts_YR2$stream))

all<-data.frame()

for (i in 1:length(streams)){
  
  stream<-streams[i]
  data1<-all_nuts_YR2[all_nuts_YR2$stream==streams[i],] 
  
  for (j in 1:(length(dates$date_dep))){
    start_date<-dates$date_dep[j]
    end_date<-dates$date_coll[j]
    
    subset_data_2<-data1[data1$date>=start_date,]
    subset_data_1<-subset_data_2[subset_data_2$date<=end_date,]
    
    mean_nh4.mgl<-mean(subset_data_1$nh4.mgl,na.rm=TRUE)
    sd_nh4.mgl<-sd(subset_data_1$nh4.mgl,na.rm=TRUE)
    N_nh4.mgl<-length(na.omit(subset_data_1$nh4.mgl))
    
    mean_no3.mgl<-mean(subset_data_1$no3.mgl,na.rm=TRUE)
    sd_no3.mgl<-sd(subset_data_1$no3.mgl,na.rm=TRUE)
    N_no3.mgl<-length(na.omit(subset_data_1$no3.mgl))
    
    mean_po4.mgl<-mean(subset_data_1$po4.mgl,na.rm=TRUE)
    sd_po4.mgl<-sd(subset_data_1$po4.mgl,na.rm=TRUE)
    N_po4.mgl<-length(na.omit(subset_data_1$po4.mgl))
    
    mean_din.mgl<-mean(subset_data_1$din.mgl,na.rm=TRUE)
    sd_din.mgl<-sd(subset_data_1$din.mgl,na.rm=TRUE)
    N_din.mgl<-length(na.omit(subset_data_1$din.mgl))
    
    
    returned_data<-data.frame(stream, start_date, end_date, mean_nh4.mgl, sd_nh4.mgl, N_nh4.mgl, mean_no3.mgl, sd_no3.mgl, N_no3.mgl, mean_po4.mgl, sd_po4.mgl, N_po4.mgl, 
                              mean_din.mgl, sd_din.mgl, N_din.mgl, deployment=j)
    all<-rbind(all,returned_data)
    
    
  }
  
  
  
  
}


#####

write.csv(all, "deployment_nuts_masterlong_SIyr2_UPD23Feb23.csv")

