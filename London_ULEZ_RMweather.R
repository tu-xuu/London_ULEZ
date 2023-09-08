library(RCurl)
library(usethis)
library(openair)
library(devtools)
library(plyr)
library(dplyr)
library(rmweather)
library(ranger)
library(magrittr)
library(globals)
library(future)
library(foreach)
library(iterators)
library(parallel)
library(doFuture)
library(readxl)
library(doParallel)
library(ggplot2)
library(usethis)
library(devtools)
library(augsynth)
library(colortools)
library(lubridate)
library(dplyr)
library(worldmet)
library(tibble)
library(gridExtra)
library(expss)
library(readr)


x <- getURL('https://raw.githubusercontent.com/tu-xuu/London_ULEZ/main/LondonKC1alldata.csv')
mydata <- read.csv(text = x)
mydata$date <- as.POSIXct(strptime(mydata$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
## This is an example data for weather Normalisation cantined time, AQ data, atmospheric conditions and back trajectory clusters.

## Creating some time relevent varibles
mydata$date_unix  <- as.numeric(mydata$date)
mydata$week <- week(mydata$date)
mydata$weekday <- weekdays(mydata$date, abbreviate = TRUE) 
mydata$weekday[which(mydata$weekday == "Mon")] = 1
mydata$weekday[which(mydata$weekday == "Tue")] = 2
mydata$weekday[which(mydata$weekday == "Wed")] = 3
mydata$weekday[which(mydata$weekday == "Thu")] = 4
mydata$weekday[which(mydata$weekday == "Fri")] = 5
mydata$weekday[which(mydata$weekday == "Sat")] = 6
mydata$weekday[which(mydata$weekday == "Sun")] = 7
mydata$hour <- hour(mydata$date)
mydata$month <- month(mydata$date)
mydata$day_julian <- yday(mydata$date)


# Pollutant list
pollutantlist<-list("PM2.5","PM10","SO2",'NO',"NO2","NOx","O3")
ncal=5


for (pollutant in pollutantlist){
    if(pollutant %in% colnames(mydata)){
      print(paste(pollutant,'training',sep=' '))
      Dataraw <- mydata
      Dataraw$weekday<-as.factor(Dataraw$weekday) 
      r.min <- 0.1
      perform<-matrix(data=NA,ncol=11,nrow=1)# return the model performance 
      colnames(perform)<-c("default","n", "FAC2","MB", "MGE", "NMB", "NMGE", "RMSE", "r","COE", "IOA")
      for (i in as.numeric(1:ncal)){
        set.seed(i) 
        data_prepared <- Dataraw %>% 
          filter(!is.na(ws)) %>% 
          dplyr::rename(value = pollutant) %>% 
          rmw_prepare_data(na.rm = TRUE,fraction = 0.7) # prepare the data
        set.seed(i) 
        RF_model <- rmw_do_all(
          data_prepared,
          variables = c(
            "date_unix","day_julian", "weekday", "hour", "temp",  "RH", "ws","wd","pressure",'ssr','tp','blh','tcc','sp'), #factors for random forest modeling
          variables_sample=c("temp",  "RH", "ws","wd","pressure",'ssr','tp','blh','tcc','sp'), #factors for weather replacement
          n_trees = 300,
          n_samples = 300,
          verbose = TRUE
        ) # RM weather model
        
        testing_model <- rmw_predict_the_test_set(model = RF_model$model,df = RF_model$observations) # testing whether the model is overfitting
        model_performance<-modStats(testing_model, mod = "value", obs = "value_predict", 
                                    statistic = c("n", "FAC2","MB", "MGE", "NMB", "NMGE", "RMSE", "r","COE", "IOA"),
                                    type = "default", rank.name = NULL)# return the model performance 
        
        perform<-rbind(perform,model_performance)
        if (model_performance$r > r.min){
          r.min <- model_performance$r
          RF_modelo <- RF_model} 
      } 
      save.image(file ='mydata.Rdata') 
      # The result will be saved in RF_modelo the normalisede one is 'RF_modelo$normalised' and observation is 'RF_modelo$observations$value'
      write.table(perform, file="RWPerformance.csv", sep=",", row.names=FALSE)
    }else{
      print(paste('Do not have',pollutant,sep=' '))
    }
  }    


AQ<-cbind(RF_modelo$normalised,RF_modelo$observations$value)
names(AQ)[names(AQ) == "value_predict"] <- 'Normalised_data'
names(AQ)[names(AQ) == "RF_modelo$observations$value"] <- 'Observation'
