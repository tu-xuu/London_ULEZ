RM_method=function(pollutant,startdate,enddate,mydata,data_MET,samples_time,sitename){
  
  message(paste0("Running RM_method for site: ", sitename))
  workingDirectory <<- "./"  # Shortcut for internal path use
  model_start_time <- Sys.time()

  # Define columns used in the model
  mycols=c('date',
    "date_unix","day_julian", "weekday", "hour", 
    "temp",  "RH", "ws","wd",'ssr','tp','blh','tcc','sp')


  data_prepared <- mydata %>%
    dplyr::select(all_of(c(model_columns, pollutant))) %>%
    dplyr::rename(value = all_of(pollutant)) %>%
    dplyr::filter(!is.na(ws)) %>%
    rmw_prepare_data(na.rm = TRUE)
  
  # Print model training info
  set.seed(12345) 
  message(paste0("Building model from ", startdate, " to ", enddate))
  
  # Train the meteorologically-normalized model using random forest
  RF_model <- rmw_do_all(
    data_prepared%>%selectByDate(start=startdate,end=enddate),
    variables = c(
      "date_unix","day_julian", "weekday", "hour", 
      "temp",  "RH", "ws","wd",'ssr','tp','blh','tcc','sp'     
      ),
    n_trees = 300,
    n_samples = samples_time,
    verbose = TRUE
  )
  
  # Evaluate model performance on the test set
  rr=modStats(rmw_predict_the_test_set(model = RF_model$model,df = RF_model$observations) , 
              mod = "value", obs = "value_predict", 
              statistic = c("n", "FAC2","MB", "MGE", "NMB", "NMGE", "RMSE", "r","COE", "IOA"),
              type = "default", rank.name = NULL)[,'r']$r
  
  print(paste('Model is ready, Your model R square is:', round(rr, 2), sep=' ' ))
  
### ==== Step 2: Prepare MET Resample Base ==== ###
  
data_MET <- data_MET %>%
    mutate(
      date_unix = as.numeric(date),
      week = lubridate::week(date),
      weekday = weekdays(date, abbreviate = TRUE),
      hour = lubridate::hour(date),
      month = lubridate::month(date),
      day_julian = lubridate::yday(date)
    ) %>%
    mutate(
      weekday = recode(weekday,
                       "Mon" = 1, "Tue" = 2, "Wed" = 3, "Thu" = 4,
                       "Fri" = 5, "Sat" = 6, "Sun" = 7)
    ) %>%
    dplyr::select(all_of(model_columns))
  
  # Extract matched observation set to apply resampled weather
  re_sample_MET<-mydata%>%
    filter(date >= startdate & date <= enddate)
  # Reorder columns in re_sample_MET to match the order of data_MET
  new_names <- setdiff(names(re_sample_MET), pollutant)
  data_MET <- data_MET[, new_names]
  
  print(paste('Your ',setdiff(names(re_sample_MET), names(data_MET)), ' corresponding Met data base is ready',sep=''))
  
   ### ==== Step 3: Define Weather Generator ==== ###
  pred<- mydata %>% select(1)
  
  # Filter the dataframe to select rows within the specified date range
  pred<- pred %>%
    filter(date >= startdate & date <= enddate)
  new_met<- data_MET %>%  slice(0)
  
  ##Function to generated the new- weather from original weather
 
  new_met<-function (i) {
    hour_1 <- data_MET[i, "hour"]
    day_1 <- data_MET[i, "day_julian"] 
    if(day_1 <= 14){
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>= 365 + day_1-14 |day_julian <= day_1 +14)  %>% sample_n (1)}
    if(day_1 > 14 & day_1<352){
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>=day_1-14 & day_julian <= day_1+14)  %>% sample_n (1)}
    if(day_1 >=352) {
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>= day_1-14|day_julian <= day_1 +14-365)  %>% sample_n (1)}
    new_met<-MET_sample
    return (new_met)}
  
   ### ==== Step 4: Define Prediction Function ==== ###
  #Function to predict the concentration of a pollutant using a re-sampled weather        
  set.seed(12345)
  cols_to_replace <- c("ws", "wd", "temp", "RH", "ssr", "tp", "blh", "tcc", "sp")
  
  
  prediction <- function (n) {
    for (i in 1:n) {
      re_MET <- ldply(1:nrow(data_MET), new_met, .parallel = TRUE) # Using parallel
      re_sample_MET[, cols_to_replace] <- re_MET[, cols_to_replace] ### Replaced old MET by generated MET
      prediction_value<- rmw_predict( ### RUN Random Forest model with new MET dataset
        RF_model$model, 
        df= rmw_prepare_data(re_sample_MET, value = pollutant))
      pred<-cbind(pred,prediction_value)}
    pred}
  
   print(paste('Model is replacing, Your resample times is ',samples_time,sep=''))
   
   ### ==== Step 5: Generate Final Normalized Data ==== ###
  
   ## Randomly ran for 150 predictions. Increase the number of prediction until it is stable
   final_weather_normalised <- prediction (samples_time)  

  cols_number=samples_time+1
  final_weather_normalised$final <- apply(final_weather_normalised[,2:cols_number],1,mean, na.rm=TRUE) ### Mean value of 150 predictions
  
  ### ==== Step 6: Save Results ==== ###
  ### File names 
  filenames1=paste(workingDirectory,"WN_data_", pollutant,'_',startdate,'_',enddate,'_',sitename,".csv",sep="")
  filenames2=paste(workingDirectory,"Tuan_", pollutant,'_',startdate,'_',enddate,'_',sitename,"_results.csv",sep="")
  Rfilename=paste(workingDirectory,"Tuan_",pollutant,'_', startdate,'_',enddate,'_',sitename,"_results.Rdata",sep="")
  ### Save the prediction
  write.csv(final_weather_normalised,filenames1) ### Save all predictions
  
  # Compare observed vs normalized concentrations
  data_compare<-merge(mydata,final_weather_normalised, by="date") 
  
  data_compare <- data_compare %>% dplyr:: rename(WN_data=final,Observed_data=pollutant) %>%
    select(date,WN_data,Observed_data)
  
  p=timePlot(data_compare,pollutant=c("WN_data", "Observed_data"), 
           lwd=c(1,2), group=TRUE, lty=c(1,1),avg.time ="week",
           key.position="top",cols=c("darkgreen","firebrick4"),
           ylab=expression("Pollutant concentration"* " (" * mu * "g m" ^-3 * ")"))
  
  
  write.csv(data_compare,filenames2) 
  save.image(Rfilename)
  
  
  model_end_time <- Sys.time()
  
  # Calculate time difference
  time_used <- model_end_time - model_start_time
  message(paste0("Completed. Time used: ", time_used))
  
  return(p)
  
}
