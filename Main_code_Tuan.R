##############################################
######The main code to run Tuan Method#######
##############################################
rm(list=ls())
setwd('~/shiz-shi-aphh/Tuan_results/')

pollutant='NO2'
##### The pollutant, NO2 and PM2.5

samples_time=140
#### Resample times
startdate <- as.POSIXct('2018-03-08') # 3 years or 20181001-20191031
enddate <- as.POSIXct('2020-03-08') # 3 years or 20181001-20191031
### The model will be trained in this time period


workingDirectory<<-"./"  ### Shortcut for the working directory

##### load envirnment#####
# List of packages
packages <- c("openair", "plyr", "dplyr", "rmweather", "lubridate", "magrittr",
              "tibble", "janitor", "RCurl", "rio", "stringr")

# Function to check if a package is available and install it if necessary
check_and_load_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    if (!require(package_name, character.only = TRUE)) {
      message("Package '", package_name, "' could not be installed.")
    }
  }
}

# Check and load each package
for (package in packages) {
  check_and_load_package(package)
}

source('./Time_processing.R')
source('./Tuan_method.R')



sitenames=c('AQ_MET_traj_London_Avg2_UB'
           ) 
### Add more site names

for (sitename in sitenames) {
  setwd('~/shiz-shi-aphh/Tuan_results/')
  # print(paste(sitename,'is starting',sep=' '))
  
  dirname=paste('./',sitename,sep='')
  
  # Check if the directory exists
  if (!dir.exists(dirname)) {
    # If the directory doesn't exist, create it
    dir.create(dirname)
  } else {
    # If the directory exists, do nothing or print a message
    # You can customize this part according to your needs
    print(paste("Directory", dirname, "already exists."))
  }
  
  setwd(dirname)
  
  
  workingDirectory<<-"./"  ### Shortcut for the working directory
  
  
  
  #########
  mydata<-read.csv(paste("../",sitename,".csv",sep="")) %>%Time_processing()
  ##### The data with AQ to build a Model
  
  data_MET <-mydata%>%
    selectByDate(year=2017:2022)
  
  
  # mydata<-read.csv(paste("../",sitename,".csv",sep="")) %>%
  #   dplyr::rename(NO2=no2)%>%Time_processing
  ##### The data with AQ to build a Model
  
  # data_MET <-read.csv(paste("../",sitename,".csv",sep=""))%>%
  #   selectByDate(year=2017:2022)%>%Time_processing
  # %>%
  #   dplyr::rename(temp=air_temp) # 5 years met to replace
  
  
  ##### The data base of the All years Met data, here we replace use the 10 years Met.
  
  
  Tuan_method_result_figure=Tuan_method(
    sitename=sitename,
    pollutant=pollutant,
    startdate=startdate,
    enddate=enddate,
    mydata=mydata,
    data_MET=data_MET,
    samples_time = samples_time
  )
  ##### The function will write three files in the current working directory:
  #### 1. Rdata save the all info we need.
  #### 2. Every resample results for example, if the resample times is 150, it will contain all 150 times' result.
  #### 3. The final result which is the Observation data and final WN data.
  #### The function will output a figure at the plot pannel, which you can have a quick checl.
  
  
}


