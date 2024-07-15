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

source('./Time_processing.R') # Modified the date column
source('./Tuan_method.R') # The function to run the Tuan's method



sitenames=c('AQ_MET_traj_London_Avg1_UT'
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

  ### Get ready the MET database
  data_MET <-mydata%>%
    selectByDate(year=2017:2022)
  
  
  
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
  #### 1. Rdata saves all the info we need.
  #### 2. For every resample result, for example, if the resample times is 150, it will contain all 150 times results.
  #### 3. The final result is the Observation data and final WN data.
  #### The function will output a figure at the plot panel, which you can have a quick check.
  
  
}


