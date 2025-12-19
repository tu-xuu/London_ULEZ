##############################################
######The main code to run WN#################
##############################################

#rm(list=ls())
setwd('YOUR WD')

##### Load Environment #####

# List of required R packages
packages <- c("openair", "plyr", "dplyr", "rmweather", "lubridate", "magrittr",
              "tibble", "janitor", "RCurl", "rio", "stringr")

# Function to check if a package is installed; install it if not, then load it
check_and_load_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    if (!require(package_name, character.only = TRUE)) {
      message("Package '", package_name, "' could not be installed.")
    }
  }
}

# Load all required packages
for (package in packages) {
  check_and_load_package(package)
}

# Load custom functions for time processing and normalization method
source("./Time_processing.R") 
source("./RM_code.R") 

##### Parameter Settings #####

# Specify the target pollutant: options include "NO2" or "PM2.5"... (case-sensitive)
pollutant <- "NO2"

# Number of resampling iterations used in the meteorological normalization process
samples_time <- 150

# Define the time period for model training
startdate <- as.POSIXct("2018-03-08") 
enddate   <- as.POSIXct("2020-03-08")


# List of site names to be analyzed; each name should match the corresponding CSV file name
sitenames <- c("Avg1_UT") # Sample data
# Add more site names as needed

# Load meteorological data and apply time preprocessing
# Customize the time window (e.g., 5 years, 10 years) for resampling as needed
data_MET <- readRDS(London_MET_2000_2024.Rdata)

#### Loop through each site and run the meteorological normalization pipeline ####

for (sitename in sitenames) {
  
  # Set the base working directory — replace 'YOUR WD' with your actual project path or use `here::here()` for safety
  setwd("YOUR WD")  
  
  # Define a folder for the current site
  dirname <- paste0("./", sitename)
  
  # Create the directory if it doesn't exist
  if (!dir.exists(dirname)) {
    dir.create(dirname)
  } else {
    message(paste("Directory", dirname, "already exists."))
  }
  
  # Switch to the site's working directory
  setwd(dirname)
  workingDirectory <<- "./"  # Shortcut for internal function use
  
  # Load air quality data for the site and preprocess it
  mydata <- read.csv(paste0("../", sitename, ".csv")) %>% Time_processing()%>%left_join(data_MET,by='date')
  
  # Run the RM (meteorological normalization) method
  WN_result_figure <- RM_method(
    sitename      = sitename,
    pollutant     = pollutant,
    startdate     = startdate,
    enddate       = enddate,
    mydata        = mydata,
    data_MET      = data_MET,
    samples_time  = samples_time
  )

  # RM_method will output:
  # 1. An `.RData` file storing all model objects and results
  # 2. A `.csv` file containing all resampling results (e.g. 150 iterations)
  # 3. A `.csv` file with observed vs normalized pollutant concentrations
  # 4. A visualization plot rendered in the R plot window
  
}
