##### Load Required Libraries #####
library(readxl)
library(dplyr)
library(augsynth)
library(openair)
library(zoo)
library(tiff)
library(purrr)
library(stringr)

##### Set Working Directory #####
setwd("WD")  # Replace with your actual project directory

##### Load Weekly-Averaged Dataset #####
alldataexp <- readRDS("ASCM_NO2.Rdata")

# Define date range for the evaluation period
s_date <- "2018-04-01"
e_date <- "2020-03-02"

# Filter data to keep only the evaluation period
alldata <- alldataexp %>%
  filter(date >= s_date & date <= e_date)

##### Define Policy Intervention Time and Target Pollutants #####
intervention <- c("04-08")   # e.g., April 8 (start of ULEZ)
yearlist <- c("2019")        # Intervention year

##### Define Pollutant and Treatment Groups #####
pollutants <- list("NO2wn")                    # Weather-normalized NO2
treatments <- list("Avg1_UT", "Avg1_UB")       # Treated sites (UT: Urban Traffic, UB: Urban Background)

##### Define Control Site Groups #####
controls <- list()

# Control group for UT
controls[[1]] <- c(
  "Aberdeen_Union_Street_Roadside", "Birmingham_A4540_Roadside",
  "Blackburn_Accrington_Road", "Bristol_Temple_Way", "Bradford_Mayo_Avenue",
  "Dumfries", "Exeter_Roadside", "Glasgow_Great_Western_Road",
  "Glasgow_High_Street", "Haringey_Roadside", "Luton_A505_Roadside",
  "Newcastle_Cradlewell_Roadside", "Swansea_Roadside", "York_Fishergate"
)

# Control group for UB
controls[[2]] <- c(
  "Birmingham_Acocks_Green", "Blackpool_Marton", "Bournemouth", "Brighton_Preston_Park",
  "Bristol_St_Paul", "Edinburgh_St_Leonards", "Glasgow_Townhead", "Hull_Freetown",
  "Leamington_Spa", "Manchester_Piccadilly", "Nottingham_Centre", "Oxford_St_Ebbes",
  "Preston", "Sheffield_Tinsley", "Wigan_Centre", "Wirral_Tranmere"
)

##### Group Structure #####
group <- list(
  poll = pollutants,
  treat = treatments,
  contr = controls
)

##### Run Augmented Synthetic Control (ASCM) #####
data_exp <- NULL  # Store results from all experiments

for (t in 1:length(group$treat)) {
  
  pollutant <- group$poll[[1]]               # Currently only one pollutant
  treated_site <- group$treat[[t]]
  control_sites <- group$contr[[t]]
  
  # Combine treated site and controls
  sites_in_group <- c(treated_site, control_sites)
  
  # Filter data for this group
  exp_data <- alldata %>% filter(site %in% sites_in_group)
  
  # Define intervention date
  intervention_date <- paste0(yearlist[1], "-", intervention[1])
  
  # Create treated indicator and unix time
  exp_data <- exp_data %>%
    mutate(
      treated = ifelse(date >= intervention_date & site == treated_site, 1, 0),
      date_unix = as.numeric(date)
    )
  
  # Run Augmented Synthetic Control Model
  HBobs <- augsynth(
    formula = as.formula(paste0(pollutant, " ~ treated")),
    unit = site,
    time = date_unix,
    data = exp_data,
    progfunc = "Ridge",
    scm = TRUE,
    fixedeff = TRUE
  )
  
  message(paste("Finished ASCM for:", treated_site, "pollutant:", pollutant))
  
  ##### Extract Results #####
  summ <- summary(HBobs, inf_type = "jackknife+")
  Pol <- summ$att %>% dplyr::rename(date = Time)
  
  # Add metadata
  Pol$city <- treated_site
  Pol$pollutant <- pollutant
  Pol$year <- yearlist[1]
  Pol$average_att <- summ$average_att$Estimate
  Pol$average_att_lower <- summ$average_att$lower_bound
  Pol$average_att_upper <- summ$average_att$upper_bound
  Pol$L2 <- summ$l2_imbalance
  Pol$Scaled_L2 <- summ$scaled_l2_imbalance
  Pol$est_bias <- format(round(mean(summ$bias_est), 3), nsmall = 3)
  Pol$improvement <- format(round((1 - summ$scaled_l2_imbalance) * 100, 1))
  
  # Append results
  data_exp <- rbind(data_exp, Pol)
}

##### Final Output #####
ASCM_NO2_UB_UT <- data_exp  # Combined results for both UT and UB groups


# write.csv(ASCM_NO2_UB_UT, "ASCM_NO2_UB_UT.csv", row.names = FALSE)
