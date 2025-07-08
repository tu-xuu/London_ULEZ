# rm(list=ls())
library(readxl)
library(dplyr)
library(augsynth)
library(openair)
library(zoo)
library(ggplot2)
library(gridExtra)
library(forestplot)
library(ggthemes)
library(tiff)
library(purrr)
library(stringr)
setwd('~/Desktop/Review_ULEZ/Final_ULEZ_review_0822/Control_ULEZ_P1/NO2/')


csv_files <- list.files(pattern = "\\.csv$")

csv_list <- lapply(csv_files, read.csv)

processed_data_list <- list()

# Loop through each file, read it, process it, and store it in the list
for (csv_file in csv_files) {
  # Read the CSV file into a data frame
  data <- read.csv(csv_file)
  
  # Apply the Time_processing function to the data frame
  processed_data <- Time_processing(data)%>%timeAverage(avg.time = 'week',type = 'site')
  
  # Add the processed data frame to the list
  processed_data_list[[csv_file]] <- processed_data
}


control_data <- do.call(rbind, processed_data_list)

control_data=control_data%>%dplyr::select(-X)

# control$site <- gsub("_$", "", control$site)
names(control_data)[names(control_data) == "WN_data"] <- "NO2wn"
names(control_data)[names(control_data) == "Observed_data"] <- "NO2"

Control_2year_covid=control_data


treat1=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Background_A1_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg1_UB')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)
treat2=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Background_A2_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg2_UB')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)

treat3=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Background_A3_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg3_UB')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)

treat4=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Traffic_A1_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg1_UT')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)
treat5=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Traffic_A2_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg2_UT')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)
treat6=read.csv('../../Treat_ULEZ_P1/Tuan_NO2_2018-03-08_2020-03-08_Urban Traffic_A3_results.csv')%>% Time_processing()%>%
  timeAverage(avg.time = 'week')%>%
  mutate(site='Avg3_UT')%>%dplyr::select(-X)%>%dplyr::rename(NO2wn=WN_data,NO2=Observed_data)



treatdata_2year_Covid <- do.call(rbind, list(treat1, treat2, treat3, treat4, treat5, treat6))

alldata_2year_Covid_NO2<-rbind(treatdata_2year_Covid,Control_2year_covid)



# treatUT2plot=left_join(treat5%>%dplyr::rename(`NO2_WN_201803-202003`=NO2wn),treatUT1plot,by='date')
# timePlot(treatUT2plot%>%selectByDate(start='01/04/2017',end='28/02/2020'),pollutant = c( 'NO2_WN_201803-202003',
#   'NO2_WN_201801-201912','NO2_WN_201810-201910',
#   # 'Tuan_201810_201910_5ys_150times', 
#   "NO2wn_Yuqing_method","NO2wn_GBM_201810-201910",'NO2wn_RF_201810-201910'
# ),lwd=2,key.position='right',key.columns=1,
# ref.x = list(
#   v = c(as.POSIXct("2019-04-09")
#         # , as.POSIXct('2019-07-22'),
#         # as.POSIXct('2019-05-22'),
#         # as.POSIXct("2019-10-01"), 
#         # as.POSIXct("2018-10-01")
#   ), 
#   lty = c(2, 2,2)
# ),
# ylab='NO2wn',cols = c('darkgreen','firebrick'),
# group = TRUE)


UBcodes<-NULL
UTcodes<-NULL
UTcodes[[1]]<-c('Avg1_UT','Avg2_UT','Avg3_UT')
UBcodes[[1]]<-c('Avg1_UB','Avg2_UB','Avg3_UB')
# UTcodes[[2]]<- c( "CE1", "CE2", "CE3", 
#                   "CT2", "CT4", "CT6", "CT8", "CTA", 
#                   "GV2",  "HK6", "IM1","MR8", "MR9", "MS4", "MS5", "MY1", 
#                   "MY4", "NB1",  "SK8", 
#                   "WM6", 
#                   "WMB", "WMC")
# 
# UTcodes[[3]]<- c("BT6", "BT8", "CA1", "CD1", 
#                  "EA6", "EA8", 
#                  "EI1", "GN5", "GN6", "GR7", "GR8", "GR9", "GV1", 
#                  "HG1",  "IS2", "LB4", "LW2", 
#                  "LW4", "LW6", 
#                  "NM2", "RB4", "RI1", "SK5", 
#                  "SK9", "SKA", "SKB", "SKC", "TH2", "TH4", "TL4", 
#                  "TL5", "TL6", "WA7", "WA8", "WAA", "WAC",  
#                  "WMD")
# 
# 
# UTcodes[[4]]<- c( "BT4", "BY7", "CR5", "CR7", "CR9", "EN4", "EN5", "GB0", "GB6", 
#                   "GN0", "GN3", "GN4", "HR2", "HV1", "HV3", "KT4", "KT5", "KT6", 
#                   "ME2", "ME9", "MW1", "MW2", "RG7", "ST4", "ST6", "ST9", "TK3", 
#                   "TK8", "TK9", "WAB", "ZR2", "ZR4", "ZV2")

UTcodes[[5]]  <- c( "Aberdeen_Union_Street_Roadside",
                    'Armagh_Roadside',
                    'Birmingham_A4540_Roadside',
                    "Blackburn_Accrington_Road", 
                    'Bristol_Temple_Way',
                    'Bury_Whitefield_Roadside',
                    "Bradford_Mayo_Avenue", 
                    'Cambridge_Roadside',
                    'Chepstow_A48',
                    'Doncaster_A630_Cleveland_Street',
                    'Dumfries',
                    'Exeter_Roadside',
                    'Glasgow_Great_Western_Road',
                    'Glasgow_High_Street',
                    'Glasgow_Kerbside',
                    'Haringey_Roadside',
                    'Leicester_A594_Roadside',
                    'Luton_A505_Roadside',
                    'Newcastle_Cradlewell_Roadside',
                    'Swansea_Roadside',
                    'York_Fishergate')
UBcodes[[5]]<-c(
  'Birmingham_Acocks_Green',
  "Aberdeen", 
  "Barnsley_Gawber", 
  "Belfast_Centre", 
  "Birmingham_Tyburn", 
  
  "Blackpool_Marton", 
  "Bournemouth", 
  
  "Brighton_Preston_Park", 
  "Bristol_St_Paul", 
  "Canterbury", 
  "Cardiff_Centre",
  "Edinburgh_St_Leonards",
  "Glasgow_Townhead", 
  "Hull_Freetown", 
  "Leamington_Spa", 
  "Leeds_Centre", 
  "Leicester_Centre", 
  "Leicester_University", 
  "Manchester_Piccadilly",
  "Newcastle_Centre", 
  'Nottingham_Centre',
  "Oxford_St_Ebbes", 
  "Plymouth_Centre", 
  "Preston", 
  "Reading_New_Town", 
  "Sheffield_Tinsley", 
  "Southampton_Centre",
  "Southampton_Centre", 
  "Stoke_on_Trent_Centre",
  "Sunderland_Silksworth", 
  "Wigan_Centre", 
  "Wirral_Tranmere")





# UBcodes[[2]]<-c("BL0",  "CLL2", "CT3", "CT9", 
#                 "HORS", 
#                 "SK6", "WM0", "WM5")
# 
# UBcodes[[3]]<-c( "BT5",   
#                  "CW3", "EI3", "EI8", "HG4", 
#                  "HP1", "HP5", "HPY", "HPZ", "IS6", 
#                  "KC1", "KF1",  "LW5", "NM3", 
#                  "WA2", "WA9")
# UBcodes[[4]]<-c("BQ7", "CR8", "EN7", "HR1", "LB6", "LH0", 
#                 "MW4", "RB7", "RD0", "TED2", "TK1", "ZV1")



# startdate<-c("2018-10-01")
# enddate<-c("2019-10-01")
intervention<-c("04-08")
yearlist<-c("2019")
# s_date<-startdate[1]
# e_date<-enddate[1]
it_day<-intervention[1]
year_s<-yearlist[1]

treatment_UB<-NULL
treatment_UT<-NULL## treatment list
pollut<-NULL ## pollutant list
control_UB<-NULL ## control list
control_UT<-NULL
pollut[[1]]<-c("NO2wn") 
# pollut[[1]]<-c("NO2") 
##### UT ######
s_date="2018-04-01"
e_date="2020-03-02"
# alldata<-rbind(treatdata,control_data)
alldata<-alldata_2year_Covid_NO2%>% filter(date>=s_date & date <= e_date) 
# datelabel=data.frame(date=unique(alldata$date))
# datelabel$date=as.Date(datelabel$date)
# treatment_UT[[1]]<-c('Tuan_201810_201910_20years','Tuan_201810_201910_5years','Tuan_201810_202002_20years','Avg1_UT')
treatment_UT[[1]]<-c('Avg1_UT','Avg3_UT','Avg2_UT')
# treatment_UT[[1]]<-c('Avg2_UT')
# treatment_UT[[1]]<-c('Avg1_UT','Avg2_UT','Avg3_UT','Avg2&3_UT')
# treatment_UT[[2]]<-c('CT4','CT6','HK6','MY1','NB1','WM6','WMB','WMC')
# treatment_UT[[3]] <- intersect(UTcodes[[3]], alldata$site) %>%
# .[!. %in% c('EA6','CA1','LB4','GV1','WA7','WAA','WAC'
#             
# )]
# treatment_UT[[4]] <- intersect(UTcodes[[4]], alldata$site)%>%
# .[!. %in% c('RG7','TK8','ZR4','BY7')]

control_UT[[1]]<-intersect(UTcodes[[5]], alldata$site) %>%
  .[!. %in% c(
    # 'Dumfries',
    # 'Swansea_Roadside',
          'Cambridge_Roadside'
    # "Aberdeen_Union_Street_Roadside", #1
    # "Birmingham_A4540_Roadside"     
     # "Blackburn_Accrington_Road"      
     # "Bristol_Temple_Way"            
       # "Bradford_Mayo_Avenue"    #1       
    # "Exeter_Roadside"               
    # "Glasgow_Great_Western_Road"    
    # "Glasgow_High_Street"           
    # "Haringey_Roadside"   #1         
    # "Luton_A505_Roadside"           
    # "Newcastle_Cradlewell_Roadside" 
    # "York_Fishergate"      
    
  )]

siteplot=c(treatment_UT[[1]],control_UT[[1]])
filtered_df2 <-alldata_2year_Covid_NO2[alldata_2year_Covid_NO2$site %in% siteplot, ]
it_date<-paste(year_s,"-",it_day,sep='')
Thedate=as.numeric(as.Date(it_date))
filtered_df2$date=as.Date(filtered_df2$date)
filtered_df2=filtered_df2%>%
  mutate(site = case_when(
    site == "Avg1_UT" ~ "Central_UT",
    site == "Avg2_UT" ~ "Inner_UT",
    site == "Avg3_UT" ~ "Outer_UT",
    TRUE ~ site
  ))


# pdf("~/Desktop/ULEZ_figures_tables/NO2_UT_WN_ULEZ_1.pdf", width = 14, height = 6)
# ggplot(filtered_df2, aes(x = date, y = NO2wn, color = site)) +
#   geom_line() +
#   # geom_vline(aes(xintercept = Thedate), colour = 'red', linetype = 'dashed') +
#   # geom_vline(aes(xintercept = as.numeric(as.Date('2019-10-01'))), colour = 'darkgreen', linetype = 'dashed') +
#   # geom_vline(aes(xintercept = as.numeric(as.Date('2019-05-22'))), colour = 'red', linetype = 'dashed') +
#   # labs(title = "NO2 Levels for Different Sites (UB)", x = "Date", y = "NO2") +
#   theme_minimal()+
#   theme(
#     legend.position = "top",  # Move the legend to the top
#     legend.box = "horizontal",  # Arrange the legend items horizontally
#     legend.key.size = unit(0.5, "cm"),  # Set the size of the legend key
#     legend.text = element_text(size = 10),  # Set the size of the legend text
#     legend.title = element_blank(),
#     legend.spacing.x = unit(0.1, "cm") # Remove the legend title
#   )+
#   guides(color = guide_legend(ncol  = 8))
# dev.off()

alldataexp<- subset(alldata, (site %in% siteplot ))

alldataexp$NO2wn<-na.spline(alldataexp$NO2wn)
alldataexp$NO2<-na.spline(alldataexp$NO2)
alldataexp$date=as.Date(alldataexp$date)
# alldataexp=split(alldataexp,alldataexp$site)

# lengths <- sapply(alldataexp, function(df) nrow(df))

# # Find the maximum length
# max_length <- max(lengths)

# # Identify data frames with a length different from the maximum length
# index_to_drop <- which(lengths != max_length)

# # Drop the identified data frames from the list
# alldataexp <- alldataexp[-index_to_drop]
# alldataexp <- do.call(rbind, alldataexp)

group<-list(poll=pollut,treat=treatment_UT[1],contr=control_UT)
data_exp <- NULL


for (t in 1:length(group$treat[[1]])){
  y<-group$poll[[1]] 
  controls<-group$contr[[1]]
  treat_<-group$treat[[1]][t]
  controls<-append(treat_,controls)
  myexp <- alldataexp %>% filter(site %in% controls)
  it_date<-paste(year_s,"-",it_day,sep='')
  myexp$treated <- "0"
  myexp$treated[myexp$date>=it_date & myexp$site == treat_]<-1
  myexp$date_unix  <- as.numeric(myexp$date)
  HBobs<- augsynth(y ~ treated, site, date_unix, myexp, progfunc="Ridge", scm=T,fixedeff=T)
  print(paste("end of augsyth",treat_,y))
  summ=summary(HBobs,inf_type = "jackknife+")
  Pol<-summ$att %>% dplyr::rename(date= Time)
  Pol$city<-treat_
  Pol$pollutant<-y
  Pol$year <- year_s
  Pol$average_att <- summ$average_att$Estimate
  Pol$average_att_lower <- summ$average_att$lower_bound
  Pol$average_att_upper <- summ$average_att$upper_bound
  Pol$L2 <- summ$l2_imbalance
  Pol$Scaled_L2 <- summ$scaled_l2_imbalance
  Pol$est_bias<- format(round(mean(summ$bias_est), 3),nsmall=3)
  Pol$improvement<- format(round(1 - summ$scaled_l2_imbalance,3)*100)
  data_exp<-rbind(data_exp,Pol)
  
}  



# data_exp=data_exp%>%selectByDate(end='22/07/2019')
# write.csv(data_exp,'NO2_UT_2019_all.csv',row.names = FALSE)
data_exp$date<-as.Date(data_exp$date)

data_exp <- data_exp %>%
  mutate(area = case_when(
    city == "Avg1_UT" ~ "Central_UT",
    city == "Avg2_UT" ~ "Inner_UT",
    city == "Avg3_UT" ~ "Outer_UT"
  ))

NO2UT=data_exp

mylist<- split(data_exp,data_exp$city)
# Apply left_join to each element in the list
# mylist <- map(mylist, ~left_join( datelabel,.x, by = "date"))


Thedate=as.numeric(as.Date(it_date))
aa=list()
for (i in 1:length(mylist)) {
  aa[[i]] <- ggplot(mylist[[i]], aes(x = date, Estimate)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
    ylim(-20, 15) +
    xlab("time") + ylab("NO2 (ug/m3)") +
    ggtitle(mylist[[i]]$city[1]) +
    geom_vline(aes(xintercept = Thedate), colour = 'red', linetype = 'dashed') +
    geom_vline(aes(xintercept = as.numeric(as.Date('2019-05-22'))), colour = 'blue', linetype = 'dashed') +
    geom_vline(aes(xintercept = as.numeric(as.Date('2019-10-01'))), colour = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line() +
    scale_x_date(breaks = pretty(mylist[[i]]$date, n = 3), date_labels = "%b-%y")
  
  # Set the title color
  
}
plots <- lapply(aa[1:4], function(plot) {
  plot
})
grid.arrange(grobs = plots[1:4], nrow = 1, ncol = 4,layout_matrix = rbind(c(1, 2, 3, 4)
)
)
# grid.arrange(grobs = plots, nrow = 8, ncol = 4
# )

##########UB

s_date="2018-04-01"
alldata<-alldata_2year_Covid_NO2%>% filter(date>=s_date & date <= e_date) 
alldata$date<-as.Date(alldata$date)

treatment_UB[[1]]<-c('Avg1_UB','Avg2_UB','Avg3_UB')

control_UB[[1]]<-intersect(UBcodes[[5]], alldata$site) %>%
  .[!. %in% c(    
    # "Blackpool_Marton",  #1 
   
    "Newcastle_Centre" ,
    
     # "Birmingham_Acocks_Green"
     "Barnsley_Gawber",        #2
   
    
    #  "Bournemouth"            
    #  "Brighton_Preston_Park"  
    # "Bristol_St_Paul"       
     "Canterbury" ,            
  
    # "Edinburgh_St_Leonards",
    #  "Glasgow_Townhead"
    #  "Hull_Freetown",
    # "Leamington_Spa",
     "Leicester_University",
    # "Manchester_Piccadilly",
    # 
    # "Nottingham_Centre"
    # "Oxford_St_Ebbes"   , #2
    #    "Preston" ,
    # 
    #  "Sheffield_Tinsley",
     "Southampton_Centre" #2
    #  "Wigan_Centre"   
    # "Wirral_Tranmere"
  )]




siteplot=c(treatment_UB[[1]],control_UB[[1]])
filtered_df2 <- alldata_2year_Covid_NO2[alldata_2year_Covid_NO2$site %in% siteplot, ]
it_date<-paste(year_s,"-",it_day,sep='')
Thedate=as.numeric(as.Date(it_date))
filtered_df2$date=as.Date(filtered_df2$date)

filtered_df2=filtered_df2%>%
  mutate(site = case_when(
    site == "Avg1_UB" ~ "Central_UB",
    site == "Avg2_UB" ~ "Inner_UB",
    site == "Avg3_UB" ~ "Outer_UB",
    TRUE ~ site
  ))

# pdf("~/Desktop/ULEZ_figures_tables/NO2_UB_WN_ULEZ_1.pdf", width = 14, height = 6)
# ggplot(filtered_df2, aes(x = date, y = NO2wn, color = site)) +
#   geom_line() +
#   # geom_vline(aes(xintercept = Thedate), colour = 'red', linetype = 'dashed') +
#   # geom_vline(aes(xintercept = as.numeric(as.Date('2019-10-01'))), colour = 'darkgreen', linetype = 'dashed') +
#   # geom_vline(aes(xintercept = as.numeric(as.Date('2019-05-22'))), colour = 'red', linetype = 'dashed') +
#   # labs(title = "NO2 Levels for Different Sites (UB)", x = "Date", y = "NO2") +
#   theme_minimal()+
#   theme(
#     legend.position = "top",  # Move the legend to the top
#     legend.box = "horizontal",  # Arrange the legend items horizontally
#     legend.key.size = unit(0.5, "cm"),  # Set the size of the legend key
#     legend.text = element_text(size = 10),  # Set the size of the legend text
#     legend.title = element_blank(),
#     legend.spacing.x = unit(0.1, "cm") # Remove the legend title
#   )+
#   guides(color = guide_legend(ncol  = 8))
# dev.off()

alldataexp<- subset(alldata, (site %in% siteplot ))
alldataexp$NO2wn<-na.spline(alldataexp$NO2wn)
alldataexp$NO2<-na.spline(alldataexp$NO2)

group<-list(poll=pollut,treat=treatment_UB,contr=control_UB)
data_exp <- NULL
########## ASCM #########################################################################
#######################################################################################
for (t in 1:length(group$treat[[1]])){
  y<-group$poll[[1]] 
  controls<-group$contr[[1]]
  treat_<-group$treat[[1]][t]
  controls<-append(treat_,controls)
  myexp <- alldataexp %>% filter(site %in% controls)
  it_date<-paste(year_s,"-",it_day,sep='')
  myexp$treated <- "0"
  myexp$treated[myexp$date>=it_date & myexp$site == treat_]<-1
  myexp$date_unix  <- as.numeric(myexp$date)
  HBobs<- augsynth(y ~ treated, site, date_unix, myexp, progfunc="Ridge", scm=T,fixedeff=T)
  print(paste("end of augsyth",treat_,y))
  summ=summary(HBobs,inf_type = "jackknife+")
  Pol<-summ$att %>% dplyr::rename(date= Time)
  Pol$city<-treat_
  Pol$pollutant<-y
  Pol$year <- year_s
  Pol$average_att <- summ$average_att$Estimate
  Pol$average_att_lower <- summ$average_att$lower_bound
  Pol$average_att_upper <- summ$average_att$upper_bound
  Pol$L2 <- summ$l2_imbalance
  Pol$Scaled_L2 <- summ$scaled_l2_imbalance
  Pol$est_bias<- format(round(mean(summ$bias_est), 3),nsmall=3)
  Pol$improvement<- format(round(1 - summ$scaled_l2_imbalance,3)*100)
  data_exp<-rbind(data_exp,Pol)
  
}

data_exp$date<-as.Date(data_exp$date)
data_exp <- data_exp %>%
  mutate(area = case_when(
    city == "Avg1_UB" ~ "Central_UB",
    city == "Avg2_UB" ~ "Inner_UB",
    city == "Avg3_UB" ~ "Outer_UB"
  ))
NO2UB=data_exp
# write.csv(data_exp,'NO2_UB_2019_all.csv',row.names = FALSE)
mylist<- split(data_exp,data_exp$city)
# mylist <- map(mylist, ~left_join( datelabel,.x, by = "date"))
Thedate=as.numeric(as.Date(it_date))
aa=list()
for (i in c(1:3)) {
  aa[[i]] <- ggplot(mylist[[i]], aes(x = date, Estimate)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
    ylim(-10, 10) +
    xlab("time") + ylab("NO2 (ug/m3)") +
    ggtitle(mylist[[i]]$city[1]) +
    geom_vline(aes(xintercept = Thedate), colour = 'red', linetype = 'dashed') +
    geom_vline(aes(xintercept = as.numeric(as.Date('2019-05-22'))), colour = 'blue', linetype = 'dashed') +
    geom_vline(aes(xintercept = as.numeric(as.Date('2019-10-01'))), colour = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line() +
    scale_x_date(breaks = pretty(mylist[[i]]$date, n = 3), date_labels = "%b-%y")
  
  # Set the title color
  
}
plots <- lapply(aa[1:length(mylist)], function(plot) {
  plot
})
grid.arrange(grobs = plots, nrow = 1, ncol = 4,layout_matrix = rbind(c(1, 2, 3)
)
)





########### FigS1-S6
treatment_UT[[1]]<-c('Avg1_UT','Avg3_UT','Avg2_UT')
treatment_UB[[1]]<-c('Avg1_UB','Avg2_UB','Avg3_UB')
siteplot=c(treatment_UT[[1]],treatment_UB[[1]])
filtered_df2 <-alldata_2year_Covid_NO2[alldata_2year_Covid_NO2$site %in% siteplot, ]
filtered_df2$date=as.Date(filtered_df2$date)
filtered_df2=filtered_df2%>%
  mutate(site = case_when(
    site == "Avg1_UT" ~ "Central_UT",
    site == "Avg2_UT" ~ "Inner_UT",
    site == "Avg3_UT" ~ "Outer_UT",
    site == "Avg1_UB" ~ "Central_UB",
    site == "Avg2_UB" ~ "Inner_UB",
    site == "Avg3_UB" ~ "Outer_UB",
    TRUE ~ site
  ))


pdf("~/Desktop/ULEZ_figures_tables/Figure_S1.pdf", width = 14, height = 6)

ggplot(filtered_df2, aes(x = date, y = NO2wn, color = site)) +
  geom_line() +
  labs(
    x = 'Date',
    y = expression("NO"[2] ~ "(µg/m"^3*")")
  ) +
  guides(color = guide_legend(ncol = 8)) +  # 将 guides 放置在正确的位置
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", size = 15),  # 加粗并放大 X 轴刻度字体
    axis.text.y = element_text(face = "bold", size = 15),  # 加粗并放大 Y 轴刻度字体
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15),
    legend.position = "top",  # 将图例移动到顶部
    legend.box = "horizontal",  # 图例项水平排列
    legend.key.size = unit(2, "cm"),  # 设置图例键的大小
    legend.text = element_text(face = "bold",size = 16),  # 设置图例文本的大小
    legend.title = element_blank(),  # 移除图例标题
    legend.spacing.x = unit(0.1, "cm")  # 设置图例项之间的水平间距
  )

dev.off()  # 关闭 PDF 设备



