Time_processing=function(data) {
  for (i in seq_along(data$date)) {
    if (nchar(data$date[i]) == 10 | nchar(data$date[i]) == 9|nchar(data$date[i]) == 8) {
      data$date[i] <- paste0(data$date[i], " 00:00:00")
    }
  }
  
  if (any(str_detect(data$date, '-'))) {
    data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  } else {
    data$date <- as.POSIXct(strptime(data$date, format = "%Y/%m/%d %H:%M", tz = "GMT"))
  }
  
  return(data)
}


