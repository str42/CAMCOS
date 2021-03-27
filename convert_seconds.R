library(rvest)
library(tidyverse)
library(stringr)

converttime <- function(x){
  conv = 0
  n = length(x)
  for (i in 1:n){
    if (is.na(x[i])){
      conv[i] <- NA}
    else if ((str_detect(x[i], "day") | str_detect(x[i], "days"))  & (str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "mins") | str_detect(x[i], "min")) & (str_detect(x[i], "secs") | str_detect(x[i], "sec"))){  #has days hours and minutes and seconds
      time <-strsplit(x[i], split="[,,:]")
      time <-unlist(time)
      time <- gsub(pattern = '[ day]', replace ="", time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[min]', replace ="", time)
      time <- gsub(pattern = '[ sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      days <- as.numeric(time[1])*86400
      hour <- as.numeric(time[2])*3600
      minutes <- as.numeric(time[3])*60
      secs <- as.numeric(time[4])
      total <- (days + hour + minutes + secs)
      conv[i] <- total}
    else if ((str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "mins")| str_detect(x[i], "min")) & (str_detect(x[i], "days")  | str_detect(x[i], "day"))) {  #has hours, minutes days
      time <-unlist(time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[min]', replace ="", time)
      time <- gsub(pattern = '[ day]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      days <- as.numeric(time[1])*86400
      minutes <- as.numeric(time[2])*60
      secs <- as.numeric(time[3])
      total <- hour + minutes + secs
      conv[i] <- total}
    else if ((str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "secs") | str_detect(x[i], "sec") ) & (str_detect(x[i], "days") | str_detect(x[i], "day"))){
      time <- strsplit(x[i], split="[,,:]") #hours, seconds, days
      time <-unlist(time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[sec]', replace ="", time)
      time <- gsub(pattern = '[ day]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      days <- as.numeric(time[1])*86400
      hours <- as.numeric(time[2])*3600
      secs <- as.numeric(time[3])
      total <- hour + days + secs
      conv[i] <- total}
    else if ((str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "mins") | str_detect(x[i], "min")) & (str_detect(x[i], "secs") | str_detect(x[i], "sec"))) {  #has hours and minutes and seconds
      time <- strsplit(x[i], split="[,,:]")
      time <-unlist(time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[min]', replace ="", time)
      time <- gsub(pattern = '[ sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      hour <- as.numeric(time[1])*3600
      minutes <- as.numeric(time[2])*60
      secs <- as.numeric(time[3])
      total <- hour + minutes + secs
      conv[i] <- total}
    else if ((str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "mins") | str_detect(x[i], "min"))){ #has hours and minutes
      time <- strsplit(x[i], split="[,,:]")
      time <-unlist(time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[min]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      hour <- as.numeric(time[1])*3600
      minutes <- as.numeric(time[2])*60
      total <- hour + minutes 
      conv[i] <- total}
    else if ((str_detect(x[i], "hrs") | str_detect(x[i], "hr")) & (str_detect(x[i], "secs")  | str_detect(x[i], "sec"))){ #has hours and seconds
      time <- strsplit(x[i], split="[,,:]")
      time <-unlist(time)
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      hour <- as.numeric(time[1])*3600
      secs <- as.numeric(time[2])
      total <- hour + secs 
      conv[i] <- total}
    else if ((str_detect(x[i], "min") | str_detect(x[i], "mins")) & (str_detect(x[i], "sec") | str_detect(x[i], "secs"))){
      time <- strsplit(x[i], split="[,,:]")
      time <-unlist(time)
      time <- gsub(pattern = '[ min]', replace ="", time)
      time <- gsub(pattern = '[sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      minutes <- as.numeric(time[1])*60
      secs <- as.numeric(time[2])
      total <- minutes + secs 
      conv[i] <- total}
    
    else if (str_detect(x[i], "hrs")| str_detect(x[i], "hr")){ #has hours
      time <- x[i]
      time <- gsub(pattern = '[ hr]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      hour <- as.numeric(time[1])*3600
      conv[i] <- hour}
    else if ((str_detect(x[i], "mins") | str_detect(x[i], "min")) & (str_detect(x[i], "secs") | str_detect(x[i], "sec"))){ #has minutes and secs
      time <- x[i]
      time <- gsub(pattern = '[ min]', replace ="", time)
      time <- gsub(pattern = '[sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      minutes <- as.numeric(time[1])*60
      secs <- as.numeric(time[2])
      total <- minutes + secs 
      conv[i] <- total}
    else if 
    (str_detect(x[i], "mins")| str_detect(x[i], "min") ){ #has minutes
      time <- x[i]
      time <- gsub(pattern = '[ min]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      minutes <- as.numeric(time[1])*60
      conv[i] <- minutes}
    else if (str_detect(x[i], "sec") | str_detect(x[i], "secs")){ #seconds
      time <- x[i]
      time <- gsub(pattern = '[ sec]', replace ="", time)
      time <- gsub(pattern = '[ s]', replace ="", time)
      secs <- as.numeric(time[1])
      conv[i] <- secs}
    else{
      conv[i] <- NA}
  }
  return(conv)
}
  







