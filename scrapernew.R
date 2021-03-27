library(rvest)
library(tidyverse)
library(stringr)

scraper2 <- function(x){
  t = 0
  data <- data.frame("hash" = 0, "block" = 0, "totalfee" = 0, "date" = 0, "time" = 0, "confirmation" = 0, "gasprice" = 0, "gasamt" = 0)
  n = length(x)
  for (i in 1:n){
    page1 <- paste("https://etherscan.io/tx/", x[i], sep = "")
    transaction <- read_html(page1)
    data[i,1] <- x[i]
    data[i,2] <- transaction %>% html_node("#ContentPlaceHolder1_maintable > div:nth-child(5) > div.col-md-9 > a") %>% html_text()
    fee <- transaction %>% html_node("#ContentPlaceHolder1_spanTxFee > span") %>% html_text()
    fee <- gsub("Ether.*", "\\1", fee)
    fee <- str_trim(fee)
    fee <- as.numeric(fee)
    data[i,3] <- fee
    clocktime <-  transaction %>% html_node("#ContentPlaceHolder1_maintable > div:nth-child(7) > div.col-md-9") %>% html_text()
    k <- str_extract_all(clocktime, "\\([^()]+\\)")[[1]] #getting data and time from string
    k <- gsub(pattern = '[(]', replace ="", k) #rid of parenthesis
    k <- gsub(pattern = '[)]', replace ="", k)
    k <- str_split_fixed(k, " ", n = 2) 
    k <- strptime(k[2], format="%H:%M:%S")
    k <- gsub(pattern = 'PDT)]', replace ="", k)
    k <- str_split_fixed(k, " ", n = 2)
    dt <- k[,1]
    data[i, 4] <- dt
    data[i,5] <- k[,2]
    confir <- transaction %>% html_node("#ContentPlaceHolder1_maintable > div:nth-child(7) > div.col-md-9 > span.text-secondary.ml-2.d-none.d-sm-inline-block") %>% html_text()
    confir <- gsub(".*within", "\\1", confir)
    confir <- str_trim(confir)
    data[i,6] <- confir
    gasprice <- transaction %>% html_node("#ContentPlaceHolder1_spanGasPrice") %>% html_text()
    gasprice <- gsub("Ether.*", "\\1", gasprice)
    gasprice <-str_trim(gasprice)
    gasprice <- as.numeric(gasprice)
    data[i,7] <- (gasprice*1000000000)
    gasamt <- transaction %>% html_node("#ContentPlaceHolder1_spanGasUsedByTxn") %>% html_text()
    gasamt <- gsub(" .*", "\\1", gasamt)
    gasamt <- gsub(pattern = ",", replace ="", gasamt)
    gasamt <- as.numeric(gasamt)
    data[i,8] <- gasamt
    t = t+1
    if (t >=28){
      Sys.sleep(22)
      t = 0
    }
    
  }
  closeAllConnections()
  return(data)}
