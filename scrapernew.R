library(rvest)
library(tidyverse)
library(stringr)

#This function requires a list of hashes for transactions.
#When reading in your hashes from your file into R use the following argument when
#reading the file: colClasses = "character"  If you don't then R will convert the 
#hash token to hexidecimal numbers.  The url needs to full hash
scraper2 <- function(x){
  t = 0  
  data <- data.frame("hash" = 0, "block" = 0, "totalfee" = 0, "date" = 0, "time" = 0, "confirmation" = 0, "gasprice" = 0, "gasamt" = 0, "usd" = 0)
  n = length(x)
  for (i in 1:n){
    page1 <- paste("https://etherscan.io/tx/", x[i], sep = "") #pastes the hash the url
    transaction <- read_html(page1) #reads the specificed url page
    data[i,1] <- x[i] #adds transaction hash to dataframe
    #adds block number to the dataframe
    data[i,2] <- transaction %>% html_node("#ContentPlaceHolder1_maintable > div:nth-child(5) > div.col-md-9 > a") %>% html_text()
    #adds the total fee to the data frame as numeric
    fee <- transaction %>% html_node("#ContentPlaceHolder1_spanTxFee > span") %>% html_text()
    fee <- gsub("Ether.*", "\\1", fee)
    fee <- str_trim(fee)
    fee <- as.numeric(fee)
    data[i,3] <- fee
    #seperates the day time and adds them to the data frame
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
    #grabs the confirmation time and adds it to the data frame
    confir <- transaction %>% html_node("#ContentPlaceHolder1_maintable > div:nth-child(7) > div.col-md-9 > span.text-secondary.ml-2.d-none.d-sm-inline-block") %>% html_text()
    confir <- gsub(".*within", "\\1", confir)
    confir <- str_trim(confir)
    data[i,6] <- confir
    #grabs gas price and adds it as a numberic value to the data frame and changes amount to gwei
    gasprice <- transaction %>% html_node("#ContentPlaceHolder1_spanGasPrice") %>% html_text()
    gasprice <- gsub("Ether.*", "\\1", gasprice)
    gasprice <-str_trim(gasprice)
    gasprice <- as.numeric(gasprice)
    data[i,7] <- (gasprice*1000000000)
    #grabs gas amount used and changes it to numeric
    gasamt <- transaction %>% html_node("#ContentPlaceHolder1_spanGasUsedByTxn") %>% html_text()
    gasamt <- gsub(" .*", "\\1", gasamt)
    gasamt <- gsub(pattern = ",", replace ="", gasamt)
    gasamt <- as.numeric(gasamt)
    data[i,8] <- gasamt
    #grabs the amount the fee was worth in usd for the transaction and converts it to the 
    #usd price for one ether at the time of transaction
    usd <- transaction %>% html_node("#ContentPlaceHolder1_spanTxFee > span") %>% html_text()
    usd <- strsplit(usd, split=' ')
    usd <- unlist(usd)
    usd1 <- as.numeric(usd[1])
    usd2 <- gsub(pattern = '[()$]', replace ="", usd[3])
    usd2 <- as.numeric(usd2)
    usd <- usd2/
    data[i,9] <- usd
    t = t+1  #counter as scripts must rest every 29 transactions.  etherscan 
    #will not allow more than 29 pages to be processed at a time
    if (t >=28){
      Sys.sleep(22)
      t = 0
    }
    
  }
  closeAllConnections()
  return(data)}
