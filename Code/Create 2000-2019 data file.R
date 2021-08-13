 #############################################
# Merge 2000-2019 tennis dat sets
# http://www.tennis-data.co.uk/alldata.php

# Created: 22 November 2020
# Last Update: 13 December 2020

#############################################


#Load packages
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)

setwd("C:/Users/Senna/Desktop/Files/Sports betting/Tennis model/Data")

### Function to format data ####

formatTdata <- function(df){
                  df<- df %>%
                    mutate_at(vars(ATP, WRank, LRank, W1, W2, W3, W4, W5, L1, L2,
                                   L3, L4, L5, Wsets, Lsets),as.double) #double formatting
                  
                  df$Date <- ymd(df$Date) #date formatting
                  
                  df <- df %>% mutate_at(vars(Series, Court, Surface, Round, `Best of`, Comment),
                                         as.factor) #factor formatting
                  
                  df <- df %>% mutate_at(vars(Location, Tournament, Winner, Loser),
                                         as.character) # character formatting
                  
                  try(df<- df %>%
                    mutate_at(vars(WPts, LPts),as.double)) #double formatting error handlers
                  
                  try(df<- df %>%
                        mutate_at(vars(B365W,B365L),as.double)) #double formatting error handlers
                  
                  return(df)}


#Create aggregate data file 2000-2020

read_xls("Odds and results/2000.xls") %>% formatTdata() -> Dbase
for (i in 2001:2020){
    tryCatch(
      paste("Odds and results/",i,".xlsx",sep = "") %>% read_xlsx(.), 
      error = function(e){
        paste("Odds and results/",i,".xls",sep = "") %>% read_xls(.)}) %>%
    formatTdata() %>%
    bind_rows(Dbase,.) %>%
    formatTdata()-> Dbase}


write_csv(Dbase,"2000-2020.csv") # write 2000-2019 df to file





