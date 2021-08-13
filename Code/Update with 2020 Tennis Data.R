#############################################
# Merge updated 2020 tennis dat set to the 2000-2019 base file
# http://www.tennis-data.co.uk/alldata.php

# Created: 13 December 2020
# Last Update: 13 December 2020

#############################################


#Load packages
library(tidyverse, lib.loc = 'C:/Users/eswaralingams/R Packages')
library(readxl, lib.loc = 'C:/Users/eswaralingams/R Packages')
library(dplyr,lib.loc='C:/Users/eswaralingams/R Packages')
library(lubridate,lib.loc='C:/Users/eswaralingams/R Packages')

setwd("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/Data files/")

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
  
  try(df<- df %>%
        mutate_at(vars(PSW,PSL),as.double)) #double formatting error handlers
  
  return(df)}


#Create aggregate data file 2000-2020

read_csv("2000-2019.csv") %>% formatTdata() -> Dbase
read_xlsx("2020.xlsx") %>% 
  formatTdata() %>%
  bind_rows(Dbase,.) %>%
  formatTdata() %>%
  write_csv(.,"2000-2020.csv") # write 2000-2020 df to file






