#####################################
## Base ELO Tennis forecasting model

#Created: November 2020
#Last Update: 13 December 2020

######################################

#Load libraries
library(tidyverse,lib.loc='C:/Users/eswaralingams/R Packages')
library(dplyr,lib.loc='C:/Users/eswaralingams/R Packages')
library(lubridate,lib.loc='C:/Users/eswaralingams/R Packages')
library(ggplot2,lib.loc='C:/Users/eswaralingams/R Packages')
library(DescTools,lib.loc='C:/Users/eswaralingams/R Packages')
library(forecast)

formatTdata <- function(df){
  df<- df %>%
    mutate_at(vars(ATP, WRank, LRank, W1, W2, W3, W4, W5, L1, L2,
                   L3, L4, L5, Wsets, Lsets),as.double) #double formatting
  
  df$Date <- ymd(df$Date) #date formatting
  
  df <- df %>% mutate_at(vars(Series, Court, Surface, Round, Best.of, Comment),
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


for (surf in levels(df$Surface)){
      df <- read.csv("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/Data files/2000-2019.csv") # data look ok
      
      df <- df %>% filter(.,Surface == surf) %>% formatTdata
    
    
    EloDF <- data.frame(Player = 'FirstRow',
                        ELO = 111,
                        Matches = 0,
                        stringsAsFactors = FALSE)
    
    
    
    ####### FUNCTIONS #######
    
    #GrabElo Function
    GrabElo <- function(Player){
      which(EloDF$Player == Player) %>%
        EloDF$ELO[.] -> PlayerElo
      return(PlayerElo)
    }
    
    #GrabMatches Function
    GrabMatches <- function(Player){
      which(EloDF$Player == Player) %>%
        EloDF$Matches[.] -> PLayerMatches
      return(as.double(PLayerMatches))
    }
    
    
    #Calulate Probability Function
    ProbCalc <- function(x, y) {
      1/(1+10^((as.double(y)-as.double(x))/400)) %>% return(.)
    }
    
    #Update Elo
    # Taken from 538: https://fivethirtyeight.com/features/how-were-forecasting-the-2016-us-open/
    
    UpdateElo <- function(P_Elo, P_Prob, P_Matches, P_Result){
      K <- as.double(250)
      offset <- as.double(5)
      shape <- as.double(0.4)
      
      as.double(P_Elo) + (K/(as.double(P_Matches) + offset)^shape)*(as.double(P_Result)-as.double(P_Prob)) %>%
        return()
      
    }
    
    
    
    ### MAIN PROGRAM #######
    
    for (i in 1:nrow(df)){
      
      #Get players for the match
      PW <- df$Winner[i]
      PL <- df$Loser[i]
      
      #Get ELO rating for player, or create new record if new player
      if (length(GrabElo(PW)) == 0){
        PWElo <- as.double(1500)
        c(PW,PWElo,0) %>% rbind(EloDF,.) -> EloDF
      } else{PWElo <- GrabElo(PW)}
      
      if (length(GrabElo(PL)) == 0){
        PLElo <- as.double(1500)
        c(PL,PLElo,as.double(0)) %>% rbind(EloDF,.) -> EloDF
      } else{PLElo <- GrabElo(PL)}
      
      #Calculate Probabiities based on ELO
      PW_Prob <- ProbCalc(PWElo,PLElo) %>% as.double()
      PL_Prob <- ProbCalc(PLElo, PWElo) %>% as.double()
      
      #Calculate revised ELO ratings based on match results
      PWElo_Post <- UpdateElo(PWElo, PW_Prob,GrabMatches(PW)+1,1) %>% as.double()
      PLElo_Post <- UpdateElo(PLElo, PL_Prob,GrabMatches(PL)+1,0) %>% as.double()
      
      #Write to Probabilities and Revised ELO ratings to df
      df$WinnerELOProb[i] <- PW_Prob
      df$LoserEloProb[i] <- PL_Prob
      df$WinnerELO_Post[i] <- PWElo_Post
      df$LoserELO_Post[i] <- PLElo_Post
      
      #Update ELO DF given match result
      which(EloDF$Player == PW) -> W_EloDF_index
      which(EloDF$Player == PL) -> L_EloDF_index
      EloDF$ELO[W_EloDF_index] <- PWElo_Post
      EloDF$Matches[W_EloDF_index] <- as.double(EloDF$Matches[W_EloDF_index]) + 1
      EloDF$ELO[L_EloDF_index] <- PLElo_Post
      EloDF$Matches[L_EloDF_index] <- as.double(EloDF$Matches[L_EloDF_index]) + 1
      
    }
    # Write to file
        write_csv(df,
              paste("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/TennisData",
              surf,".csv",sep=""))
    write_csv(EloDF,
              paste("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/",
                    surf,"ELOData.csv",sep = ""))
}

