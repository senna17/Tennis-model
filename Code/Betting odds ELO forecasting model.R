#####################################
## Base ELO Betting odds Tennis forecasting model

#Created: 20 December 2020
#Last Update: 20 December 2020

######################################

#From optimization
# K = 600; shape = 0.2

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

#Optimizing Df
#Kdf <- data.frame(Shape = 0,Brier = 0, stringsAsFactors = FALSE)
#Kvars <- 0.2 #seq(0.2,0.2,by = 0.1)


### MAIN PROGRAM #######

#for (shape in Kvars){

#Load data
df <- read.csv("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/Data files/2000-2019.csv") # data look ok
df$P.L. <- (1/df$AvgL)/(1/df$AvgW+1/df$AvgL)
df$P.W. <- (1/df$AvgW)/(1/df$AvgW+1/df$AvgL)
df %>% formatTdata() %>%
        select(.,Date, AvgW,AvgL,P.L.,P.W.,Winner, Loser) %>%
        na.omit() -> df


EloDF <- data.frame(Player = 'FirstRow',
                    ELO = 111,
                    Matches = 0,
                    stringsAsFactors = FALSE)



UpdateElo <- function(P_Elo, P_Prob, P_Matches, P_Result){
  offset <- as.double(5)
  shape <- as.double(0.2)
  K <- as.double(600)
  
  as.double(P_Elo) + (K/(as.double(P_Matches) + offset)^shape)*(as.double(P_Result)-as.double(P_Prob)) %>%
    return()
  
}



for (i in 1:nrow(df)){
  
  #Get players for the match
  PW <- df$Winner[i]
  PL <- df$Loser[i]
  PWOdds <- df$P.W.[i]
  PLOdds <- df$P.L.[i]
  
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
  PWElo_Post <- UpdateElo(PWElo, PW_Prob,GrabMatches(PW)+1,PWOdds) %>% as.double()
  PLElo_Post <- UpdateElo(PLElo, PL_Prob,GrabMatches(PL)+1,PLOdds) %>% as.double()
  
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

#dfXX <- df %>% filter(., year(Date) == 2019)
#BS <- BrierScore(as.double(dfXX$WinnerELOProb),rep(1,nrow(dfXX)))
#Kdf <- c(shape,BS) %>% rbind(Kdf,.)
#}

# Write to file
#write_csv(df,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/TennisDataCSV.csv")
#write_csv(EloDF,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/BaseELOData.csv")


