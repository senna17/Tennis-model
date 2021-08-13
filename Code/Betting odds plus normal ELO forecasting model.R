#####################################
## Base ELO Betting odds + normal ELO Tennis forecasting model

#Created: 20 December 2020
#Last Update: 20 December 2020

######################################

# One loop takes 70 seconds

start.time <- Sys.time()

#Load libraries
library(tidyverse,lib.loc='C:/Users/eswaralingams/R Packages')
library(dplyr,lib.loc='C:/Users/eswaralingams/R Packages')
library(lubridate,lib.loc='C:/Users/eswaralingams/R Packages')
library(ggplot2,lib.loc='C:/Users/eswaralingams/R Packages')
library(DescTools,lib.loc='C:/Users/eswaralingams/R Packages')
library(forecast)
library(MLmetrics,lib.loc='C:/Users/eswaralingams/R Packages')

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
Resultdf <- data.frame(K = 0, R = 0, KShape = 0, RShape = 0, 
                       BrierBet = 0, BrierOutome = 0, LogLossBet = 0, 
                       LogLossOutcome = 0, PredAcc = 0, stringsAsFactors = FALSE)

#Kvars <- seq(0,100,by = 100)
#Rvars <- seq(0,100,by = 100)
#KShapeVars <- seq(0,1,by = 0.1)
#RShapeVars <- seq(0,1,by = 0.1)

Kvars <- 0
Rvars <- 1500
KShapeVars <- 0
RShapeVars <- 0.2


#Load master dataset
MasterDF <- read.csv("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/Data files/2000-2019.csv") # data look ok
MasterDF$P.L. <- (1/MasterDF$AvgL)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF$P.W. <- (1/MasterDF$AvgW)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF %>% formatTdata() %>%
  select(.,Date, AvgW,AvgL,P.L.,P.W.,Winner, Loser) %>%
  filter(year(Date) <=2018) %>%
  na.omit() -> MasterDF

### MAIN PROGRAM #######

for (K in Kvars){for (R in Rvars){for(KShape in KShapeVars){for (RShape in RShapeVars){

#Load data
df <- MasterDF

EloDF <- data.frame(Player = 'FirstRow',
                    ELO = 111,
                    Matches = 0,
                    stringsAsFactors = FALSE)


UpdateElo <- function(P_Elo, P_Prob, P_Matches, P_Result){
  offset <- as.double(5)
  #shape <- as.double(0.4)
  #K <- as.double(600)
  
  as.double(P_Elo) + (K/(as.double(P_Matches) + offset)^KShape)*(as.double(P_Result)-as.double(P_Prob)) %>%
    return()
  
}

UpdateEloBettingOdds <- function(P_Elo, P_Prob, P_Matches, P_Result){
  offset <- as.double(5)
  #shape <- as.double(0.2)
  #R <- as.double(600)
  
  as.double(P_Elo) + (R/(as.double(P_Matches) + offset)^RShape)*(as.double(P_Result)-as.double(P_Prob)) %>%
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

  #Calculate revised ELO ratings based on betting odds
  PWElo_Post_BetOdds <- UpdateEloBettingOdds(PWElo, PW_Prob,GrabMatches(PW)+1,PWOdds) %>% as.double()
  PLElo_Post_BetOdds <- UpdateEloBettingOdds(PLElo, PL_Prob,GrabMatches(PL)+1,PLOdds) %>% as.double()
  
  #Calculate Probabiities based on Elo and betting odds
  PW_Prob_Betting_Odds <- ProbCalc(PWElo_Post_BetOdds,PLElo_Post_BetOdds) %>% as.double()
  PL_Prob_Betting_Odds <- ProbCalc(PLElo_Post_BetOdds, PWElo_Post_BetOdds) %>% as.double()
  
  #Calculate revised ELO ratings based on match results
  PWElo_Post <- UpdateElo(PWElo_Post_BetOdds, PW_Prob_Betting_Odds,GrabMatches(PW)+1,1) %>% as.double()
  PLElo_Post <- UpdateElo(PLElo_Post_BetOdds, PL_Prob_Betting_Odds,GrabMatches(PL)+1,0) %>% as.double()
  
  
  #Write to Probabilities and Revised ELO ratings to df
  df$WinnerELOProb[i] <- PW_Prob
  df$LoserEloProb[i] <- PL_Prob
  #df$WinnerELO_Post[i] <- PWElo_Post
  #df$LoserELO_Post[i] <- PLElo_Post
  
  #Update ELO DF given match result
  which(EloDF$Player == PW) -> W_EloDF_index
  which(EloDF$Player == PL) -> L_EloDF_index
  EloDF$ELO[W_EloDF_index] <- PWElo_Post
  EloDF$Matches[W_EloDF_index] <- as.double(EloDF$Matches[W_EloDF_index]) + 1
  EloDF$ELO[L_EloDF_index] <- PLElo_Post
  EloDF$Matches[L_EloDF_index] <- as.double(EloDF$Matches[L_EloDF_index]) + 1
  
}

dfXX <- df %>% filter(., year(Date) %in% c(2017,2018))
BB <- MSE(as.double(dfXX$WinnerELOProb),dfXX$P.W.)
BO <- MSE(as.double(dfXX$WinnerELOProb),rep(1,nrow(dfXX)))
LL_B <- LogLoss(as.double(dfXX$WinnerELOProb),dfXX$P.W.)
LL_O <- LogLoss(as.double(dfXX$WinnerELOProb),rep(1,nrow(dfXX)))
PA <- length(dfXX$WinnerELOProb[which(dfXX$WinnerELOProb > 0.5)])/length(dfXX$WinnerELOProb)

Resultdf <- c(K,R,KShape, RShape, BB, BO, LL_B, LL_O, PA) %>% rbind(Resultdf,.)
}}}}

# Write to file
#write_csv(df,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/TennisDataCSV.csv")
#write_csv(EloDF,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/BaseELOData.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


