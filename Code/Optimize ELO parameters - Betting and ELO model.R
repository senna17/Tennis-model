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



#Load  dataset
MasterDF <- read.csv("//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/Data files/2000-2019.csv") # data look ok
MasterDF$P.L. <- (1/MasterDF$AvgL)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF$P.W. <- (1/MasterDF$AvgW)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF %>% formatTdata() %>%
  select(.,Date, AvgW,AvgL,P.L.,P.W.,Winner, Loser) %>%
  filter(year(Date) <=2018) %>%
  na.omit() -> MasterDF ->df


#Create ELODF
Player <- c(df$Winner,df$Loser) %>% unique()
ELO <- rep(1500,length(Player))
Matches <- rep(0,length(Player))

#EloDF <- tibble(Player = 'FirstRow',
#                ELO = 111,
#                Matches = 0)



####### FUNCTIONS #######


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

#GrabElo Function
GrabElo <- function(FindPlayer){
  which(Player == FindPlayer) %>% ELO[.] -> PlayerElo
  return(PlayerElo)
}

#GrabMatches Function
GrabMatches <- function(FindPlayer){
  which(Player == FindPlayer) %>% Matches[.] -> PLayerMatches
  return(as.double(PLayerMatches))
}


#Calulate Probability Function
ProbCalc <- function(x, y) {
  1/(1+10^((as.double(y)-as.double(x))/400)) %>% return(.)
}

UpdateElo <- function(P_Elo, P_Prob, P_Matches, P_Result, K, shape){
  offset <- as.double(5)
  shape <- as.double(shape)
  K <- as.double(K)
  
  as.double(P_Elo) + (K/(as.double(P_Matches) + offset)^shape)*(as.double(P_Result)-as.double(P_Prob)) %>%
    return()
  
}


#KBetVars <- seq(0,100,by = 100)
#KOutcomeVars <- seq(0,100,by = 100)
#ShapeBetVars <- seq(0,1,by = 0.1)
#ShapeOutcomeVars <- seq(0,1,by = 0.1)

KBetVars <- 400
KOutcomeVars <- 1000
ShapeBetVars <- 0.4
ShapeOutcomeVars <- 0.1


#Set up results vectors
c<-0
for (KBet in KBetVars){for (KOutcome in KOutcomeVars)
{for(ShapeBet in ShapeBetVars){for (ShapeOutcome in ShapeOutcomeVars){
  c <- c +1
}}}}

KBetVec <- vector("double",length = c)
KOutcomeVec <- vector("double",length = c)
ShapeBetVec <- vector("double",length = c)
ShapeOutcomeVec <- vector("double",length = c)
BB <- vector("double",length = c)
BO <- vector("double",length = c)
LL_B <- vector("double",length = c)
LL_O <- vector("double",length = c)
PA <- vector("double",length = c)


### MAIN PROGRAM #######
c <- 1

for (KBet in KBetVars){for (KOutcome in KOutcomeVars)
  {for(ShapeBet in ShapeBetVars){for (ShapeOutcome in ShapeOutcomeVars){
  
    WinnerELOProb <- vector("double", nrow(df))
    
    for (i in 1:nrow(df)){
    

    
    
    #Get players for the match
    PW <- df$Winner[i]
    PL <- df$Loser[i]
    PWOdds <- df$P.W.[i]
    PLOdds <- df$P.L.[i]
    PWElo <- GrabElo(PW)
    PLElo <- GrabElo(PL)
    PWMatches <- GrabMatches(PW)
    PLMatches <- GrabMatches(PL)
    
    #Calculate Probabiities based on ELO
    PW_Prob <- ProbCalc(PWElo,PLElo) %>% as.double()
    PL_Prob <- ProbCalc(PLElo, PWElo) %>% as.double()
    
    #Probability calculation for the match
    
    WinnerELOProb[i] <- PW_Prob
    #LoserEloProb[i] <- PL_Prob
    
    #Calculate revised ELO ratings based on betting odds
    PWElo_Post_BetOdds <- UpdateElo(PWElo, PW_Prob,PWMatches+1,PWOdds,KBet,ShapeBet) %>% as.double()
    PLElo_Post_BetOdds <- UpdateElo(PLElo, PL_Prob,PLMatches+1,PLOdds,KBet,ShapeBet) %>% as.double()
    
    #Calculate Probabiities based on Elo and betting odds
    PW_Prob_Betting_Odds <- ProbCalc(PWElo_Post_BetOdds,PLElo_Post_BetOdds) %>% as.double()
    PL_Prob_Betting_Odds <- ProbCalc(PLElo_Post_BetOdds, PWElo_Post_BetOdds) %>% as.double()
    
    #Calculate revised ELO ratings based on match results
    PWElo_Post <- UpdateElo(PWElo_Post_BetOdds, PW_Prob_Betting_Odds,PWMatches+1,1, KOutcome, ShapeOutcome) %>% as.double()
    PLElo_Post <- UpdateElo(PLElo_Post_BetOdds, PL_Prob_Betting_Odds,PLMatches+1,0, KOutcome, ShapeOutcome) %>% as.double()

    
    #Update ELO DF given match result
    which(Player == PW) -> W_EloDF_index
    which(Player == PL) -> L_EloDF_index
    
    ELO[W_EloDF_index] <- PWElo_Post
    Matches[W_EloDF_index] <- Matches[W_EloDF_index] + 1
    ELO[L_EloDF_index] <- PLElo_Post
    Matches[L_EloDF_index] <- Matches[L_EloDF_index] + 1
    
  }
  
    
  TI <- which(year(df$Date) == c(2017,2018))
  
  KBetVec[c] <- KBet
  KOutcomeVec[c]<- KOutcome
  ShapeBetVec[c] <- ShapeBet
  ShapeOutcomeVec[c] <- ShapeOutcome
  BB[c] <- MSE(as.double(WinnerELOProb[TI]),df$P.W.[TI])
  BO[c] <- MSE(as.double(WinnerELOProb[TI]),rep(1,length(TI)))
  LL_B[c] <- LogLoss(as.double(WinnerELOProb[TI]),df$P.W.[TI])
  LL_O[c] <- LogLoss(as.double(WinnerELOProb[TI]),rep(1,length(TI)))
  PA[c] <- length(dfXX$WinnerELOProb[which(dfXX$WinnerELOProb > 0.5)])/length(dfXX$WinnerELOProb)
  
  #Next loop counter
  c <- c+1

}}}}

# Write to file
#write_csv(df,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/TennisDataCSV.csv")
#write_csv(EloDF,"//ykr-rpfs/fdr-fin$/eswaralingams/Desktop/Personal - finance analysis/Tennis models/BaseELOData.csv")

#Resultdf <- c(K,R,KShape, RShape, BB, BO, LL_B, LL_O, PA) %>% rbind(Resultdf,.)

Resultdf <- tibble(KBet = KBetVec, 
                   KOutcome = KoutcomeVec, 
                   ShapeBet = ShapeBetVec, 
                   ShapeOutcome = ShapeOutcomeVec, 
                   BrierBet = BB, 
                   BrierOutome = BO, 
                   LogLossBet = LL_B, 
                   LogLossOutcome = LL_O, 
                   PredAcc = PA)


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


