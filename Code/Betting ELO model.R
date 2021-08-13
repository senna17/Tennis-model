#####################################
## Base ELO Betting odds + normal ELO Tennis forecasting model

#Created: 20 December 2020
#Last Update: 20 December 2020

######################################

# One loop takes 70 seconds

#start.time <- Sys.time()

#Load libraries
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(forecast)
library(MLmetrics)



#EloDF <- tibble(Player = 'FirstRow',
#                ELO = 111,
#                Matches = 0)



####### FUNCTIONS #######


#Load  dataset
MasterDF <- read.csv("C:/Users/Senna/Desktop/Files/Sports betting/Tennis model/Data/2000-2020.csv") # data look ok
MasterDF$P.L. <- (1/MasterDF$AvgL)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF$P.W. <- (1/MasterDF$AvgW)/(1/MasterDF$AvgW+1/MasterDF$AvgL)
MasterDF %>% formatTdata() %>%
  select(.,Date, AvgW,AvgL,P.L.,P.W.,Winner, Loser) %>%
  #filter(year(Date) <=2018) %>%
  na.omit() -> MasterDF ->df


#Create ELODF
Player <- c(df$Winner,df$Loser) %>% unique()
ELO <- rep(1500,length(Player))
Matches <- rep(0,length(Player))


#Set update parameters
#KBetVars <- seq(0,1500,by = 100)
#ShapeBetVars <- seq(0,1,by = 0.1)

KBetVars <- 400
ShapeBetVars <- 0.4

#Get players for the match
PW <- df$Winner
PL <- df$Loser
PWOdds <- df$P.W.
PLOdds <- df$P.L.


#Main Pogram
OddsCalc <- pmap_dfr(list(PW,PL,PWOdds,PLOdds),
                     MainProgram)









#Set up results vectors
c<-0
for (KBet in KBetVars)
{for(ShapeBet in ShapeBetVars){
  c <- c +1
}}

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


