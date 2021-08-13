library(lubridate)
library(quantmod)
library(tidyverse)
library(dplyr)
library(ggplot2)



### Combine all tennis data from Extracted Files folder into one dataframe ###
files <- list.files(path="C:/Users/Senna/Desktop/Files/Betfair/Raw data/Extracted files/", 
                    full.names=TRUE, recursive=FALSE) %>% .[1:50]

rfiles <- function(x){
    read_csv(x) %>% 
    filter(.,SPORTS_ID == 2) %>% 
    mutate_at(vars(SETTLED_DATE,SCHEDULED_OFF,LATEST_TAKEN,FIRST_TAKEN,
                   `DT ACTUAL_OFF`),dmy_hms)  %>%
    filter(., EVENT == "Match Odds") %>%
    filter(.,IN_PLAY == "PE") %>%
    return(.)
  }


df <- map_dfr(files[1:3],rfiles)
df$Prob <- 1/df$ODDS

### Create table of unique events, which players played, and the dates they happenned ###
Event_Tbl <- df %>% group_by(EVENT_ID) %>% 
              summarise(P1 = unique(SELECTION)[1], 
                        P2 = unique(SELECTION)[2], 
                        Date = unique(`DT ACTUAL_OFF`))


# Load betting odds spreadsheet
BetDf <- readxl:: read_xlsx('C:/Users/Senna/Desktop/Files/Sports betting/Tennis model/Data/2016.xlsx')
BetDf$PW_Odds <- (1/BetDf$AvgW)/(1/BetDf$AvgW + 1/BetDf$AvgL)
BetDf$PL_Odds <- (1/BetDf$AvgL)/(1/BetDf$AvgW + 1/BetDf$AvgL)


PW_vec <- str_extract(BetDf$Winner,"[^\\s]+")
PL_vec <- str_extract(BetDf$Loser,"[^\\s]+")
Date_BetDF <- BetDf$Date

Event_IDs <- vector("list",length = nrow(BetDf))

SFind <- function(PW,PL,D){
              Event_Tbl %>% filter((Date > date(D) - 1) &
                  Date < (date(D) + 1)) %>%
                  filter((str_detect(P1,paste(c(PW,PL),collapse = "|")))) %>%
                  filter((str_detect(P2,paste(c(PW,PL),collapse = "|")))) %>%
                  select(EVENT_ID) %>%
              return(.)
}

Event_IDs <- pmap(list(PW_vec,PL_vec,Date_BetDF),SFind)

IDs <- map_lgl(Event_IDs, ~(
                if(nrow(.x) > 0){TRUE}
                else{FALSE}
                ))

EV_ID <- Event_IDs[IDs]
PW_ID <- PW_vec[IDs]
PL_ID <- PL_vec[IDs]
PW_Odds_ID <- BetDf$PW_Odds[IDs]
PL_Odds_ID <- BetDf$PL_Odds[IDs]

BetAmount_PLess <- function(EV,Player,P_Odds, threshold){
    df %>% 
    filter(EVENT_ID %in% unlist(EV)) %>%
    filter((str_detect(SELECTION,Player))) %>%
    filter(Prob < P_Odds - threshold) %>%
    select(VOLUME_MATCHED) %>%
    sum() %>% 
    return()}

BetAmount_PMore <- function(EV,Player,P_Odds, threshold){
  df %>% 
    filter(EVENT_ID %in% unlist(EV)) %>%
    filter((str_detect(SELECTION,Player))) %>%
    filter(Prob > P_Odds + threshold) %>%
    select(VOLUME_MATCHED) %>%
    sum() %>% 
    return()}
                    
pmap_dbl(list(EV_ID,PW_ID,PW_Odds_ID),
         possibly(BetAmount_PLess,NA),
         0.05)

pmap_dbl(list(EV_ID,PL_ID,PL_Odds_ID),
         possibly(BetAmount_PLess,NA),
         0.05)


#NExt steps
#1. Load the ELO df which has the predicted odds
#2. Run the volume calc against the predicted odds above a threshold (likely 10%)
#3. Filter matches with sufficient volume (like greater than 10%)
#4. Figure out the return on these matches given actual odds




