
## TENNIS MODEL FUNCTIONS FILE ##

'FORMAT DATA FUNCTION
Format Data formats the raw spreadsheets from tennis data.co.uk by getting all variales in order
'

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


MainProgram <- function(PW,PL,PWOdds,PLOdds){
  
  PWElo <- GrabElo(PW)
  PLElo <- GrabElo(PL)
  
  PWMatches <- GrabMatches(PW)
  PLMatches <- GrabMatches(PL)
  
  PW_Prob <- ProbCalc(PWElo,PLElo) %>% as.double()
  PL_Prob <- ProbCalc(PLElo, PWElo) %>% as.double()
  
  #Calculate revised ELO ratings based on betting odds
  PWElo_Post <- UpdateElo(PWElo, PW_Prob,PWMatches+1,PWOdds,KBet,ShapeBet) %>% as.double()
  PLElo_Post <- UpdateElo(PLElo, PL_Prob,PLMatches+1,PLOdds,KBet,ShapeBet) %>% as.double()
  
  #Update ELO DF given match result
  which(Player == PW) -> W_EloDF_index
  which(Player == PL) -> L_EloDF_index
  
  ELO[W_EloDF_index] <- PWElo_Post
  Matches[W_EloDF_index] <- Matches[W_EloDF_index] + 1
  ELO[L_EloDF_index] <- PLElo_Post
  Matches[L_EloDF_index] <- Matches[L_EloDF_index] + 1
  
  return(tibble(
    Elo_Prob_W = PW_Prob,
    Elo_Prob_L = PL_Prob))
}
