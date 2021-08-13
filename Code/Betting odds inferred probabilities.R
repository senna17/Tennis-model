##################################
# Forecasting model that use old betting numbers
##################################

# Take Betting odds from last match. What has changed? Form and surface mayb.
# Can update odds with a change in ELO? Odds probability * change in ratio of ELO probability


Win <- df$Winner[27174]
Los <- df$Loser[27174]

WinnerELOProb(Win,Los)

WinnerELOProb <- function(Winner, Loser) {
  iW <-  which(df$Winner[1:27173] == Winner & df$Loser[1:27173] == Loser) 
  iL<- which(df$Winner[1:27173] == Loser & df$Loser[1:27173] == Winner)
  if(iW>IL){
    BProb <- df$P.W.[iW]
    EloProb <- df$WinnerELOProb[iW]
  } else if(iL>IW){
    BProb <- df$P.L.[iL]
    EloProb <- df$LoserEloProb[iL]
 
  out <- as.list(c(BProb,EloProb))} 
  return(out)

}  


t<- which(df$Winner[25900:27173] == Winner & df$Loser[25900:27173] == Loser)
t2<- which(df$Winner[1:27173] == Winner & df$Loser[1:27173] == Loser)

df$WinnerELOProb[t2]
df$P.W.[t2]
df$WinnerELOProb[27174]
df$P.W.[27174]
BN <- df$P.W.[t2] * (df$WinnerELOProb[27174]/df$WinnerELOProb[t2])
