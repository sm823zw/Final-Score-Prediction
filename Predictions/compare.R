compareXG <- function(pred, t){
  games <- subset(pred, HomeTeam == t | AwayTeam == t)
  games$game <- 1:38
  games$Actual.FTHG <- cumsum(games$Actual.FTHG)
  games$Predicted.FTHG <- cumsum(games$Predicted.FTHG)
  games$Actual.FTAG <- cumsum(games$Actual.FTAG)
  games$Predicted.FTAG <- cumsum(games$Predicted.FTAG)
  
  g <- ggplot(games) + geom_line(aes(game, Actual.FTHG), col="red")
  g <- g + geom_line(aes(game, Predicted.FTHG), col="blue")
  g <- g + ylab("Goals Scored/Expected to score")
  g
}

compareXGA <- function(pred, t){
  games <- subset(pred, HomeTeam == t | AwayTeam == t)
  games$game <- 1:38
  games$Actual.FTHG <- cumsum(games$Actual.FTHG)
  games$Predicted.FTHG <- cumsum(games$Predicted.FTHG)
  games$Actual.FTAG <- cumsum(games$Actual.FTAG)
  games$Predicted.FTAG <- cumsum(games$Predicted.FTAG)
  
  g <- ggplot(games) + geom_line(aes(game, Actual.FTAG), col="red")
  g <- g + geom_line(aes(game, Predicted.FTAG), col="blue")
  g <- g + ylab("Goals Conceded/Expected to concede")
  g
}

summaryGoals <- function(pred){
  df <- data.frame(team=character(0), G=numeric(0), xG=numeric(0), GA=numeric(0), xGA=numeric(0))
  
  teams <- as.data.frame(table(pred$HomeTeam))$Var1
  for(t in teams){
    games <- subset(pred, HomeTeam == t)
    G <- sum(games$Actual.FTHG)
    xG <- sum(games$Predicted.FTHG)
    GA <- sum(games$Actual.FTAG)
    xGA <- sum(games$Predicted.FTAG)
    games <- subset(pred, AwayTeam == t)
    G <- G + sum(games$Actual.FTAG)
    xG <- xG + sum(games$Predicted.FTAG)
    GA <- GA + sum(games$Actual.FTHG)
    xGA <- xGA + sum(games$Predicted.FTHG)
    
    
    df <- rbind(df, c(t, G, xG, GA, xGA))
  }
  colnames(df) <- c("Team", "Goals Scored", "xG", "Goals Conceded", "xGA")
  df
}
