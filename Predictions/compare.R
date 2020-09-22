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