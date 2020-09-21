getDataset <- function(d, d1, d2){
  df <- d1[1:5]
  df <- cbind(df, d1[8:9])
  df <- cbind(df, d1[15:16])
  exclude <- c("1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00")
  df <- subset(df, !(season %in% exclude))
  df <- cbind(df, d2[4:11])
  
  d <- d %>% arrange(HomeTeam, AwayTeam, season)
  df <- cbind(df, d[4:5])
  df
}