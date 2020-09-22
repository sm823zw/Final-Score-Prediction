matches <- function(pl, rankings, param){
  teams <- as.data.frame(table(rankings$HomeTeam))$Var1
  season <- as.data.frame(table(rankings$season))$Var1
  
  df <- data.frame(season=character(0), team=character(0), rank=numeric(0), HGGS=numeric(0), HGGC=numeric(0), AGGS=numeric(0), AGGC=numeric(0))
  
  for(s in season){
    if(s!="2000-01"){
      for(t in teams){
        g1 <- subset(pl, HomeTeam==t)
        g2 <- subset(pl, AwayTeam==t)
        i = 1
        while(i <= 5){
          subs <- subset(rankings, HomeTeam==t & season==s & AGGC==i)
          subs1 <- subset(g1, HomeTeam==t & Season==ps & AwayTeam %in% subs$AwayTeam)
          s1 <- sum(subs1$FTHG)
          if(nrow(subs1)!=0){
            s1 <- round(s1/nrow(subs1), 4)
          }
          
          subs <- subset(rankings, HomeTeam==t & season==s & AGGS==i)
          subs1 <- subset(g1, HomeTeam==t & Season==ps & AwayTeam %in% subs$AwayTeam)
          s2 <- sum(subs1$FTAG)
          if(nrow(subs1)!=0){
            s2 <- round(s2/nrow(subs1), 4)
          }
          
          subs <- subset(rankings, AwayTeam==t & season==s & HGGC==i)
          subs1 <- subset(g2, AwayTeam==t & Season==ps & HomeTeam %in% subs$HomeTeam)
          s3 <- sum(subs1$FTAG)
          if(nrow(subs1)!=0){
            s3 <- round(s3/nrow(subs1), 4)
          }
          
          subs <- subset(rankings, AwayTeam==t & season==s & HGGS==i)
          subs1 <- subset(g2, AwayTeam==t & Season==ps & HomeTeam %in% subs$HomeTeam)
          s4 <- sum(subs1$FTHG)
          if(nrow(subs1)!=0){
            s4 <- round(s4/nrow(subs1), 4)
          }
          
          df <- rbind(df, c(s, t, i, s1, s2, s3, s4))
          i <- i + 1
        }
      }
    }
    ps = s
  }
  
  
  colnames(df) <- c("season", "team", "rank", "HGGS", "HGGC", "AGGS", "AGGC")
  df
  
}


datasetNew <- function(rankings, new, d){
  
  df <- data.frame(season=character(0), HomeTeam=character(0), AwayTeam=character(0), HGGS=numeric(0), HGGC=numeric(0), AGGS=numeric(0), AGGC=numeric(0))
  m <- subset(rankings, HomeTeam=="Arsenal")
  
  teams <- as.data.frame(table(m$AwayTeam))$Var1
  season <- as.data.frame(table(m$season))$Var1
  f <- 0
  
  for(t in teams){
    for(s in season){
      if(s == "2000-01" | s=="2001-02"){
        if(nrow(subset(m, season==s & AwayTeam==t))==0){
          f <- 1
        } else {
          HGGS <- 1
          HGGC <- 0.5
          AGGS <- 0.5
          AGGC <- 1
        }
      } else {
        if(nrow(subset(m, season==s & AwayTeam==t))==0){
          f <- 1
        } else {
          HGGS <- new$HGGS[new$team=="Arsenal" & new$season==ps & new$rank== m$AGGC[m$AwayTeam==t & m$season==s]]
          HGGC <- new$HGGC[new$team=="Arsenal" & new$season==ps & new$rank== m$AGGS[m$AwayTeam==t & m$season==s]]
          AGGS <- new$AGGS[new$team=="Arsenal" & new$season==ps & new$rank== m$HGGC[m$AwayTeam==t & m$season==s]]
          AGGC <- new$AGGC[new$team=="Arsenal" & new$season==ps & new$rank== m$HGGS[m$AwayTeam==t & m$season==s]]
        }
      }
      ps <- s
      
      if(f != 1){
        df <- rbind(df, c(s, "Arsenal", t, HGGS, HGGC, AGGS, AGGC))  
      } else {
        f <- 0
      }
      
    }
  }
  colnames(df) <- c("season", "HomeTeam", "AwayTeam", "HGGS", "HGGC", "AGGS", "AGGC")
  
  d <- subset(d, HomeTeam=="Arsenal")
  d <- d %>% arrange(HomeTeam, AwayTeam, season)
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  
  df$FTHG <- d$FTHG
  df$FTAG <- d$FTAG
  df
}


datasetNew1 <- function(d, stats){
  df <- data.frame(d$season, d$HomeTeam, d$AwayTeam)
  colnames(df) <- c("season", "HomeTeam", "AwayTeam")
  df$HGGS <- 1
  df$HGGC <- 1

  df$AGGS <- 1
  df$AGGC <- 1
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$season))$Var1
  print(season)
  
  for(t in teams){
    for(s in season){
      if(s=="2000-01"){
        
      } else{
        df <- within(df, HGGS[HomeTeam==t & season==s] <- stats$HGGS[stats$team==t & stats$season==s])
        df <- within(df, HGGC[HomeTeam==t & season==s] <- stats$HGGC[stats$team==t & stats$season==s])

        df <- within(df, AGGS[AwayTeam==t & season==s] <- stats$AGGS[stats$team==t & stats$season==s])
        df <- within(df, AGGC[AwayTeam==t & season==s] <- stats$AGGC[stats$team==t & stats$season==s])
      }
    }
  }
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  df
}