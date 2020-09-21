stats <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  df <- data.frame(team=character(0), season=character(0), GP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  for(t in teams){
    
    for(s in season){
      x <- subset(d, Season==s & HomeTeam==t)
      GP <- length(x$HomeTeam[x$HomeTeam==t])
      HGGS <- sum(x$FTHG)
      HGGC <- sum(x$FTAG)
      HGW <- length(x$FTR[x$FTR=="H"])
      HGD <- length(x$FTR[x$FTR=="D"])
      
      x <- subset(d, Season==s & AwayTeam==t)
      GP <- GP + length(x$AwayTeam[x$AwayTeam==t])
      AGGS <- sum(x$FTAG)
      AGGC <- sum(x$FTHG)
      AGW <- length(x$FTR[x$FTR=="A"])
      AGD <- length(x$FTR[x$FTR=="D"])
      
      df <- rbind(df, c(s, t, GP, HGGS, HGGC, HGW, HGD, AGGS, AGGC, AGW, AGD))
    }
  }
  colnames(df) <- c("season", "team", "GP", "HGGS", "HGGC", "HGW", "HGD", "AGGS", "AGGC", "AGW", "AGD")
  df
}


# Takes cumulative sum of all previous seasons for each parameter
cumstats <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  df <- data.frame(team=character(0), season=character(0), HGP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGP=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  for(t in teams){
    HGP <- 0
    HGGS <- 0
    HGGC <- 0
    HGW <- 0
    HGD <- 0
    AGP <- 0
    AGGS <- 0
    AGGC <- 0
    AGW <- 0
    AGD <- 0
    for(s in season){
      x <- subset(d, Season==s & HomeTeam==t)
      HGP <- HGP + length(x$HomeTeam[x$HomeTeam==t])
      HGGS <- HGGS + sum(x$FTHG)
      HGGC <- HGGC + sum(x$FTAG)
      HGW <- HGW + length(x$FTR[x$FTR=="H"])
      HGD <- HGD + length(x$FTR[x$FTR=="D"])
      
      x <- subset(d, Season==s & AwayTeam==t)
      AGP <- AGP + length(x$AwayTeam[x$AwayTeam==t])
      AGGS <- AGGS + sum(x$FTAG)
      AGGC <- AGGC + sum(x$FTHG)
      AGW <- AGW + length(x$FTR[x$FTR=="A"])
      AGD <- AGD + length(x$FTR[x$FTR=="D"])
      
      
      if(HGP==0 | AGP==0){
        df <- rbind(df, c(s, t, HGP, 1, 1, 0.3333, 0.3333, AGP, 1, 1, 0.3333, 0.3333))
      } else {
        df <- rbind(df, c(s, t, HGP, round(HGGS/HGP, 4), round(HGGC/HGP, 4), round(HGW/HGP, 4), round(HGD/HGP, 4), AGP, round(AGGS/AGP, 4), round(AGGC/AGP, 4), round(AGW/AGP, 4), round(AGD/AGP, 4)))
      }
    }
  }
  colnames(df) <- c("season", "team", "HGP", "HGGS", "HGGC", "HGW", "HGD", "AGP", "AGGS", "AGGC", "AGW", "AGD")
  df
}


cumstatsForselectedSeason <- function(d, cumstats, season){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  
  df <- data.frame(team=character(0), season=character(0), HGP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGP=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  for(t in teams){
    HGP <- 0
    HGGS <- 0
    HGGC <- 0
    HGW <- 0
    HGD <- 0
    AGP <- 0
    AGGS <- 0
    AGGC <- 0
    AGW <- 0
    AGD <- 0
    
    for(s in season){
      x <- subset(d, Season==s & HomeTeam==t)
      HGP <- HGP + length(x$HomeTeam[x$HomeTeam==t])
      HGGS <- HGGS + sum(x$FTHG)
      HGGC <- HGGC + sum(x$FTAG)
      HGW <- HGW + length(x$FTR[x$FTR=="H"])
      HGD <- HGD + length(x$FTR[x$FTR=="D"])
      
      x <- subset(d, Season==s & AwayTeam==t)
      AGP <- AGP + length(x$AwayTeam[x$AwayTeam==t])
      AGGS <- AGGS + sum(x$FTAG)
      AGGC <- AGGC + sum(x$FTHG)
      AGW <- AGW + length(x$FTR[x$FTR=="A"])
      AGD <- AGD + length(x$FTR[x$FTR=="D"])
      
      if(HGP==0 | AGP==0){
        df <- rbind(df, c(s, t, HGP, 1, 1, 0.3333, 0.3333, AGP, 1, 1, 0.3333, 0.3333))
      } else {
        df <- rbind(df, c(s, t, HGP, round(HGGS/HGP, 4), round(HGGC/HGP, 4), round(HGW/HGP, 4), round(HGD/HGP, 4), AGP, round(AGGS/AGP, 4), round(AGGC/AGP, 4), round(AGW/AGP, 4), round(AGD/AGP, 4)))
      }
    }
  }
  colnames(df) <- c("season", "team", "HGP", "HGGS", "HGGC", "HGW", "HGD", "AGP", "AGGS", "AGGC", "AGW", "AGD")
  df
}



h2hAllTime <- function(d){
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  
  df <- data.frame(team1=character(0), team2=character(0), H=numeric(0), D=numeric(0), A=numeric(0), HH2HG=numeric(0), AH2HG=numeric(0))

  
  for(t1 in teams){
    for(t2 in teams){
      if(t1 != t2){
        m1 <- subset(d, HomeTeam==t1 & AwayTeam==t2)

        H <- length(m1$FTR[m1$FTR=="H"])
        D <- length(m1$FTR[m1$FTR=="D"])
        A <- length(m1$FTR[m1$FTR=="A"])
        total <- H + D + A
        HG <- sum(m1$FTHG)
        AG <- sum(m1$FTAG)
        if(total == 0){
          df <- rbind(df, c(t1, t2, 0.3333, 0.3333, 0.3333, 1, 1))
        } else {
          df <- rbind(df, c(t1, t2, round(H/total, 4), round(D/total, 4), round(A/total, 4), round(HG/total, 4), round(AG/total, 4)))
        }
      }
    }
  }
  
  colnames(df) <- c("Team1", "Team2", "Home", "Draw", "Away", "HH2HG", "AH2HG")
  df
}


h2h <- function(d){
  h2hdata <- data.frame(d$Season, d$HomeTeam, d$AwayTeam, d$FTHG, d$FTAG, d$FTR)
  h2hdata$home <- rep(0, 10424)
  h2hdata$away <- rep(0, 10424)
  h2hdata$draw <- rep(0, 10424)

  h2hdata <- within(h2hdata, h2hdata$home[d.FTR=="H"] <- 1)
  h2hdata <- within(h2hdata, h2hdata$away[d.FTR=="A"] <- 1)
  h2hdata <- within(h2hdata, h2hdata$draw[d.FTR=="D"] <- 1)

  #colnames(h2hdata) <- c("season", "HomeTeam", "AwayTeam", "FTR", "v", "v", "v", "v", "v", "v", "v", "Home", "Away", "Draw")
  a <- h2hdata$h2hdata
  a
}

cumh2h <- function(d, h2h){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  
  df <- data.frame(season=character(0), team1=character(0), team2=character(0), HGH2H=numeric(0), AGH2H=numeric(0), ft=character(0), H=numeric(0), A=numeric(0), D=numeric(0))
  for(t1 in teams){
    for(t2 in teams){
      g <- subset(h2h, d.HomeTeam==t1 & d.AwayTeam==t2)
      g$home <- cumsum(g$home)
      g$away <- cumsum(g$away)
      g$draw <- cumsum(g$draw)
      g$d.FTHG <- cumsum(g$d.FTHG)
      g$d.FTAG <- cumsum(g$d.FTAG)
      len <- dim(g)[1]
      if(len == 0){
      } else if (len == 1){
        g$home <- 0.3333
        g$away <- 0.3333
        g$draw <- 0.3333
        g$d.FTHG <- 1
        g$d.FTAG <- 1
      } else {
        g$home <- c(0.3333, g$home[1:len-1])
        g$away <- c(0.3333, g$away[1:len-1])
        g$draw <- c(0.3333, g$draw[1:len-1])
        g$d.FTHG <- c(1, g$d.FTHG[1:len-1])
        g$d.FTAG <- c(1, g$d.FTAG[1:len-1])
      }
      #if(len!=1){
      #  g <- g[1:len-1, ] 
      #}
      df <- rbind(df, g)
    }
  }
  
  colnames(df) <- c("season", "HomeTeam", "AwayTeam", "HGH2H", "AGH2H", "FTR", "H", "A", "D")
  df$T <- df$H + df$A + df$D
  df$H <- round(df$H/df$T, 4)
  df$A <- round(df$A/df$T, 4)
  df$D <- round(df$D/df$T, 4)
  df$HGH2H <- round(df$HGH2H/df$T, 4)
  df$AGH2H <- round(df$AGH2H/df$T, 4)
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  df
}


goalDataset <- function(d, stats, cumh2hrecord){
  
  df <- data.frame(d$Season, d$HomeTeam, d$AwayTeam)
  colnames(df) <- c("season", "HomeTeam", "AwayTeam")
  df$HGGS <- 1
  df$HGGC <- 1
  df$HGW <- 0.3333
  df$HGD <- 0.3333
  df$AGGS <- 1
  df$AGGC <- 1
  df$AGW <- 0.3333
  df$AGD <- 0.3333
  
  df$HH2H <- 0
  df$AH2H <- 0
  df$DH2H <- 0
  
  df$HGH2H <- 0
  df$AGH2H <- 0
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  
  
  
  for(t in teams){
    for(s in season){
      if(s!="1993-94"){
        df <- within(df, HGGS[HomeTeam==t & season==s] <- stats$HGGS[stats$team==t & stats$season==ps])
        df <- within(df, HGGC[HomeTeam==t & season==s] <- stats$HGGC[stats$team==t & stats$season==ps])
        df <- within(df, HGW[HomeTeam==t & season==s] <- stats$HGW[stats$team==t & stats$season==ps])
        df <- within(df, HGD[HomeTeam==t & season==s] <- stats$HGD[stats$team==t & stats$season==ps])
        df <- within(df, AGGS[AwayTeam==t & season==s] <- stats$AGGS[stats$team==t & stats$season==ps])
        df <- within(df, AGGC[AwayTeam==t & season==s] <- stats$AGGC[stats$team==t & stats$season==ps])
        df <- within(df, AGW[AwayTeam==t & season==s] <- stats$AGW[stats$team==t & stats$season==ps])
        df <- within(df, AGD[AwayTeam==t & season==s] <- stats$AGD[stats$team==t & stats$season==ps])
      }
      ps <- s
    }
  }
  
  df$Result <- d$FTR
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  df$HH2H <- cumh2hrecord$H
  df$AH2H <- cumh2hrecord$A
  df$DH2H <- cumh2hrecord$D
  
  df$HGH2H <- cumh2hrecord$HGH2H
  df$AGH2H <- cumh2hrecord$AGH2H
  
  df
}

# Takes cumulative sum of only the previous four seasons for each parameter
cumstats1 <- function(d, st){
  
  teams <- as.data.frame(table(st$team))$Var1
  
  #teams <- c("Arsenal")
  
  df <- data.frame(team=character(0), season=character(0), GP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  
  for(t in teams){
    
    p <- subset(st, team == t)
    
    p$GP <- rollsumr(as.numeric(p$GP), k=4, fill=NA)
    p$HGGS <- rollsumr(as.numeric(p$HGGS), k=4, fill=NA)
    p$HGGC <- rollsumr(as.numeric(p$HGGC), k=4, fill=NA)
    p$HGW <- rollsumr(as.numeric(p$HGW), k=4, fill=NA)
    p$HGD <- rollsumr(as.numeric(p$HGD), k=4, fill=NA)
    p$AGGS <- rollsumr(as.numeric(p$AGGS), k=4, fill=NA)
    p$AGGC <- rollsumr(as.numeric(p$AGGC), k=4, fill=NA)
    p$AGW <- rollsumr(as.numeric(p$AGW), k=4, fill=NA)
    p$AGD <- rollsumr(as.numeric(p$AGD), k=4, fill=NA)
    
    
    df <- rbind(df, p)
  }
  
  df[df==0] <- 1
  #df <- subset(df, df$GP!=0)
  
  df$HGGS <- 2*round(df$HGGS/df$GP, 4)
  df$HGGC <- 2*round(df$HGGC/df$GP, 4)
  df$HGW <- 2*round(df$HGW/df$GP, 4)
  df$HGD <- 2*round(df$HGD/df$GP, 4)
  df$AGGS <- 2*round(df$AGGS/df$GP, 4)
  df$AGGC <- 2*round(df$AGGC/df$GP, 4)
  df$AGW <- 2*round(df$AGW/df$GP, 4)
  df$AGD <- 2*round(df$AGD/df$GP, 4)
  
  df
}
