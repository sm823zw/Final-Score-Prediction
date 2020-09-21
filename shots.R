shots <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$season))$Var1
  df <- data.frame(team=character(0), season=character(0), GP=numeric(0), HGS=numeric(0), HGST=numeric(0), HGAS=numeric(0), HGAST=numeric(0), AGS=numeric(0), AGST=numeric(0), AGAS=numeric(0), AGAST=numeric(0))
  for(t in teams){
    
    for(s in season){
      x <- subset(d, season==s & HomeTeam==t)
      GP <- length(x$HomeTeam[x$HomeTeam==t])
      HGS <- sum(x$HS)
      HGAS <- sum(x$AS)
      HGST <- sum(x$HST)
      HGAST <- sum(x$AST)
      
      x <- subset(d, season==s & AwayTeam==t)
      GP <- GP + length(x$AwayTeam[x$AwayTeam==t])
      AGS <- sum(x$HS)
      AGAS <- sum(x$AS)
      AGST <- sum(x$HST)
      AGAST <- sum(x$AST)
      
      df <- rbind(df, c(s, t, GP, HGS, HGAS, HGST, HGAST, AGS, AGAS, AGST, AGAST))
    }
  }
  colnames(df) <- c("season", "team", "GP", "HGS", "HGAS", "HGST", "HGAST", "AGS", "AGAS", "AGST", "AGAST")
  df
}

# Takes cumulative sum of all previous seasons for each parameter
cumulshotsAllSeason <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$season))$Var1
  df <- data.frame(team=character(0), season=character(0), HGP=numeric(0), HGS=numeric(0), HGST=numeric(0), HGAS=numeric(0), HGAST=numeric(0), AGP=numeric(0), AGS=numeric(0), AGST=numeric(0), AGAS=numeric(0), AGAST=numeric(0))
  for(t in teams){
    
    HGP <- 0
    HGS <- 0
    HGAS <- 0
    HGST <- 0
    HGAST <- 0
    AGP <- 0
    AGS <- 0
    AGAS <- 0
    AGST <- 0
    AGAST <- 0
    
    for(s in season){
      x <- subset(d, season==s & HomeTeam==t)
      HGP <- HGP + length(x$HomeTeam[x$HomeTeam==t])
      HGS <- HGS + sum(x$HS)
      HGAS <- HGAS + sum(x$AS)
      HGST <- HGST + sum(x$HST)
      HGAST <- HGAST + sum(x$AST)
      
      x <- subset(d, season==s & AwayTeam==t)
      AGP <- AGP + length(x$AwayTeam[x$AwayTeam==t])
      AGS <- AGS + sum(x$HS)
      AGAS <- AGAS + sum(x$AS)
      AGST <- AGST + sum(x$HST)
      AGAST <- AGAST + sum(x$AST)
      
      if(HGP==0 | AGP==0){
        df <- rbind(df, c(s, t, HGP, 1, 1, 0.3333, 0.3333, AGP, 1, 1, 0.3333, 0.3333))
      } else {
        df <- rbind(df, c(s, t, HGP, round(HGS/HGP, 4), round(HGAS/HGP, 4), round(HGST/HGP, 4), round(HGAST/HGP, 4), AGP, round(AGS/AGP, 4), round(AGAS/AGP, 4), round(AGST/AGP, 4), round(AGAST/AGP, 4)))
      }

    }
  }
  colnames(df) <- c("season", "team", "HGP", "HGS", "HGAS", "HGST", "HGAST", "AGP", "AGS", "AGAS", "AGST", "AGAST")
  df
}

shotsDataset <- function(d, st){
  
  df <- data.frame(d$season, d$HomeTeam, d$AwayTeam)
  colnames(df) <- c("season", "HomeTeam", "AwayTeam")
  df$HGS <- 10
  df$HGAS <- 5
  df$HGST <- 4
  df$HGAST <- 2
  df$AGS <- 5
  df$AGAS <- 10
  df$AGST <- 2
  df$AGAST <- 4
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$season))$Var1
  
  for(t in teams){
    for(s in season){
      if(s!="2000-01"){
        df <- within(df, HGS[HomeTeam==t & season==s] <- st$HGS[st$team==t & st$season==ps])
        df <- within(df, HGAS[HomeTeam==t & season==s] <- st$HGAS[st$team==t & st$season==ps])
        df <- within(df, HGST[HomeTeam==t & season==s] <- st$HGST[st$team==t & st$season==ps])
        df <- within(df, HGAST[HomeTeam==t & season==s] <- st$HGAST[st$team==t & st$season==ps])
        df <- within(df, AGS[AwayTeam==t & season==s] <- st$AGS[st$team==t & st$season==ps])
        df <- within(df, AGAS[AwayTeam==t & season==s] <- st$AGAS[st$team==t & st$season==ps])
        df <- within(df, AGST[AwayTeam==t & season==s] <- st$AGST[st$team==t & st$season==ps])
        df <- within(df, AGAST[AwayTeam==t & season==s] <- st$AGAST[st$team==t & st$season==ps])
      }
      ps <- s
    }
  }
  
  df$HGS[is.na(df$HGS)] <- 10
  df$HGAS[is.na(df$HGAS)] <- 5
  df$HGST[is.na(df$HGST)] <- 4
  df$HGAST[is.na(df$HGAST)] <- 2
  df$AGS[is.na(df$AGS)] <- 5
  df$AGAS[is.na(df$AGAS)] <- 10
  df$AGST[is.na(df$AGST)] <- 2
  df$AGAST[is.na(df$AGAST)] <- 4

  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  
  df
}

# Takes cumulative sum of only the previous four seasons for each parameter
cumulshots <- function(shots){
  
  teams <- as.data.frame(table(shots$team))$Var1
  df <- data.frame(team=character(0), season=character(0), GP=numeric(0), HGS=numeric(0), HGST=numeric(0), HGAS=numeric(0), HGAST=numeric(0), AGS=numeric(0), AGST=numeric(0), AGAS=numeric(0), AGAST=numeric(0))
  
  for(t in teams){
    d <- subset(shots, team == t)

    d$GP <- rollsumr(as.numeric(d$GP), k=4, fill=NA)
    d$HGS <- rollsumr(as.numeric(d$HGS), k=4, fill=NA)
    d$HGAS <- rollsumr(as.numeric(d$HGAS), k=4, fill=NA)
    d$HGST <- rollsumr(as.numeric(d$HGST), k=4, fill=NA)
    d$HGAST <- rollsumr(as.numeric(d$HGAST), k=4, fill=NA)
    d$AGS <- rollsumr(as.numeric(d$AGS), k=4, fill=NA)
    d$AGAS <- rollsumr(as.numeric(d$AGAS), k=4, fill=NA)
    d$AGST <- rollsumr(as.numeric(d$AGST), k=4, fill=NA)
    d$AGAST <- rollsumr(as.numeric(d$AGAST), k=4, fill=NA)
    
    df <- rbind(df, d)
  }
  
  df[df==0] <- 1
  
  df$HGS <- 2*round(df$HGS/df$GP, 4)
  df$HGAS <- 2*round(df$HGAS/df$GP, 4)
  df$HGST <- 2*round(df$HGST/df$GP, 4)
  df$HGAST <- 2*round(df$HGAST/df$GP, 4)
  df$AGS <- 2*round(df$AGS/df$GP, 4)
  df$AGAS <- 2*round(df$AGAS/df$GP, 4)
  df$AGST <- 2*round(df$AGST/df$GP, 4)
  df$AGAST <- 2*round(df$AGAST/df$GP, 4)
  
  df
}