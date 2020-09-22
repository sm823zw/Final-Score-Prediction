goalRanking <- function(st){
  
  season <- as.data.frame(table(st$season))$Var1
  
  df <- data.frame(season=character(0), team=character(0), HGGS=numeric(0), HGGC=numeric(0), AGGS=numeric(0), AGGC=numeric(0))
  
  for(s in season){
    d <- subset(st, season==s & GP!=0)
    
    d$HGGS[order(as.numeric(d$HGGS))] <- nrow(d):1
    d$HGGC[order(as.numeric(d$HGGC))] <- 1:nrow(d)
    d$AGGS[order(as.numeric(d$AGGS))] <- nrow(d):1
    d$AGGC[order(as.numeric(d$AGGC))] <- 1:nrow(d)
    
    df <- rbind(df, d)
  }
  
  for(s in season){
    d <- subset(st, season==s & GP==0)
    df <- rbind(df, d)
  }
  
  df
}

shotsRanking <- function(sh){
  season <- as.data.frame(table(sh$season))$Var1
  
  df <- data.frame(season=character(0), team=character(0), HGS=numeric(0), HGAS=numeric(0), HGST=numeric(0), HGAST=numeric(0), AGS=numeric(0), AGAS=numeric(0), AGST=numeric(0), AGAST=numeric(0))
  
  for(s in season){
    d <- subset(sh, season==s & GP!=0)
    
    d$HGS[order(as.numeric(d$HGS))] <- nrow(d):1
    d$HGAS[order(as.numeric(d$HGAS))] <- 1:nrow(d)
    d$HGST[order(as.numeric(d$HGST))] <- nrow(d):1
    d$HGAST[order(as.numeric(d$HGAST))] <- 1:nrow(d)
    d$AGS[order(as.numeric(d$AGS))] <- nrow(d):1
    d$AGAS[order(as.numeric(d$AGAS))] <- 1:nrow(d)
    d$AGST[order(as.numeric(d$AGST))] <- nrow(d):1
    d$AGAST[order(as.numeric(d$AGAST))] <- 1:nrow(d)
    
    df <- rbind(df, d)
  }
  
  for(s in season){
    d <- subset(sh, season==s & GP==0)
    df <- rbind(df, d)
  }
  
  df <- df %>% arrange(team, season)
  df
}



rankDataset <- function(d, st, sh){
  
  df <- data.frame(d$Season, d$HomeTeam, d$AwayTeam)
  colnames(df) <- c("season", "HomeTeam", "AwayTeam")
  df$HGGS <- 21
  df$HGGC <- 21
  df$AGGS <- 21
  df$AGGC <- 21
  
  df$HGS <- 21
  df$HGST <- 21
  df$HGAS <- 21
  df$HGAST <- 21
  df$AGS <- 21
  df$AGST <- 21
  df$AGAS <- 21
  df$AGAST <- 21
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  
  for(t in teams){
    for(s in season){
      if(s!="1993-94"){
        df <- within(df, HGGS[HomeTeam==t & season==s] <- st$HGGS[st$team==t & st$season==ps])
        df <- within(df, HGGC[HomeTeam==t & season==s] <- st$HGGC[st$team==t & st$season==ps])
        df <- within(df, AGGS[AwayTeam==t & season==s] <- st$AGGS[st$team==t & st$season==ps])
        df <- within(df, AGGC[AwayTeam==t & season==s] <- st$AGGC[st$team==t & st$season==ps])
      }
      ps <- s
    }
  }
  for(t in teams){
    for(s in season){
      if(!s %in% c("1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01")){
        df <- within(df, HGS[HomeTeam==t & season==s] <- sh$HGS[sh$team==t & sh$season==ps])
        df <- within(df, HGST[HomeTeam==t & season==s] <- sh$HGST[sh$team==t & sh$season==ps])
        df <- within(df, HGAS[HomeTeam==t & season==s] <- sh$HGAS[sh$team==t & sh$season==ps])
        df <- within(df, HGAST[HomeTeam==t & season==s] <- sh$HGAST[sh$team==t & sh$season==ps])
        df <- within(df, AGS[AwayTeam==t & season==s] <- sh$AGS[sh$team==t & sh$season==ps])
        df <- within(df, AGST[AwayTeam==t & season==s] <- sh$AGST[sh$team==t & sh$season==ps])
        df <- within(df, AGAS[AwayTeam==t & season==s] <- sh$AGAS[sh$team==t & sh$season==ps])
        df <- within(df, AGAST[AwayTeam==t & season==s] <- sh$AGAST[sh$team==t & sh$season==ps])
      } 
      ps <- s
    }
  }
  
  df <- subset(df, !season %in% c("1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00"))
  df[is.na(df)] <- 21
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  
  df
}


category <- function(rank){
  if(as.numeric(rank) ==0){
    c = 5
  } else if(as.numeric(rank) <=4){
    c = 1
  } else if(as.numeric(rank) <= 8){
    c = 2
  } else if(as.numeric(rank) <= 12){
    c = 3
  } else if(as.numeric(rank) <= 16){
    c = 4
  } else if(as.numeric(rank) <= 20){
    c = 5
  } else {
    c = 3
  }
  c
}


categorize <- function(rankings){
  rankings$HGGS <- lapply(rankings$HGGS, category)
  rankings$HGGC <- lapply(rankings$HGGC, category)
  rankings$AGGS <- lapply(rankings$AGGS, category)
  rankings$AGGC <- lapply(rankings$AGGC, category)
  rankings$HGS <- lapply(rankings$HGS, category)
  rankings$HGAS <- lapply(rankings$HGAS, category)
  rankings$HGST <- lapply(rankings$HGST, category)
  rankings$HGAST <- lapply(rankings$HGAST, category)
  rankings$AGS <- lapply(rankings$AGS, category)
  rankings$AGAS <- lapply(rankings$AGAS, category)
  rankings$AGST <- lapply(rankings$AGST, category)
  rankings$AGAST <- lapply(rankings$AGAST, category)
  rankings
}

categorizeGoals <- function(rankings){
  rankings$HGGS <- lapply(rankings$HGGS, category)
  rankings$HGGC <- lapply(rankings$HGGC, category)
  rankings$AGGS <- lapply(rankings$AGGS, category)
  rankings$AGGC <- lapply(rankings$AGGC, category)
  
  rankings
}
