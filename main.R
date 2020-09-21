main <- function(){
  
  source("includeAllLibraries.R")
  source("goalsAndH2H.R")
  source("shots.R")
  source("getDataset.R")
  
  includeAllLibraries()
  pl <- read.csv("eplData.csv")
  d <- read.csv("shotsData.csv")
  
  d1 <- goalDataset(pl, cumstats1(pl, stats(pl)), cumh2h(pl, h2h(pl)))
  
  d2 <- shotsDataset(d, cumulshots(shots(d)))
  
  final <- getDataset(d, d1, d2)
  
  final

}