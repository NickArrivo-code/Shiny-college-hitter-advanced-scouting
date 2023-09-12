Hitting_Leaders <- read.csv("2023 Hitting Leaders.csv")
Hitting_Leaders$RunsCreated <- as.double(Hitting_Leaders$RunsCreated)
Hitting_Leaders <- Hitting_Leaders[order(Hitting_Leaders$RunsCreated, decreasing = TRUE),]
Hitting_Leaders$Contact. <- as.numeric(sub("%", "", Hitting_Leaders$Contact.))

#Find percentiles with ecdf((Hitting_Leaders)(as.double(Hitting_Leaders[1,13])))


getPlayerTeam <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,5]))
}
getPlayerName <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,2]))
}
getPlayerMaxEV <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,14]))
}
getPlayerRunsCreated <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,13]))
}
getPlayerLineDrive <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,15]))
}
getPlayerIZWhiff <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,17]))
}
getPlayerGroundBall <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,26]))
}
getPlayerHardHit <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,24]))
}
getPlayerBarrel <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,34]))
}
getPlayerK <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,45]))
}
getPlayerBB <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,46]))
}
getPlayerAVG <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,40]))
}
getPlayerOBP <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,41]))
}
getPlayerSLG <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,42]))
}
getPlayerOPS <- function(name){
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$playerFullName == name)
  return(as.character(df[,43]))
}
#Can add function to choose conference the team is in and use that value of projected wins
getDataTable <- function(name){
  runs <- as.double(getPlayerRunsCreated(name))
  df <- Hitting_Leaders %>% filter(Hitting_Leaders$RunsCreated >= (runs-20) & Hitting_Leaders$RunsCreated <= (runs+5))
  df <- df[,c(2,13,14,15,24,31,34,35,41,42,43,45,46)]
  return(gt(df[1:12,]))
}



plotRunsRange <- function(runs){
  min <- min(runs)
  max <- max(runs)
  runsData <- Hitting_Leaders %>% filter(Hitting_Leaders$Max.EV >= 90 & Hitting_Leaders$AB >= 50)
  runsData <- runsData %>% filter( (runs + 20) >= runsData$RunsCreated & (runs - 30) <= runsData$RunsCreated)
  runsData$HardHit. <- as.numeric(sub("%", "", runsData$HardHit.))
  runsData$InZoneWhiff. <- as.numeric(sub("%", "", runsData$InZoneWhiff.))
  runsData <- runsData[order(runsData$InZoneWhiff. , decreasing = FALSE),]
  plotRuns <- plot_ly() %>% add_trace(data = runsData, x = ~InZoneWhiff., y = ~Max.EV, color = ~`OPS` ,text = ~paste("Name: ", runsData$playerFullName, '<br>Runs Created: ', runsData$RunsCreated, '<br>Barrel%:', runsData$Barrel.), type = "scatter", mode = "markers")
  plotRuns <- plotRuns %>% add_lines(y = median(runsData$Max.EV), x = range(runsData$InZoneWhiff.),line = list(color = "red"), inherit = FALSE, showlegend = FALSE)
  plotRuns <- plotRuns %>% add_lines(y = range(runsData$Max.EV), x = median(runsData$InZoneWhiff.), line = list(color = "red"), inherit = FALSE, showlegend = FALSE)
  return(config(plotRuns, displayModeBar = FALSE))
}



