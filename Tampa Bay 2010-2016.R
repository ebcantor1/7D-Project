setwd("~/Dropbox/Tampa Bay 2010-2016/")
library(lubridate); library(data.table); library(dplyr); options(dplyr.width = Inf)

### Read TEAM Data ###
#All Strengths and Score - TEAM
dt.ALL.S.S <- read.csv("TBL TEAM.ALL 2010.16.csv", stringsAsFactors = FALSE)
dt.ALL.S.S$Date <- as.Date(dt.ALL.S.S$Date)

# Limit columns
dsubT.ALL.S.S <- subset(dt.ALL.S.S, select=c("Date","TOI","GF","GA","PDO"))

#Rename columns
names(dsubT.ALL.S.S) <- c("Date","TOI.ALL.S.S","GF.ALL.S.S", "GA.ALL.S.S", "PDO.ALL.S.S")


#5v5 Score Adjusted - TEAM
dt.55.ADJ <- read.csv("TBL TEAM.55adj 2010.16.csv", stringsAsFactors = FALSE)
dt.55.ADJ$Date <- as.Date(dt.55.ADJ$Date)

# Limit columns
dsubT.55.ADJ <- subset(dt.55.ADJ, select=c("Date","TOI","CF","CA","FF","FA","xGF","xGA","SCF","SCA","CF60","CA60",
                                           "CF.","FF60","FA60","FF.","xGF60","xGA60","xGF.","SCF60","SCA60","SCF."))

#Rename columns
names(dsubT.55.ADJ) <- c("Date","TOI","CF.55.ADJ","CA.55.ADJ","FF.55.ADJ","FA.55.ADJ","xGF.55.ADJ","xGA.55.ADJ",
                         "SCF.55.ADJ","SCA.55.ADJ","CF60.55.ADJ","CA60.55.ADJ","CFP.55.ADJ","FF60.55.ADJ","FA60.55.ADJ",
                         "FFP.55.ADJ","xGF60.55.ADJ","xGA60.55.ADJ","xGFP.55.ADJ","SCF60.55.ADJ","SCA60.55.ADJ","SCFP.55.ADJ")

#Merge TEAM data
dteam <- inner_join(dsubT.ALL.S.S, dsubT.55.ADJ, by = "Date")


### Read PLAYER data ###
#5v5 Score Adjusted - PLAYER
dp <- read.csv("TBL PLAYER.55adj 2010.16.csv", stringsAsFactors=FALSE)
dp$Date <- as.Date(dp$Date)

#Limit columns
dsubP <- subset(dp, select=c("Player", "Position", "Date", "TOI", "CF","CA","FF","FA","xGF","xGA","CF60","CA60",
                            "CF.","FF60","FA60","FF.","xGF60","xGA60","xGF."),drop=TRUE)

#Rename columns
names(dsubP) <- c("Player", "Position", "Date", "TOI", "CF","CA","FF","FA","xGF","xGA","CF60","CA60",
                  "CFP","FF60","FA60","FFP","xGF60","xGA60","xGFP")


#Subset defensive players
theDefense <- dsubP[ dsubP$Position == "D", ]

#Function that counts rostered defensemen on gamedate
countDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]        # Limit to gamedate
  thisGame <- unique(thisGame)
  if (nrow(thisGame)==6) {
    return("Six")
  }
  if (nrow(thisGame)==7) {
    return("Seven")
  }
  return("Error")
}

#Function that finds MEAN TOI for rostered defensemen on gamedate
meanTOIDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  thisGame <- unique(thisGame)
  return(mean(thisGame$TOI))
}

#Function that finds MAX TOI for rostered defensemen on gamedate
maxTOIDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  thisGame <- unique(thisGame)
  return(max(thisGame$TOI))
}

#Function that finds SD of TOI for rostered defensemen on gamedate
SDTOIDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  thisGame <- unique(thisGame)
  return(sd(thisGame$TOI))
}

#Function that finds MIN TOI for rostered defensemen on gamedate
minTOIDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  thisGame <- unique(thisGame)
  return(min(thisGame$TOI))
}

#Function that finds mean xGF for defense on gamedate
xGFDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGF))
}

#Function that finds mean xGA for defense on gamedate
xGADefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGA))
}

#Function that finds mean xGF60 for defense on gamedate
xGF60Defense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGF60))
}

#Function that finds mean xGA60 for defense on gamedate
xGA60Defense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGA60))
}

#Function that finds mean CFP for defense on gamedate
meanCFPDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$CFP))
}

#Function that finds mean FFP for defense on gamedate
meanFFPDefense <- function(gamedate) {
  thisGame <- theDefense[ theDefense$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$FFP))
}

#Function that finds MEAN TOI for top 6 offensemen on gamedate
meanTOIOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$TOI))
}

#Function that finds SD of TOI for top 6 offensemen on gamedate
SDTOIOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(sd(thisGame$TOI))
}

#Function that finds MIN TOI for rostered offensemen on gamedate
minTOIOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(min(thisGame$TOI))
}

#Function that finds MAX TOI for rostered offensemen on gamedate
maxTOIOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(max(thisGame$TOI))
}

#Function that finds mean xGF for top 6 offensemen on gamedate
xGFOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGF))
}

#Function that finds mean xGA for top 6 offensemen on gamedate
xGAOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGA))
}

#Function that finds mean xGF60 for top 6 offensemen on gamedate
xGF60Offense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGF60))
}

#Function that finds mean xGA60 for top 6 offensemen on gamedate
xGA60Offense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$xGA60))
}

#Function that finds mean CFP for top 6 offensemen on gamedate
meanCFPOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$CFP))
}

#Function that finds mean FFP for top 6 offensemen on gamedate
meanFFPOffense <- function(gamedate) {
  thisGame <- top6[ top6$Date == gamedate, ]      # Limit to gamedate
  return(mean(thisGame$FFP))
}

#testset[date %between% c("2013-08-02", "2013-11-01")]
#Function that gets season from game date
getSeason <- function(gamedate) {
  thisGame <- dteam[ dteam$Date == gamedate, ]
  if(thisGame$Date %between% c("2010-10-01", "2011-07-01")) {
      return("2010-2011")
}
  if(thisGame$Date %between% c("2011-10-01", "2012-07-01")) {
    return("2011-2012")
}  
  if(thisGame$Date %between% c("2012-10-01", "2013-07-01")) {
    return("2012-2013")
  }   
  if(thisGame$Date%between% c("2013-10-01", "2014-07-01")) {
    return("2013-2014")
  }      
  if(thisGame$Date%between% c("2014-10-01", "2015-07-01")) {
    return("2014-2015")
  }  
  if(thisGame$Date%between% c("2015-10-01", "2016-07-01")) {
    return("2015-2016")
  }  
  return("Error")  
}

### New function that limits to top 6 offensive players on each gamedate
topOffense <- function(gamedate) {
  thisGame <- dsubP[dsubP$Date == gamedate, ]      # Limit to gamedate
  theoffense <- thisGame[ thisGame$Position != "D", ]  # Limit to offense
  theoffense <- unique(theoffense)
  theoffense <- theoffense[order(-theoffense$TOI), ]
  return(theoffense[1:6, ])
}

# Get the regular season game dates
gameDates <- unique(dsubP$Date)
gameDates <- as.character(gameDates)

####  Use a loop to build d.f. with top 6 offensive players for each gamedate
top6 <- NULL
for (d in gameDates) {
  fromthisdate <- topOffense(d)
  top6 <- rbind(top6, fromthisdate)
}

# Create new data frame for things we want to see
PlayerTable <- data.frame(Date=gameDates, 
                          Season = sapply(gameDates, getSeason),
                          Defense.Count = sapply(gameDates, countDefense), 
                          Defense.MeanTOI = sapply(gameDates, meanTOIDefense),  
                          Defense.SDTOI = sapply(gameDates, SDTOIDefense),
                          Defense.MinTOI = sapply(gameDates, minTOIDefense),
                          Defense.MaxTOI = sapply(gameDates, maxTOIDefense),
                          Defense.xGF = sapply(gameDates, xGFDefense),
                          Defense.xGA = sapply(gameDates, xGADefense),
                          Defense.xGF60 = sapply(gameDates, xGF60Defense),
                          Defense.xGA60 = sapply(gameDates, xGA60Defense),
                          Defense.MeanCFP = sapply(gameDates, meanCFPDefense),
                          Defense.MeanFFP = sapply(gameDates, meanFFPDefense),
                          Offense.MeanTOI = sapply(gameDates, meanTOIOffense),
                          Offense.SDTOI = sapply(gameDates, SDTOIOffense),
                          Offense.MinTOI = sapply(gameDates, minTOIOffense),
                          Offense.MaxTOI = sapply(gameDates, maxTOIOffense),
                          Offense.xGF = sapply(gameDates, xGFOffense),
                          Offense.xGA = sapply(gameDates, xGAOffense),
                          Offense.xGF60 = sapply(gameDates, xGF60Offense),
                          Offense.xGA60 = sapply(gameDates, xGA60Offense),
                          Offense.MeanCFP = sapply(gameDates, meanCFPOffense),
                          Offense.MeanFFP = sapply(gameDates, meanFFPOffense)
)

PlayerTable$Date <- as.Date(PlayerTable$Date)

### Merge Player and Team Data ###
TBFINAL1016 <- inner_join(PlayerTable, dteam, by="Date")


# Cumulative Stats #
TBFINAL1016$tC60 <- as.numeric(TBFINAL1016$CF60.55.ADJ +  TBFINAL1016$CA60.55.ADJ)  #Total Corsi/60
TBFINAL1016$tF60 <- as.numeric(TBFINAL1016$FF60.55.ADJ +  TBFINAL1016$FA60.55.ADJ)  #Total Fenwick/60
TBFINAL1016$tSC60 <- as.numeric(TBFINAL1016$SCF60.55.ADJ +  TBFINAL1016$SCA60.55.ADJ)  #Total SC/60
TBFINAL1016$GDiff.ALL.S.S <- as.numeric(TBFINAL1016$GF.ALL.S.S - TBFINAL1016$GA.ALL.S.S) #Goal Diff
TBFINAL1016$WLT <- ifelse(TBFINAL1016$GDiff.ALL.S.S>0 
                         & TBFINAL1016$GDiff.ALL.S.S != 0, 1,     
                         ifelse(TBFINAL1016$GDiff.ALL.S.S< 0, -1, 0)) #WLT Points -- (1W . 0T . -1L)


# Write file
write.csv(TBFINAL1016, "TBFINAL1016.CSV" )



######## 6 and 7 Defensmen Subsets##########
SIXDefense <- subset(TBFINAL1016, TBFINAL1016$Defense.Count == "Six")
SEVENDefense <- subset(TBFINAL1016, TBFINAL1016$Defense.Count == "Seven")