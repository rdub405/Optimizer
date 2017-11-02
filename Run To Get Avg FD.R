setwd("C:/Users/williarl/Documents/R Training/NBA")

library(sqldf)
require(rvest)


#Distinct Players
url2 <- paste0("https://www.basketball-reference.com/leagues/NBA_2018_per_game.html")

html2 <- xml2::read_html(url2)
node <- rvest::html_node(html2, "table")
players <- rvest::html_table(node, header = TRUE)

duplicated_header_rows <- players$Rk == "Rk"
players <- players[!duplicated_header_rows, ]

players$MP <- as.integer(players$MP)
players$FG <- as.integer(players$FG)
players$`3P` <- as.integer(players$`3P`)
players$FT <- as.integer(players$FT)
players$TRB <- as.integer(players$TRB)
players$AST <- as.integer(players$AST)
players$STL <- as.integer(players$STL)
players$BLK <- as.integer(players$BLK)
players$TOV <- as.integer(players$TOV)

#Fandual calc
players$FD <- 2*players$FG+players$`3P`+players$FT+1.2*players$TRB+1.5*players$AST+3*players$BLK+3*players$STL-players$TOV

players[is.na(players$FD)] <- 0
###########################################################################################################################################
#Get players projections from rotogrinder
rotonba <- read.csv(url("https://rotogrinders.com/projected-stats/nba-player.csv?site=fanduel"), header=FALSE)
colnames(rotonba) <- c("Name","Salary","Team","Position","Oppt","Ceil","Floor","Points")
rotonba$Id <- rownames(rotonba)
rotonba$Value <- (rotonba$Points/rotonba$Salary)*1000
#reorder columns
rotonba <- rotonba[c(9,1,2,3,4,5,6,7,8,10)]
rotonba$PosRank <- rank(c(10))
#train <- rotonba 
#write.csv(rotonba, file = "C:/Users/williams/Documents/NBA/main_nba.csv", fileEncoding = "UTF-16LE")

#rotonba
###########################################################################################################################################
## change Phantom.js scrape file
url <- paste0("https://rotogrinders.com/schedules/nba")
lines <- readLines("scrape_final.js")
lines[1] <- paste0("var url ='", url ,"';")
writeLines(lines, "scrape_final.js")

## Download website
system("phantomjs scrape_final.js")

#Ussing css clss selector to select
# Webpage

gg <- read_html("1.html")

# Webpage

nba.odds <- html_nodes(gg,'#tschedules')
nba_odds <- html_table(nba.odds[[1]],fill = TRUE, header = TRUE)
#rename columns
names(nba_odds)[6]<-"OverUnder"
names(nba_odds)[7]<-"ProjectedPTS"
nba_odds

###########################################################################################################################

rotonba <- sqldf('select a.*, b.Time, b.Opponent, b.Line, b.OverUnder, b.ProjectedPTS, c.FD
                 from rotonba a
                 left join nba_odds b on a.Team = b.Team
                 left join players c on a.Name = c.Player and a.Position = c.Pos
                 where a.Points >= 12') 
rotonba[is.na(rotonba$FD)] <- 0
head(rotonba)


###########################################LineUP######################################################################
library(lpSolveAPI)

find_teams <- function(train, cap, constraint = c("none", "all_diff", "no_opp"), 
                       league = c("FanDuel", "DraftKings"), setplayers = NULL, removeteams = NULL) {
  
  colnames(train) <- c( "Id", "Name", "Salary", "Team", "Position", "Oppt", "Ceil", "Floor", "Points","Value","PosRank", "Time", "Opponent", "Line", "OverUnder", "ProjectedPTS", "FD")
  #colnames(train) <- c("Id", "Position", "PLAYER_NAME", "ExpectedPoints", "Salary", "Team", "Opponent")
  
  
  ## set constraints to use
  c <- ifelse(train$Position == "C", 1, 0)
  pg <- ifelse(train$Position == "PG", 1, 0)
  sg <- ifelse(train$Position == "SG", 1, 0)
  sf <- ifelse(train$Position == "SF", 1, 0)
  pf <- ifelse(train$Position == "PF", 1, 0)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  lpfantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(lpfantasy, train$FD)
  
  ## Make sure the decision variables are binary
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  ## Add some contraints
  ## Only select one defense, exactly 3 wide receivers, etc.
  ## Depends on what fantasy league you are playing in, currently for FanDuel and DraftKings
  if(league == "FanDuel") {
    add.constraint(lpfantasy, c, "=", 1)
    add.constraint(lpfantasy, pg, "=", 2)
    add.constraint(lpfantasy, sg, "=", 2)
    add.constraint(lpfantasy, sf, "=", 2)
    add.constraint(lpfantasy, pf, "=", 2)
  }
  if(league == "DraftKings") {
    dk_total <- c + pg + sg + sf + pf
    add.constraint(lpfantasy, c, "=", 1)
    add.constraint(lpfantasy, pg, "=", 1)
    add.constraint(lpfantasy, sg, "<=", 4)
    add.constraint(lpfantasy, sf, "<=", 3)
    add.constraint(lpfantasy, pf, "<=", 2)
    #add.constraint(lpfantasy, k, "=", 0)
    add.constraint(lpfantasy, dk_total, "=", 9)
  }
  
  ## Add monetary constraint, max salary for the team
  add.constraint(lpfantasy, train$Salary, "<=", cap)
  
  ## Set objective direction
  lp.control(lpfantasy, sense='max')
  
  team_names <- levels(factor(train$Team))
  constraint <- match.arg(constraint)
  if(constraint == "all_diff") {
    for(i in 1:length(team_names)) {
      ## label opponent of each player (what defense they are playing against)
      check <- ifelse(train$Oppt == team_names[i], 1, 0)
      ## label only that defense with a 1
      check <- ifelse(train$Position == "C", 0, check) 
      check <- ifelse((train$Team == team_names[i] & train$Position == "C"), 1, check)
      ## add the set of constraints
      add.constraint(lpfantasy, check, "<=", 1)
    }
  }
  
  if(constraint == "no_opp") {
    team_names <- levels(factor(train$Team))  
    for(i in 1:length(team_names)) {
      ## No more than two players from each team (including that team's defense)
      no_two <- ifelse(train$Team == team_names[i], 1, 0)
      add.constraint(lpfantasy, no_two, "<=", 2)
    }
    for(j in 1:nrow(train)) {
      no_opposing <- ifelse(train$Oppt == train$Team[j], 1, 0)
      no_opposing[j] <- 1
      ## To deal with defenses (since Team and Opponent are swtiched for defenses)
      no_opposing <- ifelse(train$Position == "C", 0, no_opposing) 
      no_opposing <- ifelse((train$Team == train$Oppt[j] & train$Position == "C"), 1, no_opposing)
      for(k in 1:nrow(train)) {
        out <- rep(0, nrow(train))
        out[j] <- 1
        out[k] <- no_opposing[k]
        add.constraint(lpfantasy, out, "<=", 1)
      }
    }
  }
  
  if(!is.null(setplayers)) {
    if(league == "FanDuel") {
      if((sum(setplayers$Position == "PG") > 2) || (sum(setplayers$Position == "SG") > 2) || (sum(setplayers$Position == "SF") > 2) ||
         (sum(setplayers$Position == "PF") > 2) || (sum(setplayers$Position == "C") > 1))
        stop("One of your positions has too many players")
    }
    if(league == "DraftKings") {
      if((sum(setplayers$Position == "PG") > 4) || (sum(setplayers$Position == "SG") > 2) || (sum(setplayers$Position == "SF") > 1) ||
         (sum(setplayers$Position == "PF") > 2) || (sum(setplayers$Position == "C") > 1))
        stop("One of your positions has too many players")
    }
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(setplayers)) {
      add.constraint(lpfantasy, ifelse(setplayers$Id[k] == train$Id, 1, 0), "=", 1)
    }
  }
  
  if(!is.null(removeteams)) {
    if(nrow(removeteams) != nrow(train))
      stop("Your team restrictions do not match the number of players included in the 'train' file")
    for(m in 1:ncol(removeteams)) {
      add.constraint(lpfantasy, removeteams[, m], "<=", 8)
    }
  }
  
  team <- data.frame(matrix(0, 1, ncol(train) + 2))
  colnames(team) <- c(colnames(train), "TeamSalary", "TotalPoints")
  
  ## Solve the model, if this returns 0 an optimal solution is found
  solve(lpfantasy)
  if(solve(lpfantasy) != 0)
    stop("Optimization failed at some step")
  
  ## Get the players on the team
  team_select <- subset(data.frame(train, get.variables(lpfantasy)), get.variables.lpfantasy. == 1)
  team_select$get.variables.lpfantasy. <- NULL
  team_select$TeamSalary <- sum(team_select$Salary)
  team_select$TotalPoints <- sum(team_select$FD)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  team
}

NumLineups <- 30

#train <- read.csv("C:/Users/williarl/Documents/RW/fantasylineup-master/fantasylineup-master/late.csv", header = T)
train <- rotonba


## Returns the top ten teams with no constraints, subject to the max salary cap of 60,000
#TopTeam <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = NULL, removeteams = NULL)


## Keep Player
#setplayers <- subset(train, (Id == 3))
#AroundPlayer <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = setplayers, removeteams = NULL)

###Teams to Keepl
## Keep Player
#setTeams <- subset(train, (Id == 3))
#AroundTeam <- find_teams(train, MaxSalary, constraint = "none", league = "FanDuel", setplayers = setplayers, removeteams = NULL)

## Small function to generate the top set of teams
## All arguments are the same, except you must enter the number of top teams to return - 'num_top'

top_teams <- function(train, num_top, cap, constraint, league, setplayers = NULL) {
  result <- find_teams(train, cap, constraint = constraint, league = league, setplayers = setplayers)
  restrict <- as.matrix(rep(0, nrow(train)))
  restrict[match(result$Id, train$Id), 1] <- 1
  j <- 1
  
  while(j < num_top) {
    resultnew <- find_teams(train, cap, constraint = constraint, league = league, setplayers = setplayers, removeteams = restrict)
    restrict <- cbind(restrict, rep(0, nrow(restrict)))
    restrict[match(resultnew$Id, train$Id), j] <- 1
    result <- rbind(result, resultnew)
    j <- j + 1
  }
  
  TeamNumber <- rep(1:num_top, each = 9)
  result <- cbind.data.frame(result, TeamNumber)
  result
}

## Generate the top 10 teams with no constraints (this may be a bit slow with other constraints)
lineups <- top_teams(train, NumLineups, 60000, constraint = "none", league = "FanDuel")

write.csv(lineups, file = "C:/Users/williarl/Documents/AustinDojo/main_nba_lineup.csv")

#express <-write.csv(lineups, file="C:/Users/williarl/Documents/RW/optimizer/express.csv")

expo <- sqldf('select Name, count(Name) as "NameCount"
              from lineups
              group by Name
              order by NameCount desc')

expo$Percent <- (expo$NameCount/max(lineups$TeamNumber))*100


expo
