#install.packages("lpSolveAPI")
library(lpSolveAPI)


find_teams <- function(train, cap, constraint = c("none", "all_diff", "no_opp"), 
                       league = c("FanDuel", "DraftKings"), setplayers = NULL, removeteams = NULL) {
  
  #colnames(train) <- c("Id", "Position", "FirstName", "LastName", "ExpectedPoints", "Salary", "Team", "Opponent")
  
  colnames(train) <- c("Id", "Name", "Salary", "Position", "Oppt", "Points", "PlayerStatus", "Team", "PosRank", "Value", "DFPoints", "DFPointsRank")
  
  ## set constraints to use
  defense <- ifelse(train$Position == "D", 1, 0)
  qb <- ifelse(train$Position == "QB", 1, 0)
  wr <- ifelse(train$Position == "WR", 1, 0)
  te <- ifelse(train$Position == "TE", 1, 0)
  rb <- ifelse(train$Position == "RB", 1, 0)
  k <- ifelse(train$Position == "K", 1, 0)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  lpfantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(lpfantasy, train$DFPoints)
  
  ## Make sure the decision variables are binary
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  ## Add some contraints
  ## Only select one defense, exactly 3 wide receivers, etc.
  ## Depends on what fantasy league you are playing in, currently for FanDuel and DraftKings
  if(league == "FanDuel") {
    add.constraint(lpfantasy, defense, "=", 1)
    add.constraint(lpfantasy, qb, "=", 1)
    add.constraint(lpfantasy, wr, "=", 3)
    add.constraint(lpfantasy, rb, "=", 2)
    add.constraint(lpfantasy, te, "=", 1)
    add.constraint(lpfantasy, k, "=", 1)
  }
  if(league == "DraftKings") {
    dk_total <- defense + qb + wr + rb + te + k
    add.constraint(lpfantasy, defense, "=", 1)
    add.constraint(lpfantasy, qb, "=", 1)
    add.constraint(lpfantasy, wr, "<=", 4)
    add.constraint(lpfantasy, rb, "<=", 3)
    add.constraint(lpfantasy, te, "<=", 2)
    add.constraint(lpfantasy, k, "=", 0)
    add.constraint(lpfantasy, dk_total, "=", 9)
  }
  
  ## DEPRECATED, but can uncomment if you want to impose the restrictions described on the next two lines
  ## Make sure not to select more than one WR, or more than one RB from a single team
  ## Constraint: make sure not to select a WR/TE, WR/RB, RB/TE combo from same team
  #for(i in 1:length(team_names)){
  #  position_check <- ifelse((train$Team == team_names[i] & 
  #                             (train$Position == "WR" | train$Position == "RB" | train$Position == "TE")), 1, 0)
  #  add.constraint(lpfantasy, position_check, "<=", 1)
  #}
  
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
      check <- ifelse(train$Position == "D", 0, check) 
      check <- ifelse((train$Team == team_names[i] & train$Position == "D"), 1, check)
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
      no_opposing <- ifelse(train$Position == "D", 0, no_opposing) 
      no_opposing <- ifelse((train$Team == train$Oppt[j] & train$Position == "D"), 1, no_opposing)
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
      if((sum(setplayers$Position == "WR") > 3) || (sum(setplayers$Position == "RB") > 2) || (sum(setplayers$Position == "QB") > 1) ||
         (sum(setplayers$Position == "TE") > 1) || (sum(setplayers$Position == "K") > 1) || (sum(setplayers$Position == "D") > 1))
        stop("One of your positions has too many players")
    }
    if(league == "DraftKings") {
      if((sum(setplayers$Position == "WR") > 4) || (sum(setplayers$Position == "RB") > 3) || (sum(setplayers$Position == "QB") > 1) ||
         (sum(setplayers$Position == "TE") > 2) || (sum(setplayers$Position == "K") > 0) || (sum(setplayers$Position == "D") > 1))
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
  team_select$TotalPoints <- sum(team_select$DFPoints)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  team
}


#################################################################
#################################################################

## Remove all injuries and non-desireable players first
## Change the 'Expected Points' column to the number of points you expect 
## the player to make this upcoming week and make sure to include up to date 
## matchup information (which teams will be playing eachother).
train <- team
#train <- read.csv("C:/Users/williarl/Documents/GameAnalysis/week3.csv", header = T)

## Returns the top ten teams with no constraints, subject to the max salary cap of 60,000
#test1 <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = NULL, removeteams = NULL)

## Some desirable constraints (don't play against yourself!)
#test2 <- find_teams(train, 60000, constraint = "no_opp", league = "FanDuel", setplayers = NULL, removeteams = NULL)

## Now restrict so that no two players can be on the same team AND
## offensive players do not play against the chosen defense
#test3 <- find_teams(train, 60000, constraint = "all_diff", league = "FanDuel", setplayers = NULL, removeteams = NULL)

## Keep Aaron Rodgers and Seattle Seahawks defense
#setplayers <- subset(train, (Id == 155 | Id == 181))
#test4 <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = setplayers, removeteams = NULL)



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
Lineup <- top_teams(train, 5, 60000, constraint = "none", league = "FanDuel")