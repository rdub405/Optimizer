library(plyr)
library(dplyr)
library(sqldf)
library(data.table)
library(stringi)
library(zoo)

#Get Player Table from http://www.basketball-reference.com/leagues/NBA_2017_totals.html#totals_stats::none
setwd("~/PythonProjects")

df <- read.csv("gamelog.csv", header=TRUE)
#df <- gamelog
#Create nba teams table
nba_teams <- data.table(team_abbreviation=df$TEAM_ABBREVIATION, team_name=df$TEAM_NAME)
#remove duplicated rows
nba_teams <- sqldf('SELECT DISTINCT
                  case when team_abbreviation = "BRK" then "BKN"
                   when team_abbreviation = "CHO" then "CHA"
                   when team_abbreviation = "PHO" then "PHX"
                    else team_abbreviation end as team_abbreviation
                   ,team_name
                   FROM nba_teams 
                   WHERE team_name is not null ORDER BY team_abbreviation')

#clean up team table
#new_team <- sqldf('SELECT DISTINCT p.team FROM players p ORDER BY p.team')
#check_team <- sqldf('SELECT a.*, b.* FROM nba_teams a left join new_team b on a.team_abbreviation = b.Team')
players <- sqldf('SELECT 
                  Players
                  ,Position
                  ,Age
                  ,case when Team = "BNK" then "BKN"
	                 when Team = "CHO" then "CHA"
	                 when Team = "PHO" then "PHX"	 
	                  else Team end as Team
                    FROM players p')
#new game log table
df <- sqldf('SELECT 
                 p.Position
                ,p.age
                ,g.*
                FROM gamelog g
                INNER JOIN players p on g.Player_Name = p.Players and g.team_abbreviation = p.Team
                where game_date is not null
                order by g.Game_Date')
#FD Points Column
df$FD <- 2*df$FGM+df$FG3M+df$FTM+1.2*df$REB+1.5*df$AST+2*df$BLK+2*df$STL-df$TOV

#sorting column
df <- df[order(df$PLAYER_NAME, df$GAME_DATE),]
#df <- arrange(df,desc(df$GAME_DATE))

#remove columns
df$SEASON_ID <- NULL
df$PLAYER_ID <- NULL
df$home <- NULL
df$away <- NULL

#add column and add if game was home or away
df$home_away <- ifelse(grepl(" @ ", df$MATCHUP), 1, 0)

#new oppt column
df$oppt <- stri_sub(df$MATCHUP,-3,-1)

#df$Avg.Pts <- NULL

# avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)
# 
# res1 <- df %>% group_by(PLAYER_NAME) %>% arrange(GAME_DATE) %>%
#   mutate(Avg.Pts=avg.last.3(FD)) %>%
#   ungroup() %>% arrange(PLAYER_NAME, GAME_DATE)
# 
# df <- res1[,c(colnames(res1)[1:33], "Avg.Pts")]





