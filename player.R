# Set the working directory
setwd("C:/Users/williarl/Documents/GameAnalysis")


library(sqldf)
library(zoo)
library(dplyr)

head(df)

df$SEASON_ID <-NULL
df$PLAYER_ID <- NULL
df$TEAM_ID <- NULL
df$GAME_ID <- NULL
df$VIDEO_AVAILABLE <- NULL
df$PLUS_MINUS <- NULL
head(df)

player <- sqldf('select * from df where player_name ="Damian Lillard"')
#filter to only russell westbrook from df
westbrook <- df[which(df$PLAYER_NAME == 'Russell Westbrook'),]

player <- sqldf('SELECT PLAYER_NAME
,TEAM_ABBREVIATION
,GAME_DATE
,MATCHUP
,MIN
,FGM
,FG3M
,FTM
,REB
,AST
,BLK
,STL
,TOV
FROM player
order by GAME_DATE asc')

player$MIN <- as.integer(player$MIN)
player$FG3M <- as.integer(player$FG3M)
player$FTM <- as.integer(player$FGM)
player$GAME_DATE <- as.Date(player$GAME_DATE)

str(player)

player$FD <- 2*player$FGM+player$FG3M+player$FTM+1.2*player$REB+1.5*player$AST+2*player$BLK+2*player$STL-player$TOV

#add column and add if game was home or away
player$home_away <- ifelse(grepl(" @ ", player$MATCHUP), 'A', 'H')

#Get Oppt
player$Left <- substr(player$MATCHUP, 1,3)
player$Right <- substr(player$MATCHUP,7, 12)

player$Left <- NULL
player$OPPT <- gsub("\\.", "", player$Right)
player$Right <- NULL
head(player)


#Changing var to factor
player$home_away <- as.factor(player$home_away)

player <- player[order(player$GAME_DATE, player$PLAYER_NAME),]



#avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

#res <- player %>% group_by(PLAYER_NAME) %>% arrange(GAME_DATE) %>%
#  mutate(Avg3.Pts=avg.last.3(FD)) %>%
#  ungroup() %>% arrange(PLAYER_NAME,GAME_DATE)
#player <- res[,c(colnames(res)[1:31], "Avg3.Pts")]

#player$over_proj <- ifelse(player$FD < player$Avg3.Pts, "Over", "Under")    


#write.csv(player, file = "foo.csv", fileEncoding = "UTF-16LE")


