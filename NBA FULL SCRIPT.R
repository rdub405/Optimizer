library(rvest)
library(dplyr)
library(sqldf)
library(zoo)
###############################################GET PLAYERS####################################
url <- paste0("https://www.basketball-reference.com/leagues/NBA_2017_per_game.html")
html <- xml2::read_html(url)
node <- rvest::html_node(html, "table")
table <- rvest::html_table(node, header = TRUE)


html <- xml2::read_html(url)
node <- rvest::html_node(html, "table")
table <- rvest::html_table(node, header = TRUE)
duplicated_header_rows <- table$Rk == "Rk"
players <- table[!duplicated_header_rows, ]

players <- select(players, 'Player', 'Pos', 'Age', 'Tm')
names(players)[2]<-"Position"
names(players)[4]<-"Team"


###################################################GAME LOGS#################################

url_log <- paste0("https://www.basketball-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=2017&year_max=2017&is_playoffs=N&age_min=0&age_max=99&season_start=1&season_end=-1&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&order_by=player")

html_log <- xml2::read_html(url_log)
node_log <- rvest::html_node(html_log, "table")
table_log <- rvest::html_table(node_log, header = TRUE)

duplicated_header_rows_log <- table_log$Rk == "Rk"
table_log <- table_log[!duplicated_header_rows_log, ]
#converted <- lapply(table_log, maybe_as_numeric)
#converted <- lapply(converted_log, empty_string_to_na)
df <- as.data.frame(table_log, stringsAsFactors = FALSE)
df <- df[, !(names(df) == "Rk")] # remove "Rank" column
names(df) <- gsub("\\.", "_pct", names(df))
names(df) <- gsub("X2", "two_", names(df))
names(df) <- gsub("X3", "three_", names(df))
names(df) <- tolower(names(df))
names(df)[6]<-"H_A"
names(df)[8]<-"W_L"

df <- select(df, 'player', 'date', 'tm', 'H_A', 'opp', 'W_L','gs', 'mp','fg','3p','ft','trb','ast','stl','blk','tov')
df$date <- as.Date(df$date)
df$mp <- as.integer(df$mp)
df$fg <- as.integer(df$fg)
df$`3p` <- as.integer(df$`3p`)
df$ft <- as.integer(df$ft)
df$trb <- as.integer(df$trb)
df$ast <- as.integer(df$ast)
df$stl <- as.integer(df$stl)
df$blk <- as.integer(df$blk)
df$tov <- as.integer(df$tov)
#Fandual calc
df$FD <- 2*df$fg+df$`3p`+df$ft+1.2*df$trb+1.5*df$ast+3*df$blk+3*df$stl-df$tov

#rename columns
names(df)[3]<-"Team"
names(df)[5]<-"Oppt"
names(df)[7]<-"GAME_START"
names(df)[8]<-"MINUTES"
names(df)[12]<-"REBOUND"

df$H_A <- ifelse(df$H_A == '@', 'A', 'H')

#Get the moving average of the past 3 games
avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

res <- df %>% group_by(player) %>% arrange(date) %>%
  mutate(Avg3.Pts=avg.last.3(FD)) %>%
  ungroup() %>% arrange(player,date)
df <- res[,c(colnames(res)[1:17], "Avg3.Pts")]

df$over_proj <- ifelse(df$FD < df$Avg3.Pts, "Under", "Over")   

df$Projected <- df$Avg3.Pts
df$Avg3.Pts <- NULL
df$Projected <-format(round(df$Projected, 2), nsmall = 2)
df$DIFF <- (df$FD-  as.numeric(df$Projected))




################################################JOIN PLAYER AND GAMELONG###################################################
 df2 <- sqldf('select a.*, b.Position 
             from df a
              left outer join players b on a.player = b.player and a.Team = b.Team
            ')





