http://rotoguru1.com/cgi-bin/fyday.pl?week=1&game=fd&scsv=1

dfs_history <- read.csv(url("http://rotoguru1.com/cgi-bin/fyday.pl?week=1&game=fd&scsv=1"))

rm(dfs_history)


library(devtools)

devtools::install_github("mbjoseph/bbr")

library(bbr)
getOKC <- get_season(2017)
str(getOKC)

get_players("W")
