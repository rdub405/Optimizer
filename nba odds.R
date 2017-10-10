setwd("C:/Users/williarl/Documents/R Training/NBA")

options(stringsAsFactors = FALSE)
require(rvest)

  
  ## change Phantom.js scrape file
  url <- paste0("https://rotogrinders.com/schedules/nba")
  lines <- readLines("scrape_final.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "scrape_final.js")
  
  ## Download website
  system("phantomjs scrape_final.js")
  
  #Ussing css clss selector to select
  # Webpage
  
  pg <- read_html("1.html")
  
  # Webpage
  
  nba.odds <- html_nodes(pg,'#tschedules')
  nba_odds <- html_table(nba.odds[[1]],fill = TRUE, header = TRUE)
  #rename columns
  names(nba_odds)[6]<-"OverUnder"
  names(nba_odds)[7]<-"ProjectedPTS"
  nba_odds

  #Summary over under
  TeamLine <- sqldf('select a.Abbr, a.LongName, b.Time, b.Opponent, b.Line, b.OverUnder, b.ProjectedPTS
                  from Team a
                  inner join nba_odds b on a.Abbr = b.Team')
  
  
  