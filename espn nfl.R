burl = "http://games.espn.go.com/ffl/tools/projections?"

espn_base_url <- burl

espn_pages <- seq(0,960,40)
espn_urls <- paste0(espn_base_url, "&startIndex=", espn_pages)

loadESPN <- function(url) {
  
  df = readHTMLTable(url, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
  colnames(df) <- df[1,]
  colnames(df) <- c("PLAYER, TEAM POS", "OPP", "STATUS ET", "C/A", "PASSING YDS", "PASSING TDS", "PASSING INT", "RUSH", "RUSH YDS", "RUSH TD", "REC", 'REC YDS', 'REC TD', 'PTS')
  df <- df[2:nrow(df),]
  df$Player = sapply(strsplit(df[[1]], ","), function(x) { 
    name <- unlist(x)[[1]];
    
    name <- gsub("\\*","", name);
    name <- gsub("D/ST","",name);
    name <- gsub("^\\s+|\\s+$", "", name)
    return( name ); 
  });
  df$Team = sapply(strsplit(df[["PLAYER, TEAM POS"]], ","), function(x) { 
    if(length(x) > 1) {
      el <- unlist(strsplit(x[[2]][[1]], " ")); 
      return( el[[1]] );
    } else {
      return( unlist(strsplit(x[[1]][[1]], " "))[[1]] );
    }
    return(''); 
  });
  df$PlayerStatus = sapply(strsplit(df[["PLAYER, TEAM POS"]], ","), function(x) { 
    if(length(x) > 1) {
      el <- unlist(strsplit(x[[2]][[1]], " ")); 
      
      if(length(el) > 2) { return( el[[4]] ); }
    }
    return('');
  });
  return(df);
}

data <- lapply(espn_urls, loadESPN)
result= do.call("rbind", data)

Data = result