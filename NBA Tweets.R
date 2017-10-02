library("twitteR")
library("ROAuth")

# Set API Keys
api_key <- "x"
api_secret <- "x"
access_token <- "x"
access_token_secret <- "x"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

search.string <- "#nba"
no.of.tweets <- 100

tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en", since="2017-06-20")
tweets.df <- twListToDF(tweets)

