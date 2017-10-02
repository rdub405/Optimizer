library("twitteR")
library("ROAuth")

# Set API Keys
api_key <- "DTEcxRmKgDpPOsZrFq6LVdOtc"
api_secret <- "YSunJxmG4tQtktIwFfByDyNjkAmOEt14UcIjtu5YyUGaYbrK8a"
access_token <- "17455353-yUpMBFl4wS1UQ6OhiENozkVHLdlr9Kz7i4sQKhLqd"
access_token_secret <- "Sdtb9pz24EJFZbrCYsgp839gN1AOVmH1DL5nkLeiS2MOi"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

search.string <- "#nba"
no.of.tweets <- 100

tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en", since="2017-06-20")
tweets.df <- twListToDF(tweets)

