# This script takes the project data with sentiment scores per tweet and does the following:
# - weights each score by like and retweet; each like and retweet adds the orignial score again to the tweet
# - groups tweets by incident index and before or after marker and gives a total score for each grouping
# - outputs two csv files
# - one file with all the tweets and weighted sentiment
# - second file with one row for each incident-time-period grouping (e.g., 1-before, 1-after, 2-before, 2-after,...)

library(tidyverse)

tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/all_tweets_with_sentiment.csv")

for(row in 1:nrow(tweet_file)) {
  num_favorites <- as.numeric(tweet_file[row, "favorites"])
  num_retweets <- as.numeric(tweet_file[row, "retweets"])
  score <- as.numeric(tweet_file[row, "sentiment_score"])

  weighted_score <- score + score * num_favorites + score * num_retweets
  tweet_file[row, "weighted_score"] <- weighted_score
}

write_csv(tweet_file, "/Users/joehoskisson/rprojects/social_media_censor_project/all_tweets_with_weighted_sentiment.csv")

# row 5289 creates a problem. All after it are NAs
# that row messes up in the first sentiment score analysis