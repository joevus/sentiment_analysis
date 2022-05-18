# This script takes the project data with sentiment scores per tweet and does the following:
# - weights each score by like and retweet; each like and retweet adds the orignial score again to the tweet
# - groups tweets by incident index and before or after marker and gives a total score for each grouping
# - outputs three csv files
# - one file with all the tweets and weighted sentiment
# - second file with one row for each incident-time-period grouping (e.g., 1-before, 1-after, 2-before, 2-after,...) and the total sentiment score for each group
# - third file with same incident-time-period grouping but with weighted sentiment totals for each group

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

sentiment_sums <- tweet_file %>%
  group_by(incident_index, type) %>%
  summarise(total_sentiment=sum(sentiment_score))

write_csv(sentiment_sums, "~/rprojects/social_media_censor_project/incident_sentiment_totals.csv")

weighted_sentiment_sums <- tweet_file %>%
  group_by(incident_index, type) %>%
  summarise(total_weighted_sentiment=sum(weighted_score))

write_csv(weighted_sentiment_sums, "~/rprojects/social_media_censor_project/incident_weighted_sentiment_totals.csv")
