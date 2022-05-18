# helpful article:
# https://www.kaggle.com/code/rtatman/tutorial-sentiment-analysis-in-r/notebook

# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/read_and_write_all_tweets.csv")
tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/for_sentiment_analysis_all_tweets.csv")

ensure_two_sentiment_categories <- function(count_data) {
  df_negative <- data.frame(sentiment=c("negative"), n=c(0))
  df_positive <- data.frame(sentiment=c("positive"), n=c(0))
  
  if(!any(count_data == "negative")) {
    count_data <- rbind(count_data, df_negative)
  }
  
  if(!any(count_data == "positive")) {
    count_data <- rbind(count_data, df_positive)
  }
  return(count_data)
}

generate_sentiment_score <- function(text_data) {
  # tokenize
  tokens <- data_frame(text = text_data) %>% unnest_tokens(word, text)

  tokens %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>% # count the # of positive & negative words
    ensure_two_sentiment_categories() %>%
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) # # of positive words - # of negative words
}

for(row in 1:nrow(tweet_file)) {
  score_table <- generate_sentiment_score(tweet_file[row, "translated"])

  if(length(score_table[1,]) > 3) {
    print("length of score_table")
    print(length(score_table))
    print("nrow")
    print(row)
    break
  }

  tweet_file[row, "sentiment_score"] <- score_table$sentiment

  if(length(tweet_file[row,]) > 10) {
    print("length of tweet_file at current row")
    print(length(tweet_file[row,]))
    print("current row")
    print(row)
    print("tweet_file[row,]")
    print(tweet_file[row,])
    print("score_table")
    print(score_table)
    break
  }
}

write_csv(tweet_file, "/Users/joehoskisson/rprojects/social_media_censor_project/read_and_write_all_tweets2.csv")


# problem rows all_tweets_with_sentiment.csv 15:17
# 2392 - userid 1142023032
# 5793 - userid 565859934
# 6479 - userid 149264357

# problem rows read_and_write_all_tweets.csv 16:00
# 3298 - userid 774512321351807000
# 8663 - userid 1651411382

# problem rows read_and_write_all_tweets.csv 16:46
# 4029 - userid 255923009

# problem rows read_and_write_all_tweets.csv 17:04
# no problem rows! could use this?
