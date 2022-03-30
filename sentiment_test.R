# helpful article:
# https://www.kaggle.com/code/rtatman/tutorial-sentiment-analysis-in-r/notebook

# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/test_tweets.csv")
tweet_texts <- tweet_file$tweet_text

ensure_two_sentiment_categories <- function(count_data) {
  df_negative <- data.frame(sentiment=c("negative"), n=c(0))
  df_positive <- data.frame(sentiment=c("positive"), n=c(0))
  
  if(!any(count_data == "negative")) {
    count_data <- rbind(count_data, df_negative)
  }
  
  if(!any(count_data == "positive")) {
    count_data <- rbind(count_data, df_positive)
  }
  View(count_data)
  return(count_data)
}

# tokenize
tokens <- data_frame(text = tweet_texts[1]) %>% unnest_tokens(word, text)

tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>% # count the # of positive & negative words
  ensure_two_sentiment_categories() %>%
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words


