# helpful article:
# https://www.kaggle.com/code/rtatman/tutorial-sentiment-analysis-in-r/notebook

# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/test_tweets.csv")
tweet_texts <- tweet_file$tweet_text

# tokenize
tokens <- data_frame(text = tweet_texts[1]) %>% unnest_tokens(word, text)

tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)  %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds


