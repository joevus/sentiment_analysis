# helpful article:
# https://www.kaggle.com/code/rtatman/tutorial-sentiment-analysis-in-r/notebook

# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

tweet_file <- read.csv("/Users/joehoskisson/rprojects/social_media_censor_project/egypt_tweets.csv")
tweet_texts <- tweet_file$translated

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

for(tweet_text in tweet_texts) {
  print(generate_sentiment_score(tweet_text))
}


# create data frame with day column and score column
egypt_days <- data.frame(matrix(ncol = 2, nrow = 0))
names(egypt_days) <- c("date", "sentiment_score")

# iterate over tweets
for(i in 1:nrow(tweet_file)) {
  tweet_date <- tweet_file[i,2]
  tweet_content <- tweet_file[i,4]
  # check date
  date_exists <- any(egypt_days$post_date == tweet_date)

  # add row with date when date is not in data
  clean_date <- substr(tweet_date, 1, 10)
  if(!date_exists) {
    egypt_days[nrow(egypt_days) + 1,] <-  c(clean_date, 0)
  }
  # calculate sentiment score for tweet text
  current_sentiment_score <- generate_sentiment_score(tweet_content)
  # add score to existing score for that date
  date_row <- which(egypt_days$date == clean_date)
  previous_sentiment_score <- egypt_days$sentiment_score[date_row]
  previous_sentiment_score <- as.numeric(previous_sentiment_score)
  egypt_days$sentiment_score[date_row] <- current_sentiment_score + previous_sentiment_score
}

write.csv(egypt_days,"/Users/joehoskisson/rprojects/social_media_censor_project/output.csv", row.names = FALSE)
