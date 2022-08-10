library(tidyverse)

tweet_data <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\tweets_data.csv")

head(tweet_data)

tweet_data <- tweet_data %>%
  filter(enviro == 1)

tweet_sample <- tweet_data %>%
  sample_n(1000) %>%
  select(Account, text)

write.csv(tweet_sample, "C:\\Users\\dp005352\\Dropbox\\PhD\\PR_Machine\\Data\\tweet_sample.csv", row.names=F)
