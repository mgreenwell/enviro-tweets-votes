# Script takes all tweets from both current and former MPs
# Removes all re tweets without quote
# Assign as either environmental or not environmental

#### Packages Required ####

library(tidyverse)

#### Read in Data ####

tweets2 <- readRDS("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Old\\all_tweets.rds")

#### Data Manipulation ####

# Get relevant columns and remove retweets - remove tweets without original content
tweets_subset <- tweets2 %>% 
  select(status_id, created_at, screen_name, name, text, is_retweet, followers_count, statuses_count, account_created_at) %>%
  filter(is_retweet == "FALSE") %>%
  select(status_id, created_at, screen_name, name, text, followers_count, statuses_count, account_created_at)

# tweets_subset <- tweets %>% 
#   select(status_id, created_at, screen_name, name, text, is_quote, is_retweet, hashtags, retweet_text) %>%
#   filter(is_retweet != TRUE) %>%
#   select(status_id, created_at, screen_name, name, text)

# # Tweets that contain evnironmental terms
# tweets_subset$enviro = ifelse(grepl(
#   "environment|climate|nature|wildlife|conservation|carbon|emissions|global warming|renewable|flood|drought|ocean acidification|coral bleaching|wildfire|pollution|poaching|fossil fuel|insect decline|extinction|deforestation|microplastic|recycl|pollinat|sustain|biodivers|greenhouse", tweets_subset$text), 1, 0)
# 
# table(tweets_subset$enviro)

# Tweets that contain evnironmental terms
tweets_subset$enviro = ifelse(grepl(
  "emmissions|global warming|poaching|extinction|deforestation|greenhouse|pollinat|biodivers|climate|pollution|wildlife|carbon|renweable|flood|fossil fuel|Emmissions|Global warming|Poaching|Extinction|Deforestation|Greenhouse|Pollinat|Biodivers|Climate|Pollution|Wildlife|Carbon|Renweable|Flood|Fossil fuel", tweets_subset$text), 1, 0)

table(tweets_subset$enviro)

# Write to smaller CSV
write.csv(tweets_subset, "C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Tweet_data\\tweet_subset.csv", row.names = F)

# Create a summary data set - proportion of tweets that are environmental per person
tweet_summary1 <- tweets_subset %>%
  group_by(name) %>%
  summarise(total = n())

tweet_summary2 <- tweets_subset %>%
  group_by(name) %>%
  summarise(environmental = sum(enviro))

tweet_summary <- inner_join(tweet_summary1, tweet_summary2, by = "name")

tweet_summary <- tweet_summary %>% 
  mutate(proportion = environmental/total)

# Write to csv
write.csv(tweet_summary, "C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Tweet_data\\tweet_summary.csv", row.names = F)  
