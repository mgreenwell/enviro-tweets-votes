######################### General Workthrough Script #########################


#### 1 Packages Required ####
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(lubridate)
#library(lemon)
library(ggpubr)
library(gridExtra)
library(scales)
library(ggeffects)
library(ggwordcloud)
library(tm)
library(SnowballC)

#### 2 Read in Data ####

tweet_data <- read.csv("C:\\Users\\mgree\\OneDrive\\Documents\\test\\tweets_data.csv")

#vote_data <- read.csv("C:\\Users\\mgree\\OneDrive\\Documents\\test\\votes_data.csv")
#unique(vote_data$Name)
#proportions_data <- read.csv("C:\\Users\\mgree\\OneDrive\\Documents\\test\\proportions_data.csv")

#constituencies <- read.csv("C:\\Users\\mgree\\OneDrive\\Documents\\test\\constituencies_data.csv")



#### Exploratory plots data manipulation ####
plot1_data <- tweet_data %>%
  select(Party)
plot1_data <- as.data.frame(table(plot1_data))
plot1_data <- plot1_data %>% rename(Party = plot1_data)

plot2_data <- tweet_data %>%
  filter(enviro == 1) %>%
  select(Party)
plot2_data <- as.data.frame(table(plot2_data))
plot2_data <- plot2_data %>% rename(Party = plot2_data)


plot3_data <- left_join(plot1_data, plot2_data, by = "Party")
plot3_data <- plot3_data %>% mutate(Prop = Freq.y/Freq.x)


histogram1_data <- tweet_data %>%
  select(created_at, Date_Numeric, Party) %>%
  mutate(Date = Date_Numeric + 1970) %>%
  select(Date, Party)
histogram1_data$Date <- round(histogram1_data$Date, 1)
histogram1_data <- as.data.frame(table(histogram1_data))

histogram2_data <- tweet_data %>%
  filter(enviro == 1) %>%
  select(created_at, Date_Numeric, Party) %>%
  mutate(Date = Date_Numeric + 1970) %>%
  select(Date, Party)
histogram2_data$Date <- round(histogram2_data$Date, 1)
histogram2_data <- as.data.frame(table(histogram2_data))

head(histogram2_data)


#### 4 Plotting ####

# Conservatives  "#6AB023", 
# Labour   "#6AB023", 
# Lib Dems   "#6AB023", 
# SNP   "#6AB023", 
# DUP   "#6AB023", 


#### ~ 4.1 Proportions ####
options(scipen = 999)

# Plot coloured by party colours

# Conservatives  #0087DC
# Labour   #E4003B
# Lib Dems   #FAA61A 
# SNP   #FDF38E 
# DUP   "#D46A4C" 
tweet_vs_vote_proportion_plot <- 
  ggplot(proportions_data, aes(x=vote_score*100, y=(tweet_score*100))) + 
  geom_jitter(aes(color=Party), size = 4, alpha = 0.7, width = 1, height = 0) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  labs(x = "Pro-environmental votes (%)",
       y = "Pro-environmental tweets (%)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_log10(breaks = c(0.1, 1, 10), expand = c(0,0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.position = "none") 



votes_vs_followers_plot <- 
  ggplot(proportions_data, aes(x=vote_score*100, y=(followers_count))) + 
  geom_jitter(aes(color=Party), size = 4, alpha = 0.7, width = 1, height = 0) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  labs(x = "Pro-environmental votes (%)",
       y = "Twitter followers (N)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_log10(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.position = "none") 

tweet_vs_followers_plot <- 
  ggplot(proportions_data, aes(x=tweet_score*100, y=(followers_count))) + 
  geom_jitter(aes(color=Party), size = 4, alpha = 0.7, width = 1, height = 0) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  labs(x = "Pro-environmental tweets (%)",
       y = "Twitter followers (N)") +
  scale_x_log10(breaks = c(0.1, 1, 10), expand = c(0,0)) +
  scale_y_log10(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.position = "none") 


vote_proportion_plot <-
  ggplot(proportions_data, aes(x= vote_score, y=Party)) + 
  geom_boxplot(aes(fill = Party),color="black") +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  labs(x = "Proportion of environmental votes supported",
       y = "Party") +
  theme_classic() +
 # theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  coord_flip()

tweet_proportion_plot <-
  ggplot(proportions_data, aes(x=(tweet_score), y=Party)) + 
  geom_boxplot(aes(fill = Party), colour="black") +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) + 
  labs(x = "Proportion of tweets containing\n environmental term",
       y = "Party") +
  theme_classic() +
  #scale_x_log10(breaks = c(0.1, 1, 10)) +
  # theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  xlim(0.0, 0.05) +
  coord_flip() 
  

#### Exploratory plots  ####
plot1 <- ggplot(plot1_data, aes(x=reorder(Party, -Freq), y = Freq, fill = Party)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  theme_classic() +
  labs(y = "Number of Tweets (Total)",
       x = "Party") +
  theme(legend.position = "none") 

plot2 <- ggplot(plot2_data, aes(x=reorder(Party, -Freq), y = Freq, fill = Party)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  theme_classic() +
  labs(y = "Number of Environmental Tweets",
       x = "Party") +
  theme(legend.position = "none") 

plot3 <- ggplot(plot3_data, aes(x=reorder(Party, -Prop), y = Prop, fill = Party)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  theme_classic() +
  labs(y = "Proportion of Environmental Tweets",
       x = "Party") +
  theme(legend.position = "none") 


hist1 <- ggplot(histogram1_data, aes(Date, Freq, fill = Party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") 

hist2 <- ggplot(histogram2_data, aes(Date, Freq, fill = Party)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") 



tweet_vs_vote_proportion_plot
votes_vs_followers_plot
tweet_vs_followers_plot
vote_proportion_plot
tweet_proportion_plot

plot1
plot2
plot3

hist1
hist2

ggarrange(plot1, plot2, plot3, ncol = 1, nrow = 3)
ggarrange(tweet_vs_vote_proportion_plot, votes_vs_followers_plot, tweet_vs_followers_plot, ncol = 1, nrow = 3)
ggarrange(hist1, hist2, ncol = 1, nrow = 2)







#### ~ 4.3 Raw Data ####


#### ~ ~ 4.3.1 Votes ####

# Trends of votes over time
# Group by MP
votes_over_time <-
ggplot(vote_data) +
  geom_line(aes(x = Date_Numeric + 1970, y = vote_score, group = Name, colour = Party), 
            stat = "smooth", method = "glm", 
            method.args = list(family = "binomial"), 
            se = F, alpha = 0.1) +
  geom_smooth(aes(x = Date_Numeric + 1970, y = vote_score, group = Party, colour = Party), 
              method = "glm", method.args = list(family = "binomial"), se = F, size = 1.5) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(y = "Probability of voting pro (1) or against (0)\n environmentally favourable legislation",
       x = "Date") +
  theme_classic() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.position = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5)))

votes_over_time
# Group by Party
# votes_over_time_party <-
# ggplot(vote_data) +
#   geom_smooth(aes(x = Date_Numeric + 1970, y = vote_score, group = Party, colour = Party), 
#               method = "glm", method.args = list(family = "binomial"), se = F, size = 1) +
#   scale_color_manual(values=c("#0087DC", "#D46A4C", "#6AB023",  "#DC241f", "#FAA61A", "#008142", "#FDF38E", "#2AA82C")) +
#   # geom_vline(xintercept = as.numeric(as.Date("2010-05-06")), linetype = 2, size = 0.5) +
#   # geom_vline(xintercept = as.numeric(as.Date("2015-05-07")), linetype = 2, size = 0.5) +
#   # geom_vline(xintercept = as.numeric(as.Date("2017-06-08")), linetype = 2, size = 0.5) +
#   # geom_vline(xintercept = as.numeric(as.Date("2019-12-12")), linetype = 2, size = 0.5) +
#   scale_y_continuous(breaks = c(0, 1)) +
#   labs(y = "Probability of voting pro (1) or against (0)\n environmentally favourable legislation",
#        x = "Date") +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15)) +
#   theme(axis.text = element_text(size = 20)) +
#   theme(legend.position = "bottom") +
#   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))
# str(vote_data)

#### ~ ~ 4.3.2 tweets ####


#ggplot(tweet_model_data, aes(x = Date, y = enviro)) + geom_point()
#
head(tweet_data)
tweets_over_time <- ggplot(tweet_data) +
  geom_line(aes(x = Date_Numeric + 1970, y = enviro, 
                group = Account, colour = Party), 
              stat = "smooth", method = "glm", 
            method.args = list(family = "binomial"),
              se = F, alpha = 0.1) +  
  geom_smooth(aes(x = Date_Numeric + 1970, y = enviro, 
                  group = Party, colour = Party),
              method = "glm", method.args = list(family = "binomial"), se = F, size = 1.5) +  
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A",  "#FDF38E")) +
  theme_classic() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.position = "none") +
  labs(y = "Probability of tweet containing \n environmental term (1) or not (0)",
       x = "Date") +
  coord_cartesian(ylim = c(0.0, 0.05)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))


tweets_over_time

# tweets_over_time_party <- ggplot(tweet_plot_data) +
#   geom_smooth(aes(x = Date_Numeric + 1970, y = enviro, 
#                   group = Party, colour = Party),
#               method = "glm", method.args = list(family = "binomial"), se = F) +  
#   scale_color_manual(values=c("#0087DC", "#D46A4C", "#6AB023",  "#DC241f", "#FAA61A", "#008142", "#FDF38E", "#2AA82C")) +
#   #geom_vline(xintercept = as.numeric(as.Date("2010-05-06")), linetype = 2, size = 0.5) +
#   #geom_vline(xintercept = as.numeric(as.Date("2015-05-07")), linetype = 2, size = 0.5) +
#   #geom_vline(xintercept = as.numeric(as.Date("2017-06-08")), linetype = 2, size = 0.5) +
#   #geom_vline(xintercept = as.numeric(as.Date("2019-12-12")), linetype = 2, size = 0.5) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   labs(y = "Probability of tweet containing \n environmentalterm (1) or not (0)",
#        x = "Date") +
#   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))


votes_over_time
#votes_over_time_party
tweets_over_time
#tweets_over_time_party

ggarrange(tweets_over_time_party, tweet_proportion_plot)

#### Word Cloud ####

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

environmental <- tweet_data %>% filter(enviro == 1)
non_environmental <- tweet_data %>% filter(enviro == 0)

# All Tweets

#Characterise tweets data ----
tweet_corpus <- Corpus(VectorSource(tweet_data$text))
tweet_corpus <- tm_map(tweet_corpus, toSpace, "/")
tweet_corpus <- tm_map(tweet_corpus, toSpace, "@")
tweet_corpus <- tm_map(tweet_corpus, toSpace, "\\|")
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removeNumbers)
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
#tweet_corpus <- tm_map(tweet_corpus, stemDocument)

tweet_dtm <- TermDocumentMatrix(tweet_corpus)
tweet_dtm2 <- removeSparseTerms(tweet_dtm, sparse = 0.99)
tweet_matrix <- as.matrix(tweet_dtm2)
tweet_term_n <- sort(rowSums(tweet_matrix),decreasing=TRUE)
tweet_term_n <- data.frame(word = names(tweet_term_n),freq=tweet_term_n)
rownames(tweet_term_n) = NULL
head(tweet_term_n, 50)

remove_terms = c(
  "tco", "https","can","http","will","amp","take","get","sorri","now","busi","one","use", "'s"
)

trim_tweet_term_n = tweet_term_n[c(1:64),]
trim_tweet_term_n$word = as.character(trim_tweet_term_n$word)
trim_tweet_term_n = trim_tweet_term_n[!trim_tweet_term_n$word %in% remove_terms,]
trim_tweet_term_n$col = "grey"
trim_tweet_term_n$col= ifelse(trim_tweet_term_n$word == "forest", "darkgreen", trim_tweet_term_n$col)
trim_tweet_term_n$col= ifelse(trim_tweet_term_n$word == "planet", "darkgreen", trim_tweet_term_n$col)

plt_tweets_wordcloud = ggplot(trim_tweet_term_n, aes(label = word, size = freq, colour = col)) +
  scale_colour_manual(values = c("grey")) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 23) +
  theme_minimal()


#Characterise tweets data ----
non_environmental_corpus <- Corpus(VectorSource(non_environmental$text))
non_environmental_corpus <- tm_map(non_environmental_corpus, toSpace, "/")
non_environmental_corpus <- tm_map(non_environmental_corpus, toSpace, "@")
non_environmental_corpus <- tm_map(non_environmental_corpus, toSpace, "\\|")
non_environmental_corpus <- tm_map(non_environmental_corpus, content_transformer(tolower))
non_environmental_corpus <- tm_map(non_environmental_corpus, removeNumbers)
non_environmental_corpus <- tm_map(non_environmental_corpus, removeWords, stopwords("english"))
non_environmental_corpus <- tm_map(non_environmental_corpus, removePunctuation)
non_environmental_corpus <- tm_map(non_environmental_corpus, stripWhitespace)
#non_environmental_corpus <- tm_map(non_environmental_corpus, stemDocument)

non_environmental_dtm <- TermDocumentMatrix(non_environmental_corpus)
non_environmental_dtm2 <- removeSparseTerms(non_environmental_dtm, sparse = 0.99)
non_environmental_matrix <- as.matrix(non_environmental_dtm2)
non_environmental_term_n <- sort(rowSums(non_environmental_matrix),decreasing=TRUE)
non_environmental_term_n <- data.frame(word = names(non_environmental_term_n),freq=non_environmental_term_n)
rownames(non_environmental_term_n) = NULL
head(non_environmental_term_n, 50)

non_environmental_remove_terms = c(
  "tco", "https","can","http","will","amp","take","get","sorri","now","busi","one","use", "'s"
)

trim_non_environmental_term_n = non_environmental_term_n[c(1:64),]
trim_non_environmental_term_n$word = as.character(trim_non_environmental_term_n$word)
trim_non_environmental_term_n = trim_non_environmental_term_n[!trim_non_environmental_term_n$word %in% non_environmental_remove_terms,]
trim_non_environmental_term_n$col = "grey"
# trim_non_environmental_term_n$col= ifelse(trim_non_environmental_term_n$word == "forest", "darkgreen", trim_non_environmental_term_n$col)
# trim_non_environmental_term_n$col= ifelse(trim_non_environmental_term_n$word == "planet", "darkgreen", trim_non_environmental_term_n$col)

plt_non_environmental_wordcloud = ggplot(trim_non_environmental_term_n, aes(label = word, size = freq, colour = col)) +
  scale_colour_manual(values = c("grey")) +
  geom_text_wordcloud_area(shape = "square") +
  scale_size_area(max_size = 23) +
  theme_minimal()


#Characterise tweets data ----
environmental_corpus <- Corpus(VectorSource(environmental$text))
environmental_corpus <- tm_map(environmental_corpus, toSpace, "/")
environmental_corpus <- tm_map(environmental_corpus, toSpace, "@")
environmental_corpus <- tm_map(environmental_corpus, toSpace, "\\|")
environmental_corpus <- tm_map(environmental_corpus, content_transformer(tolower))
environmental_corpus <- tm_map(environmental_corpus, removeNumbers)
environmental_corpus <- tm_map(environmental_corpus, removeWords, stopwords("english"))
environmental_corpus <- tm_map(environmental_corpus, removePunctuation)
environmental_corpus <- tm_map(environmental_corpus, stripWhitespace)
#environmental_corpus <- tm_map(environmental_corpus)

environmental_dtm <- TermDocumentMatrix(environmental_corpus)
environmental_dtm2 <- removeSparseTerms(environmental_dtm, sparse = 0.999)
environmental_matrix <- as.matrix(environmental_dtm2)
environmental_term_n <- sort(rowSums(environmental_matrix),decreasing=TRUE)
environmental_term_n <- data.frame(word = names(environmental_term_n),freq=environmental_term_n)
rownames(environmental_term_n) = NULL

# Consolidate environment and environmental
environmental_term_n <- environmental_term_n %>% 
  filter(word != "environmental") %>%
  mutate(freq = ifelse(as.numeric(freq) == 2728, (2728 + 1041), as.numeric(freq)))

# Consolidate flood and floods
environmental_term_n <- environmental_term_n %>% 
  filter(word != "floods") %>%
  mutate(freq = ifelse(as.numeric(freq) == 1116, (1116 + 402), as.numeric(freq)))

environmental_remove_terms = c(
  "tco", "https","can","http","will","amp","take","get","sorri","now","busi","one","use", "'s"
)
#remove_terms
trim_environmental_term_n = environmental_term_n[c(1:64),]
trim_environmental_term_n$word = as.character(trim_environmental_term_n$word)
trim_environmental_term_n = trim_environmental_term_n[!trim_environmental_term_n$word %in% environmental_remove_terms,]
trim_environmental_term_n$col = "grey"
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "climate", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "environment", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "environment", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "environmental", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "sustainable", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "carbon", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "nature", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "green", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "emissions", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "wildlife", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "energy", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "emissions", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "change", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "flood", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "air", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "plastic", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "pollution", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "flooding", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "climatechange", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "climateemergency", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "emergency", "darkgreen", trim_environmental_term_n$col)
trim_environmental_term_n$col= ifelse(trim_environmental_term_n$word == "crisis", "darkgreen", trim_environmental_term_n$col)

plt_environmental_wordcloud = ggplot(trim_environmental_term_n, aes(label = word, size = freq, colour = col)) +
  scale_colour_manual(values = c("darkgreen", "grey")) +
  geom_text_wordcloud_area(shape = "square") +
  scale_size_area(max_size = 23) +
  theme_minimal()
 

ggarrange(
plt_environmental_wordcloud,
#plt_non_environmental_wordcloud,
plt_tweets_wordcloud)
