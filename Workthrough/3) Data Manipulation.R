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


#### 2 Read in Data ####


tweets <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Tweet_data\\tweet_subset.csv")
# This is the correct dataset including sitting and former MPs - retweets without quote have been removed

votes <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Vote_data\\guardian_votes.csv")
#unique(votes$Official.Name)

handles <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\mp.csv")
handles_2 <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\more_MPs.csv")


#### 3 Data Manipulation ####
#### ~ 3.1 General ####
#### ~ ~ 3.1.1 Handles ####

# This section takes the list of twitter handles from sitting MPs and the 
# additional MPs selected and joins them together
handles <- handles %>% 
  select(-(DOB))

handles_2 <- handles_2 %>%
  select(-c(DOB, MP_id))
handles_2$MP_id <- 595:679

handles <- rbind(handles, handles_2)


# Remove titles and extras from names
handles$Name <- gsub("Dame ", "", handles$Name)
handles$Name <- gsub("Sir ", "", handles$Name)
handles$Name <- gsub("Dr ", "", handles$Name)
handles$Name <- gsub(" QC", "", handles$Name)
handles$Name <- gsub(" MP", "", handles$Name)
handles$Name <- gsub("David T. C. Davies", "David Davies", handles$Name)


#### ~ ~ 3.1.2 Votes ####

# Remove MPs that swtiched parties during the period
votes <- votes %>%
  filter(Name != "Angela Smith") %>%
  filter(Name != "Ann Coffey") %>%
  filter(Name != "Anna Soubry") %>%
  filter(Name != "Antoinette Sandbach") %>%
  filter(Name != "Chris Leslie") %>%
  filter(Name != "Chuka Umunna") %>%
  filter(Name != "Douglas Carswell") %>%
  filter(Name != "Gavin Shuker") %>%
  filter(Name != "Heidi Allen") %>%
  filter(Name != "Joan Ryan") %>%
  filter(Name != "John Woodcock") %>%
  filter(Name != "Luciana Berger") %>%
  filter(Name != "Mark Reckless") %>%
  filter(Name != "Mike Gapes") %>%
  filter(Name != "Phillip Lee") %>%
  filter(Name != "Sam Gyimah") %>%
  filter(Name != "Sarah Wollaston")

#Create dataset of MPs that had the whip removed
whip <- votes %>% 
  filter(Name == "Michelle Thomson" |
           Name =="John Woodcock" |
           Name =="Mike Hancock" |
           Name =="Natalie McGarry" |
           Name =="Stephen Lloyd") %>%
  arrange(Name)

#Remove MPs that had the whip removed from main data
votes <- votes %>% 
  filter(Name != "Michelle Thomson") %>%
  filter(Name !="John Woodcock") %>%
  filter(Name !="Mike Hancock") %>%
  filter(Name !="Natalie McGarry") %>%
  filter(Name !="Stephen Lloyd")

# Create new df of them in their original party
Indi_replace <- as.data.frame(c( 
  "SNP", "SNP",
  "LDem", "LDem", "LDem", "LDem", 
  "SNP", "SNP",
  "LDem", "LDem", "LDem", "LDem", "LDem"))

whip <- cbind(whip, Indi_replace)

names(whip) [11] <- "Party2"

whip <- whip %>%
  select(-Party) %>%
  rename(Party = Party2)

# Return re-whipped MPs to main data
votes <- rbind(votes, whip)


#### ~ ~ 3.1.3 Tweets ####
# Rename the MP column to Name to match other datasets
# Rename the screen-name_collumn to account to match other datasets
tweets <- tweets %>%
  rename("Name" = "name") %>%
  rename("Account" = "screen_name")


#### ~ 3.2 Proportions ####


#### ~ ~ 3.2.1 Votes ####


# Count how many votes per MP
names <- votes %>%
  group_by(Name) %>%
  summarise(total = n())

names <- as.data.frame(names)

# Remove MPs that voted in less than five votes
names <- names %>%
  filter(total >= 5) # 

name_list <- names$Name

votes <- votes %>%
  filter(Name %in% name_list)

# Create new df with fewer columns
vote_summary <- votes %>%
  select(Official.Name, Date, Name, Party, Vote.1, Constituency) %>%
  rename(Vote = Vote.1,
         Motion = Official.Name) 

# Format dates
vote_summary$Date <- gsub("/", "-", vote_summary$Date)
vote_summary$Date <- as.character(as.Date(vote_summary$Date, "%d-%m-%Y"), "%Y%m%d")
vote_summary$Date <- as.numeric(vote_summary$Date)

vote_summary$Vote <- as.numeric(vote_summary$Vote)

# Create dataframe of proportions
vote_proportions <- vote_summary

# Convert vote from character to numeric
vote_proportions$Vote <- as.numeric(vote_proportions$Vote)  

# Count number of votes per mp and mean environmental score
vote_proportions <- vote_proportions %>%
  group_by(Name, Constituency) %>%
  summarise(mean_vote = mean(Vote),
            votes = n()) 


#### ~ ~ 3.2.2 Tweets ####


# Create new df for proportions
tweet_proportions <- tweets

# Convert enviro scores from character to numeric
tweet_proportions$enviro <- as.numeric(tweet_proportions$enviro)  

# Count number of tweets per MP and mean environmental tweet
tweet_proportions <- tweet_proportions %>%
  group_by(Account) %>%
  summarise(mean_enviro = mean(enviro),
            tweets = n()) 

# Rename columns in handle df to match tweet proportions df
handles_proportions <- handles %>%
  select(Name, Account, Party)

# Combine tweet proportions data with MP twitter handles df
tweet_proportions <- left_join(tweet_proportions, handles_proportions, by = "Account")


#### ~ ~ 3.2.3 Join Dataframes ####


followers1 <- tweets %>%
  select(Account, followers_count)
followers1 <- distinct(followers1)

followers2 <- handles %>%
  select(Name, Account)

followers <- left_join(followers1, followers2, by = "Account")
followers <- followers %>%
  select(-Account)

# Left join dataframes using Name as common column
proportions_data <- left_join(tweet_proportions, vote_proportions, by = "Name")

proportions_data <- na.omit(proportions_data)

proportions_data <- left_join(proportions_data, followers, by = "Name")

# Reorder columns for ease of reading, remove unecessary columns
# Calculate a score for tweets
proportions_data <- proportions_data %>%
  select(Name, Party, everything()) %>%
  select(-Account) %>%
  rename(tweet_score = mean_enviro, vote_score = mean_vote) %>%
  filter(Party != "NA")

# Ensure consistenc of party names throughout 
proportions_data$Party <- gsub("Liberal Democrats", "Liberal Democrat", proportions_data$Party)
proportions_data$Party <- gsub("SNP", "Scottish National Party", proportions_data$Party)
proportions_data$Party <- gsub("SDLP", "Social Democratic and Labour Party", proportions_data$Party)
proportions_data$Party <- gsub("DUP", "Democratic Unionist Party", proportions_data$Party)
proportions_data$Party <- gsub("UKIP", "UK Independence Party", proportions_data$Party)
proportions_data$Party <- gsub("UUP", "Ulster Unionist Party", proportions_data$Party)


#### ~ 3.3 Raw Data Manipulation ####


#### ~ ~ 3.3.1 Votes ####


# merge votes with MPs
vote_raw <- left_join(handles, votes, by = "Name")

# Remove MPs that have twitter but aren't in data

vote_raw <- vote_raw %>%
  filter(Official.Name != "NA") %>%
  rename(Motion = Official.Name)

# Remove duplicate columns and rename
vote_raw <- vote_raw %>%
  select(-c(Party.y, Constituency.y)) %>%
  rename(Party = Party.x) %>%
  rename(Constituency = Constituency.x)

# Count unique values
#unique(votes$Name) #432 pMPs

# How many votes by party
party <- vote_raw %>%
  group_by(Party) %>%
  summarise(total = n())

# Rename column
vote_raw <- vote_raw %>%
  rename(vote_score = Vote.1)

vote_raw$vote_score <- as.numeric(vote_raw$vote_score)

# Ensure consistenc of party names throughout 
vote_raw$Party <- gsub("Liberal Democrats", "Liberal Democrat", vote_raw$Party)
vote_raw$Party <- gsub("SNP", "Scottish National Party", vote_raw$Party)
vote_raw$Party <- gsub("SDLP", "Social Democratic and Labour Party", vote_raw$Party)
vote_raw$Party <- gsub("DUP", "Democratic Unionist Party", vote_raw$Party)
vote_raw$Party <- gsub("UKIP", "UK Independence Party", vote_raw$Party)
vote_raw$Party <- gsub("UUP", "Ulster Unionist Party", vote_raw$Party)

#Create specific parliaments
labour_summary <- vote_raw %>%
  filter(as.Date(Date) <= as.Date("2010-05-06"))

coalition_summary <- vote_raw %>%
  filter(as.Date(Date) > as.Date("2010-05-06")) %>%
  filter(as.Date(Date) < as.Date("2015-05-07"))

conservative_summary <- vote_raw %>%
  filter(as.Date(Date) > as.Date("2015-05-07"))


#### ~ ~ 3.3.2 Tweets ####


# merge votes with MPs
tweets_raw <- left_join(handles, tweets, by = "Account")

number_of_tweets <- tweets_raw %>%
  group_by(Account) %>%
  summarise(total = n())

# Ensure consistenc of party names throughout 
tweets_raw$Party <- gsub("Liberal Democrats", "Liberal Democrat", tweets_raw$Party)
tweets_raw$Party <- gsub("SNP", "Scottish National Party", tweets_raw$Party)
tweets_raw$Party <- gsub("SDLP", "Social Democratic and Labour Party", tweets_raw$Party)
tweets_raw$Party <- gsub("DUP", "Democratic Unionist Party", tweets_raw$Party)
tweets_raw$Party <- gsub("UKIP", "UK Independence Party", tweets_raw$Party)
tweets_raw$Party <- gsub("UUP", "Ulster Unionist Party", tweets_raw$Party)


#### ~ ~ 3.3.3 Sort Dates ####


# Votes
# code formats dates into numeric yyyymmdd hh:mm format
vote_raw$Date <- as.Date(vote_raw$Date, format = "%d/%m/%Y")
vote_raw$Date_time <- ymd_hm(paste(vote_raw$Date, vote_raw$Time))
vote_raw$Date_Numeric <- as.numeric(vote_raw$Date_time)
vote_raw$Date_Numeric <- vote_raw$Date_Numeric/60/60/24/365.25 #Converts seconds into years
tweets_raw$Date_Numeric <- as.numeric(as.POSIXct(tweets_raw$created_at))
tweets_raw$Date_Numeric <- tweets_raw$Date_Numeric/60/60/24/365.25 #Converts seconds into years

head(vote_raw)

# Remove Parties with less than 5 MPS

vote_raw <- vote_raw %>%
  filter(Party == "Conservative" |
           Party == "Democratic Unionist Party"|
           Party == "Labour"|
           Party == "Liberal Democrat"|
           Party == "Scottish National Party")


tweets_raw <- tweets_raw %>%
  filter(Party == "Conservative" |
           Party == "Democratic Unionist Party"|
           Party == "Labour"|
           Party == "Liberal Democrat"|
           Party == "Scottish National Party")

proportions_data <- proportions_data %>%
  filter(Party == "Conservative" |
           Party == "Democratic Unionist Party"|
           Party == "Labour"|
           Party == "Liberal Democrat"|
           Party == "Scottish National Party")


#### ~ Write Datasets ####

write.csv(tweets_raw, "C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\tweets_data.csv", row.names = FALSE)

write.csv(vote_raw, "C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\votes_data.csv", row.names = FALSE)

write.csv(proportions_data, "C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\proportions_data.csv", row.names = FALSE)

