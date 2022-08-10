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
library(GGally)
options(scipen = 999)
#### 2 Read in Data ####


tweet_data <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\tweets_data.csv")
head(tweet_data)
vote_data <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\votes_data.csv")

proportions_data <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\Model_Data\\proportions_data.csv")

proportions_data$tweet_score <- proportions_data$tweet_score * 100

constituencies <- read.csv("C:\\Users\\dp005352\\Documents\\PhD\\Files_to_keep_not_on_DropBox\\PR_Machine\\Data\\constituencies_data.csv")

#### Testing correlations ####

correlation_data <- constituencies %>%
  select(constituency, pop_den, wage) %>%
  rename(Constituency = constituency)
correlation_data_2 <- proportions_data %>%
  select(Constituency, followers_count, vote_score)

correlation_data <- left_join(correlation_data, correlation_data_2, by = "Constituency")
correlation_data <- correlation_data %>%
  filter(followers_count != "NA")

correlation_data$pop_den <- scale(log10(correlation_data$pop_den))
correlation_data$wage <- scale(correlation_data$wage)
correlation_data$followers_count <- scale(log10(correlation_data$followers_count))
correlation_data$vote_score <- scale(correlation_data$vote_score)

correlation_data$pop_den <- as.numeric(correlation_data$pop_den)
correlation_data$wage <- as.numeric(correlation_data$wage)
correlation_data$followers_count <- as.numeric(correlation_data$followers_count)
correlation_data$vote_score <- as.numeric(correlation_data$vote_score)


str(correlation_data)
ggpairs(correlation_data, columns = 2:5)

#### 5 Modelling ####

#### ~ 5.2 Raw ####

##### ~ ~ 5.2.1 Tweet Model ####

tweet_model <- glmer(enviro ~ Date_Numeric + (1|Party/Account),
  data = tweet_data, family = binomial)
 summary(tweet_model)
 
 exp(0.213)
 test <- ggpredict(tweet_model, terms = c("Date_Numeric [all]", "Party"))
# 
 test$x <- test$x+1970


# #### ~ ~ ~ 5.2.1.1 tweet model outputs ####
# 
  #plot(tweet_model)
  #qqnorm(residuals(tweet_model))
  #hist(residuals(tweet_model))
# 
 #MuMIn::r.squaredGLMM(tweet_model)

  # > summary(tweet_model)
  # Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
  # Family: binomial  ( logit )
  # Formula: enviro ~ Date_Numeric + (1 | Party/Account)
  # Data: tweet_data
  # 
  # AIC      BIC   logLik deviance df.resid 
  # 110751.9 110798.5 -55371.9 110743.9   844286 
  # 
  # Scaled residuals: 
  #   Min      1Q  Median      3Q     Max 
  # -0.5248 -0.1268 -0.0864 -0.0624 29.1542 
  # 
  # Random effects:
  #   Groups        Name        Variance Std.Dev.
  # Account:Party (Intercept) 0.9901   0.9951  
  # Party         (Intercept) 0.1614   0.4017  
  # Number of obs: 844290, groups:  Account:Party, 612; Party, 5
  # 
  # Fixed effects:
  #   Estimate Std. Error z value            Pr(>|z|)    
  # (Intercept)  -15.330429   0.383953  -39.93 <0.0000000000000002 ***
  #   Date_Numeric   0.213000   0.007269   29.30 <0.0000000000000002 ***
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Correlation of Fixed Effects:
  #   (Intr)
  # Date_Numerc -0.865

 # >  MuMIn::r.squaredGLMM(tweet_model)
 # R2m        R2c
 # theoretical 0.038252411 0.28760323
 # delta       0.001997794 0.01502054
# Warning messages:
#   1: 'r.squaredGLMM' now calculates a revised statistic. See the help page. 
# 2: The null model is correct only if all variables used by the original model remain unchanged. 

#### ~ ~ 5.2.2 Vote Model ####

vote_model <- glmer(vote_score ~ (Date_Numeric) + 
                      ((Date_Numeric) | Party/Name),
                    data = vote_data, family = binomial)

summary(vote_model)
exp(-0.1852)
#confint(vote_model)
#plot(vote_model)

MuMIn::r.squaredGLMM(vote_model)

# > summary(vote_model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: vote_score ~ (Date_Numeric) + ((Date_Numeric) | Party/Name)
# Data: vote_data
# 
# AIC      BIC   logLik deviance df.resid 
# 2430.6   2480.5  -1207.3   2414.6     3782 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5444 -0.1885 -0.1125  0.3753 25.1431 
# 
# Random effects:
#   Groups     Name         Variance             Std.Dev.      Corr 
# Name:Party (Intercept)    0.0000000016877572  0.0000410823      
# Date_Numeric   0.0000000000009733  0.0000009866 -1.00
# Party      (Intercept)  392.6792648462469515 19.8161364763      
# Date_Numeric   0.2519349907923611  0.5019312610 -1.00
# Number of obs: 3790, groups:  Name:Party, 374; Party, 5
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)    8.9490     9.0206   0.992    0.321
# Date_Numeric  -0.1852     0.2268  -0.816    0.414
# 
# Correlation of Fixed Effects:
#   (Intr)
# Date_Numerc -0.998
# convergence code: 0
# boundary (singular) fit: see ?isSingular
# 
# > MuMIn::r.squaredGLMM(vote_model)
# boundary (singular) fit: see ?isSingular
# R2m       R2c
# theoretical 0.01555835 0.7381463
# delta       0.01471655 0.6982082


vote_data$prop <-  predict(vote_model, type = "response")

# Calculate the average predicted vote scores by party for each vote
 av_votes <- vote_data %>%
   group_by(Date_Numeric, Party) %>%
   summarise(mean = mean(prop))



cmb_df = NULL
for (a in 1:length(unique(vote_data$Name))){
  print(nrow(cmb_df))
  mp = unique(vote_data$Name)[a]
  tmp_df = subset(vote_data, Name == mp,
                  select = c(
                    "Name",
                    "Party",
                    "Date_Numeric",
                    "vote_score"
                  ))
  if(nrow(tmp_df) > 1){
    tmp_mod = glm(vote_score ~ scale(Date_Numeric),
                  data = tmp_df, family = binomial)
    tmp_df$prop = predict(tmp_mod, type = "response")
    cmb_df = rbind(cmb_df, tmp_df)

  } else {
  }
}

cmb_df = left_join(cmb_df, av_votes)
cmb_df$difference = cmb_df$prop - cmb_df$mean

deviance_over_time <- ggplot(cmb_df) +
  geom_jitter(aes(y = difference, x = Date_Numeric + 1970, 
                  colour = Party, fill = Party),  width = 1) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#6AB023",  "#DC241f", "#FAA61A", "#008142",  "#FDF38E", "#2AA82C"))  + 
  labs(x = "Date of Vote",
       y = "Party Deviance") +
  theme_classic() +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

deviance_over_time

#### 6 Vote Model Residuals #### 


#### ~ 6.1 Data Manipulation ####

residual_plot_data <- cmb_df %>%
  group_by(Name) %>%
  summarise(mean = mean(difference))

# Join proportions data
residual_plot_data <- left_join(residual_plot_data, proportions_data, by = "Name")

# Remove NA and parties with fewer than 5 representatives
residual_plot_data <- residual_plot_data %>%
  filter(Party != "NA") %>%
  filter(Party != "Birkenhead Social Justice" &
           Party != "Green Party" &
           Party != "Plaid Cymru" &
           Party != "Social Democratic and Labour Party" &
           Party != "Speaker")

#table(residual_plot_data$Party)

constituencies <- constituencies %>%
  rename(Constituency = constituency)

residual_plot_data <- left_join(residual_plot_data, constituencies, by = "Constituency")

residual_plot_data <- na.omit(residual_plot_data)


#### ~ 6.2 Plotting ####


# Full Period
tweet_vs_vote_residual_plot <-
  ggplot(residual_plot_data, aes(x=tweet_score, y=(mean*100))) + 
  geom_point(aes(color=Party), size = 2, alpha = 0.5) +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  geom_smooth(colour = "black", method = "lm") +
  labs(y = "Party Deviance %",
       x = "Environmental Tweets %") +
  theme_classic() +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

tweet_vs_vote_residual_plot

#### ~ 6.3 Modeling ####


# Create new column of longitude to add small variation 
# if points are within 0.0001 (i.e. identical) of each
# other
residual_plot_data$long2 <- residual_plot_data$long +
  runif(nrow(residual_plot_data), 0.0001, 0.01)

# nlme model accounts for lat and long
# mean vote difference as a function of:
# Fixed effects - tweets + pop + wage + followers
# Random Effect - Party 
# Control for spatial
#   corExp represents an exponential spatial correlation structure
#   form = 1 sided formula specifying spatial covariates
#   nugget - correlation between two observations a distance r apart 
#          = (l-n)*exp(-r/d) if nugget = T
#          creates distance matrix between site

# final_model <- lme(mean ~ tweet_score +
#                      log10(pop_den) +
#                      wage +
#                      log10(followers_count),
#                    random = ~ 1|Party,
#                    correlation=corExp(form = ~ lat + long2, nugget=T),
#                    data=residual_plot_data,
#                    method = "ML",
#                    control = lmeControl(opt = 'optim'))
# summary(final_model)
# # 
#  plot(final_model)
 # qqnorm(residuals(final_model))
#  hist(residuals(final_model))
# 
# # Check R squareds - almost the same
# MuMIn::r.squaredGLMM(final_model)


# Need to inverse hyperbolic sine transform the data due to heavy tails 
# distribution of residual
# IHS works better at larger scales so need to multiply the mean difference scores
# Trail and error 100 not enough, 1000 too much, 500 about right

# Create IHS function
ihs <- function(x) {
  y <- log(x + sqrt(x^ 2 + 1))
  return(y)
}


residual_plot_data <- residual_plot_data %>%
  mutate(mean_difference_t = ihs(mean * 100))


ihs_model <- lme(mean_difference_t ~ 
                   scale(tweet_score) + 
                   scale(log10(pop_den)) + 
                   scale(wage) + 
                   scale(log10(followers_count)), 
                 random = ~ 1|Party,
                   correlation=corExp(form = ~ lat + long2, nugget=T),
                   data=residual_plot_data,
                   method = "ML",
                   control = lmeControl(opt = 'optim'))

summary(ihs_model)
plot(ihs_model)
qqnorm(residuals(ihs_model))
hist(residuals(ihs_model))
MuMIn::r.squaredGLMM(ihs_model)
confint(ihs_model)

#scale results
# tweet
sinh(0.4601736)
sinh(0.1090113)

# wage
sinh(0.3159409)
sinh(0.1190532)



residual_plot_data$predicted <-  predict(ihs_model, type = "response")


# Generate predicted values for each variable
tweets <- ggpredict(ihs_model, terms = "tweet_score [n=50]")
tweets$predicted <- sinh(tweets$predicted)
tweets$conf.low <- sinh(tweets$conf.low)
tweets$conf.high <- sinh(tweets$conf.high)

# To find increase in votes per 1% increase in tweets
#find predicted values at 0% and 1%
#sinh values and calulate difference
#sinh(0.19882051)
#sinh(-0.08012539)
#0.20+0.08

pop_den <- ggpredict(ihs_model, terms = "pop_den [n=10]")
pop_den <- pop_den %>% filter(x != 0)
pop_den$predicted <- sinh(pop_den$predicted)
pop_den$conf.low <- sinh(pop_den$conf.low)
pop_den$conf.high <- sinh(pop_den$conf.high)

wage <- ggpredict(ihs_model, terms = "wage")
wage$predicted <- sinh(wage$predicted)
wage$conf.low <- sinh(wage$conf.low)
wage$conf.high <- sinh(wage$conf.high)

# To find increase in votes per 1% increase in tweets
#find predicted values at 500 and 600
#sinh values and calulate difference
#wage <- ggpredict(ihs_model, terms = "wage [500, 600]")


followers <- ggpredict(ihs_model, terms = "followers_count [n=10]")
followers <- followers %>% filter(x != 0)
followers$predicted <- sinh(followers$predicted)
followers$conf.low <- sinh(followers$conf.low)
followers$conf.high <- sinh(followers$conf.high)

tweets_plot <- ggplot() +
  geom_line(aes(tweets$x, (tweets$predicted))) +
  geom_ribbon(aes(tweets$x, (tweets$predicted), 
                  ymin = (tweets$conf.low), 
                  ymax = (tweets$conf.high)), alpha = 0.1) +
  geom_point(aes(x = residual_plot_data$tweet_score, 
                 y = (residual_plot_data$predicted),
                 color=residual_plot_data$Party), alpha =.3) +
  theme_classic() +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab("Environmental Tweets (%)")+
  ylab("Party Deviance (%)") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank())


pop_den_plot <- ggplot() +
  geom_line(aes(pop_den$x, (pop_den$predicted)), linetype = "dashed") +
  geom_ribbon(aes(pop_den$x, (pop_den$predicted), 
                  ymin = (pop_den$conf.low), 
                  ymax = (pop_den$conf.high)), alpha = 0.1) +
  geom_point(aes(x = residual_plot_data$pop_den, 
                  y = (residual_plot_data$predicted),
                 color=residual_plot_data$Party), alpha =.3) +
  theme_classic() +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab(expression(paste("Population Density"~(km^2)))) +
  ylab("Party Deviance (%)") +
  scale_x_log10() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank())


wage_plot <- ggplot() +
  geom_line(aes(wage$x, (wage$predicted))) +
  geom_ribbon(aes(wage$x, (wage$predicted), 
                  ymin = (wage$conf.low), 
                  ymax = (wage$conf.high)), alpha = 0.1) +
  geom_point(aes(x = residual_plot_data$wage, 
                 y = (residual_plot_data$predicted),
                 color=residual_plot_data$Party), alpha =.3) +
  theme_classic() +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab("Average Weekly Wage (£)")+
  ylab("Party Deviance (%)") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank())


followers_plot <- ggplot() +
  geom_line(aes(followers$x, (followers$predicted)), linetype = "dashed") +
  geom_ribbon(aes(followers$x, (followers$predicted), 
                  ymin = (followers$conf.low), 
                  ymax = (followers$conf.high)), alpha = 0.1) +
  geom_point(aes(x = (residual_plot_data$followers_count), 
                 y = (residual_plot_data$predicted),
                 color=residual_plot_data$Party), alpha =.3) +
  theme_classic() +
  scale_color_manual(values=c("#0087DC", "#D46A4C", "#E4003B", "#FAA61A", "#FDF38E")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  xlab("Followers (N)")+
  ylab("Party Deviance (%)") +
  scale_x_log10() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank())

ggarrange(tweets_plot, wage_plot, pop_den_plot,followers_plot, common.legend = T,
          legend = "bottom")

