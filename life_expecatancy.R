# Script: Longevity and Gender: Do women live longer than men? #################

# Preparation ----
rm(list = ls(all=TRUE))

# packages needed
library(tidyverse)
library(nlme)
library(MuMIn)

# set wd
setwd("C:/Users/enara681/Desktop")

# import data
life <- read.csv2("life.csv")

# delete one sample from Vanessa to obtain balanced design
life <- life[-149, ]

# estimation of sample size ----

# create subset man
man <- life %>% 
  filter(Gender == "M")

# create subset woman
woman <- life %>% 
  filter(Gender == "F")

# create random subset of samples for power analysis
set.seed(14)
sub_man <- man[sample(nrow(man), 30), ]
sub_woman <- woman[sample(nrow(woman), 30), ]

# means
mean1 <- mean(sub_man$Age) # 77.46667
mean2 <- mean(sub_woman$Age) # 78.5

# sd
sd1 <- sd(sub_man$Age) # 11.21
sd2 <- sd(sub_woman$Age) # 8.48

# calculate pooled sd
s_pooled <- sqrt( ((30 - 1) * sd1^2 + (30 - 1) * sd2^2) / (30 + 30 - 2) )

# effect size d
d <- abs(mean1 - mean2) / s_pooled

# power analysis
power.t.test(power = 0.85,        # power needed according to Prof. Wolinska
             delta = d,           
             sd = 1,              
             sig.level = 0.05,    
             type = "two.sample", 
             alternative = "one.sided") 

# ~ 54 samples needed
# 90 samples left in our data set (45 women and 45 men)

# remove samples used for power analysis from the original data set:

# identify samples
ids_to_remove <- unique(c(sub_man$Sample_ID, sub_woman$Sample_ID))

# exclude samples from dataset
life <- life %>%
  filter(!Sample_ID %in% ids_to_remove) # 90 observations left


# analysis ----

# analyse the effect of sex on age with cementry as a random effect

# built first linear mixed effect model 
life_mod <- lme(fixed = Age ~ Gender, random = ~ 1 | Cemetary..No..,
                data = life)

# check summary
summary(life_mod)

# variance coefficients of random effect
VarCorr(life_mod)

# calculate R² (marginal and conditional)
r2 <- r.squaredGLMM(life_mod)
print(r2) # very low

# p value adjustment since the default gives out p for two-sided t test
0.0247*0.5 # 0.0124

# double check with a normal one-sided t test
t.test(Age ~ Gender, data = life, alternative = "greater") # 0.012


# model diagnostics ----

# residuals vs fitted values
plot(life_mod)

# Q-Q-plot
qqnorm(resid(life_mod))
qqline(resid(life_mod, col = "red"))# still OK
       
# histogram of residuals
hist(resid(life_mod)) # looks Ok

hist(life$Age)

# plot results ----

# create subset man
men <- life %>% 
  filter(Gender == "M")

# create subset woman
women <- life %>% 
  filter(Gender == "F")

mean(men$Age) # 68.33
mean(women$Age) # 75.96

# standard error (SE = SD / sqrt(n))
sd_men <- sd(men$Age)
sd_women <- sd(women$Age)

# calculate standard error for men
se_men <- sd(men$Age) / sqrt(length(men$Age))
se_men

# standard error for women
se_women <- sd(women$Age) / sqrt(length(women$Age))
se_women

# create tibble with relevant stats for plotting
stats <- life %>%
  filter(Gender %in% c("M", "F")) %>%
  group_by(Gender) %>%
  summarise(
    mean_Age = mean(Age),
    se_Age = sd(Age) / sqrt(n()),
    .groups = "drop")

# plot 
ggplot(stats, aes(x = Gender, y = mean_Age)) +
  geom_point(size = 4, color = "black") +                       
  geom_errorbar(aes(ymin = mean_Age - 1.96 * se_Age, 
                    ymax = mean_Age + 1.96 * se_Age), 
                width = 0.03, color = "black") +                   
  labs(x = "Sex", 
       y = "Age") +
  scale_y_continuous(limits = c(60, NA)) +                       
  theme_classic() +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14))

# save
ggsave("results_mean.jpg", 
       width = 4, height = 4)