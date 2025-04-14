# Script: Longevity and Gender: Do women live longer than men? #################

# Preparation ----
rm(list = ls(all=TRUE))

# packages needed
library(tidyverse)
library(nlme)
library(MuMIn)

# set wd
setwd("C:/Users/enara681/Desktop")

# import trial data
trial <- read.csv2("trial_survey.csv")

# create subset man
sub_man <- trial %>% 
  filter(Gender == "M")

# create subset woman
sub_woman <- trial %>% 
  filter(Gender == "F")

# means
mean1 <- mean(sub_man$Age) 
mean2 <- mean(sub_woman$Age)

# sd
sd1 <- sd(sub_man$Age) 
sd2 <- sd(sub_woman$Age) 

# calculate pooled sd
sd_pool <- sqrt( ((30 - 1) * sd1^2 + (30 - 1) * sd2^2) / (30 + 30 - 2) )

# calculate effect size d
d <- abs(mean1 - mean2) / sd_pool

# power analysis
power.t.test(power = 0.85,        # power needed according to Prof. Wolinska
             delta = d,           
             sd = 1,              
             sig.level = 0.05,    
             type = "two.sample", 
             alternative = "one.sided") 

# ~ 43 samples in each group needed

# analysis ----

# import data
life <- read.csv2("life.csv") # 75 samples per group

# data exploration

# boxplots

# sealed surfaces
ggplot(life, mapping = aes(x = Gender, y = Age,
                             color = factor(Gender)))+
  geom_boxplot(notch = T)+
  geom_point(position=position_jitter(), alpha = 0.7, size = 2)+
  scale_x_discrete(breaks = c(0, 1))+
  scale_color_manual(values = c("F" = "#457983", "M" = "#cf3759"))+
  theme_bw()+
  theme(legend.position = "none") # difficult to interpret

# distributions of men 
sub_man <- life %>% 
  filter(Gender == "M")

ggplot(sub_man, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#7FFFD4", alpha = 0.7) +
  labs(title = "Distribution of lifespan for men",
    x = "Age",
       y = "Frequency") +
  theme_bw() # very skewed

# distribution of women
sub_woman <- life %>% 
  filter(Gender == "F")

ggplot(sub_woman, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#7FFFD4", alpha = 0.7) +
  labs(title = "Distribution of lifespan for women",
    x = "Age",
       y = "Frequency") +
  theme_bw() # very skewed

# analyse the effect of sex on age with cementry as a random effect

# built first linear mixed effect model 
life_mod <- lme(fixed = Age ~ Gender, random = ~ 1 | Cemetary..No..,
                data = life)

# check summary
summary(life_mod)

# variance coefficients of random effect
VarCorr(life_mod)

# calculate R² (marginal and conditional)
r2 <- MuMIn::r.squaredGLMM(life_mod)
print(r2) # very low

# p value adjustment since the default gives out p for two-sided t test
0.0065*0.5 # 0.00325

# double check with a normal one-sided t test
t.test(Age ~ Gender, data = life, alternative = "greater") # 0.003


# model diagnostics ----

# residuals vs fitted values
plot(life_mod)

# Q-Q-plot
qqnorm(resid(life_mod))
qqline(resid(life_mod, col = "red"))# still OK
       
# histogram of residuals
hist(resid(life_mod)) # looks OK

hist(life$Age)

# plot results ----

# create subset man
men <- life %>% 
  filter(Gender == "M")

# create subset woman
women <- life %>% 
  filter(Gender == "F")

mean(men$Age) 
mean(women$Age) 

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
