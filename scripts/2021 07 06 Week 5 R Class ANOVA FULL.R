# So today we are going to try a few different approaches
# The goal is to
#   - read in data
#   - change id and seed_type to factors
#   - run an ANOVA on the data by plant
#   - test assumptions
#   - if significant run post f tests

# # install new packages if you have not done so already  ----
# install.packages("car")
# install.packages("emmeans")
# install.packages("multcompView")
# install.packages("Hmisc")

# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)

# then load libraries
library(car)
library(emmeans)
library(multcompView)

# read in files ---
# this is how you read a csv file - commas separated values
# this file - reduced_mutant_seed_parameters.csv - should be in your data directory
anova.df  <- read_csv("data/reduced_mutant_seed_parameters.csv") %>% clean_names()

# now lets make each id a factor and seed_type
anova.df <- anova.df %>% 
  mutate(id = as.factor(id),
         seed_type = as.factor(seed_type))

# Factor reorder -----
# Note we could reorder the factors from alphabetical and numberic to what we want

# Get Factor levels-----
levels(anova.df$seed_type)

#Reorder factors ------
anova.df <- anova.df %>% 
  mutate(seed_type = fct_relevel(seed_type, 
                                  "wildtype", "mutant 11B", "mutant 7B", "mutatn 8A" ))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Mean and Standard Error
anova.df %>% 
  ggplot(aes(seed_type, mean_length, color=seed_type)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) 

# Mean and Standard Deviation +/-1 SD
anova.df %>% 
  ggplot(aes(seed_type, mean_length, color=seed_type)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.data = mean_sdl, na.rm = TRUE, 
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ANOVA - TESTING FOR DIFFERNECES IN SEED LENGTH ------
# options settings for proper test---
# this is necessary to set up the system to do the contrasts properly
options(contrasts = c("contr.sum", "contr.poly"))


# ANOVA MODEL
# this makes the model that we will test assumptions and also 
# test to see if there is a differnece in means
anova.model <- lm(mean_length ~ seed_type, data = anova.df)


# Check assumptions------
# Normality Assumption graphical QQ PLOT ----
# the GGplot way
ggplot(anova.df, aes(sample=mean_length)) + 
  geom_qq() +
  geom_qq_line()

# the base way
plot(anova.model, 2)

# check for normally distributed data
qqnorm(anova.model$res); qqline(anova.model$res)


# Test for normaly distributed residuals ----
shapiro.test(anova.model$residuals)
# should be non-significant which means
# the data do not deviate from a normal distribution

# Homogeneity of Variance - visual test ------
# GGPlot method
data_frame(
  fitted = predict(anova.model),
  residual = residuals(anova.model)) %>%
  # and then plot points and a smoothed line
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=FALSE)

# base method
plot(anova.model, 1)

# or 
plot(fitted(anova.model), residuals(anova.model))

# Homogeneity of Variance - Levene's Test 
leveneTest(mean_length ~ seed_type, data = anova.df)

# Run the ANOVA-----
# Anova Type II SS
# when you have an unbalanced design you need to use a type 3 sum of squares
# this uses the pacakge car
Anova(anova.model, type="III")


# POST F TESTS ------
# Post F Tests run if there is a significant effect----
anova.emm <- emmeans(anova.model, ~ seed_type)

# Plot means
plot(anova.emm, comparisons = TRUE)

# Mean separation
multcomp::cld(anova.emm, Letters = letters, adjust="bonferroni")

# Now to obtain the emmeans
# pairwise
anova_emminteraction = emmeans(anova.model, 
                             pairwise ~ seed_type,
                             adjust = "bonferroni")

anova_emminteraction$emmeans

anova.emmeans <- as.data.frame(anova_emminteraction$emmeans)

length.plot <- anova.emmeans %>% 
  ggplot(aes(x=seed_type)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Seed Type", y= "Seed Length") 
length.plot

ggsave(filename = "Seed Length emmeans.pdf", length.plot,
       width = 7, height = 7, units="in")





# we could look at each plant in the summary dataframe by
anova.df %>% 
  ggplot(aes(x=seed_type, y=mean_length, group=seed_type, color=seed_type)) +
  geom_point(position =  position_dodge2(width = 0.2))  +
  geom_errorbar(aes(ymin = mean_length-stddev_length_mm, ymax = mean_length + stddev_length_mm), 
                stat="identity", width = 0.2, position = position_dodge2(width = 0.2)) 


# Now for Liza's final graph
anova_sub.df <- anova.df %>% 
  select(-starts_with("stddev_"), -id)

anova_long.df <- anova_sub.df %>% 
  pivot_longer(
    !seed_type,
    names_to = "dimension",
    values_to = "values"
  )

anova_long.df %>% 
  ggplot(aes(dimension, values, color=seed_type)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3, 
               position = position_dodge(width = 0.4)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, 
               position = position_dodge(width = 0.4))+
  stat_summary(geom = 'text', fun = mean, 
               position = position_dodge(width = 0.4),
               label = c("*","*","*","*",
                         "a","ab", "ab","b",
                         "*","*","*","*"), 
               vjust = -4,
               size=5)  +
  facet_wrap(~dimension)


