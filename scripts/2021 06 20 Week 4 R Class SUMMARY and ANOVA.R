# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)

# # install new packages ----
# install.packages("car")
# install.packages("emmeans")
# install.packages("multcompView")
# install.packages("Hmisc")


# then load libraries
library(car)
library(emmeans)
library(multcompView)



# So today we are going to try a few differnet approaches
# The goal is to
#   - create file of plant measure standard deviations not in summary file
#   - join the standard deviations into the summary file
#   - plot the data to see both from the original file and the new summary file
#        - there are two ways to do this and knowing both is key for later
#   - run an ANOVA on the data by plant
#   - test assumptions
#   - if significant run post f tests
#   - maybe get into emmeans



# MYCT4seeds_FLchamber.xls - a summary file of the above dataframe and a few more
# will be called summary.df
# the data is as follows
#     id = plant id
#     type = species of plant           
#     seeds_in_sample = how many sees analyzed    
#     sample_weight_g  = weight of sample
#     thousand_weight_g = weight of 1000 seeds
#     mean_area     = mean area of seeds by plant id   
#     mean_width    = mean width of seeds    
#     mean_length   = mean length of seeds   
#     mean_circularity = mean circularity
#     mean_lw_ratio    = mean lw ratio

# we have two files 
# MYCT4seeds_FLchamber_complete data.xls - all seed sizing by plant - long
#    will be called full.df
#        id_from_main_protocol  = renamed to id - plant id
#        type = species                 
#        count_as = number seeds             
#        length_mm = length             
#        width_mm = width               
#        area_mm2 = area            
#        circularity = how circular           
#        l_w_ratio  = length divided by width 
#        datetime = the date and time of the measures
#        seed_type = genotype name of seeds



# read in files ---
# this is how you read a csv fiele - commas separated values
# this file - MYCT4seeds_FLchamber.xls - should be in your data directory
summary.df  <- read_excel("data/MYCT4seeds_FLchamber.xls") %>% clean_names()

# this file - MYCT4seeds_FLchamber_complete data.xls - should be in your data directory
full.df <- read_excel("data/MYCT4seeds_FLchamber_complete data.xls") %>% clean_names()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Clean up the summary dataframe ---
# change data in the dataframe
# rename the variables and make id numeric ----
summary.df <- summary.df %>% 
  rename(
    type                = main_seed,
    seeds_in_sample     = main_seeds,
    sample_weight_g     = weight_g,
    thousand_weight_g   = tgw_g,
    mean_area           = o_area,
    mean_width          = o_width,
    mean_length         = o_length ,
    mean_circularity    = o_circularity,
    mean_lw_ratio       = ol_w_ratio,
  )

# make id numeric
summary.df <- summary.df %>% 
  mutate(id = as.numeric(id))

# now lets make each id a factor and seed_type
summary.df <- summary.df %>% 
  mutate(id = as.factor(id),
         seed_type = as.factor(seed_type))

# Lubridate and paste to make a datetime column
summary.df <- summary.df %>% 
  mutate(datetime = paste(date, time, sep=" "))

# Make data time a datetime variable
summary.df <- summary.df %>% 
  mutate(datetime = dmy_hm(datetime))

# select to remove junk columns
summary.df <- summary.df %>% 
  select(-date, -time)

# Filter out empty rows ----
summary.df <- summary.df %>% 
  filter(!is.na(id))

# Now lets add in the real names for the plants and what they are
# First list the plants to console to copy here
noquote(summary.df$id)

summary.df <- summary.df %>% 
  mutate(seed_type = case_when(
    id %in% c(4298, 4299, 4300, 4301, 4302, 4303, 4304, 4305) ~ "wildtype",
    id %in% c(4306, 4307, 4308, 4309, 4310, 4311, 4312, 4313) ~ "mutant 11B",
    id %in% c(4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321) ~ "mutatn 8A",
    id %in% c(4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329) ~ "mutant 7B",
    TRUE~"other"))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Clean up the full dataframe ----
# Full Marvin Data Summary ----
# first rename id_from_main_protocol to id 
# Make it numeric and then make it a factor 

full.df <- full.df %>% 
  rename(id = id_from_main_protocol)

# make numeric
full.df <- full.df %>% 
  mutate(id = as.numeric(id))

# make id and seedtype a factor
full.df <- full.df %>% 
  mutate(id = as.factor(id),
         seed_type = as.factor(seed_type))

# remember above you need to make ID Numeric - how do you do it.

# Case_when for plant line
# lets add in the type of line the plants come from 
# lets change ID to the plant type like above So how would you do it???
full.df <- full.df %>% 
  mutate(seed_type = case_when(
    id %in% c(4298, 4299, 4300, 4301, 4302, 4303, 4304, 4305) ~ "wildtype",
    id %in% c(4306, 4307, 4308, 4309, 4310, 4311, 4312, 4313) ~ "mutant 11B",
    id %in% c(4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321) ~ "mutatn 8A",
    id %in% c(4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329) ~ "mutant 7B",
    TRUE~"other"))
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Make a summary stats dataframe ----
# here we are only going to do standard deviations 
std_dev.df <- full.df %>% 
  group_by(id, type) %>% 
  summarise(
    stddev_length_mm = sd(length_mm, na.rm=TRUE),
    stddev_width_mm = sd(width_mm, na.rm=TRUE)
    )
  

# Joining files ----
# Note that the std_dev.df dataframe is 32 observations and so is summary.df
# the id is the same in both and we want to link them based on this.
# we can save it to the summary dataframe

summary.df <- full_join(summary.df, std_dev.df, by="id")


# We can plot the summary data in two ways - how did we do it before 
# using mean and se?
# note we want standard deviation - how can you find this - try googling  - ggplot stat_summary stddev
# look at https://stackoverflow.com/questions/55943000/how-to-add-error-bars-with-standard-deviation-using-ggplot2
# and this https://stackoverflow.com/questions/41848271/ggplot2-stat-summary-mean-sdl-not-the-same-as-mean-sd

full.df %>% 
  ggplot(aes(seed_type, length_mm, color=seed_type)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.data = mean_sdl, na.rm = TRUE, fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2) 

# now how would you do this on the summary dataframe
summary.df %>% 
  ggplot(aes(seed_type, mean_length, color=seed_type)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.data = mean_sdl, na.rm = TRUE, fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2) 


# ANOVA - TESTING FOR DIFFERNECES IN LENGTH ------
# options settings for proper test---
options(contrasts = c("contr.sum", "contr.poly"))

# we can cover this later but reording the factors is something often nice

# ANOVA MODEL
length.model<- lm(mean_length ~ seed_type, data = summary.df)


# Check assumptions
# Normality Assumption graphical QQ PLOT ----
plot(length.model,2)

# check for normally distributed data
qqnorm(length.model$res)

# Noramlity Stats
shapiro.test(length.model$residuals)
# should be non-significant which means
# does not deviate from a normal distribution

# Homogeneity of Variance - visual
plot(length.model,1)
# or 
plot(fitted(length.model), residuals(length.model))

# Homogeneity of Variance - Levene Test 
leveneTest(mean_length ~ seed_type, data = summary.df)

# now run the ANOVA-----
# Anova Type II SS
Anova(length.model, type="III")

# POST F TESTS ------
# Post F Tests run if there is a significant effect----
length.emm<- emmeans(length.model, ~ seed_type)

# Plot means
plot(length.emm, comparisons = TRUE)

# Mean separation
multcomp::cld(length.emm, Letters = letters,adjust="bonferroni")


# now to plot emmeans
# pairwise
length_emminteraction = emmeans(length.model, 
                             pairwise ~ seed_type,
                             adjust="bonferroni")

length_emminteraction$emmeans
length.emmeans <- as.data.frame(length_emminteraction$emmeans)

length.plot <- length.emmeans %>% 
  ggplot(aes(x=seed_type)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Seed Type", y= "Seed Length") 
length.plot

ggsave(filename = "Seed Length emmeans.pdf", length.plot,
       width = 7, height = 7, units="in")

















# we could look at each plant in the summary dataframe by
summary.df %>% 
  ggplot(aes(x=seed_type, y=mean_length, group=seed_type, color=seed_type)) +
  geom_point(position =  position_dodge2(width = 0.2))  +
  geom_errorbar(aes(ymin = mean_length-stddev_length_mm, ymax = mean_length + stddev_length_mm), 
                stat="identity", width = 0.2, position = position_dodge2(width = 0.2)) 



