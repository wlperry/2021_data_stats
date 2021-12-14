# install packages ----
# these shoudl be installed already and are commented out so they wont run
# unless you remove the '#' 

# The ones to be sure are installed this week are ---
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")
# install.packages("skimr")


# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)


# read in files ---
# this is how you do a csv fiele - commas separated values
pc.df <- read_csv("data/Liza_MYCT8F_complete data from Marvin.csv") %>% clean_names()

names(pc.df)

# this is how you import excel
pc_excel.df <- read_excel("data/Liza_MYCT8F_complete data from Marvin.xls") %>% clean_names()

# the dataframe ----
# How to see variable names?
# replace `dataframe` with the name of the dataframe
# - do you see an issue? Note `Name`
# what is the easy way to fix this?
names(pc_excel.df)

# look at the top of the dataframe
head(pc.df)

# to look at only one column you can use the dataframe$column
pc.df$length_mm

# How to summarize data
# summary(dataframe)
summary(pc.df)

# use skimr - skim(dataframe)
skim(pc.df)

pc.df %>% group_by(id_from_main_protocol) %>% skim()

# Graphing the data -----
ggplot(pc.df, aes(x=as.factor(id_from_main_protocol), y=length_mm )) +
  geom_boxplot()


# Graphing mean and standard error ----
ggplot(pc.df, aes(x=as.factor(id_from_main_protocol), y=area_mm2, color= as.factor(id_from_main_protocol))) +
   stat_summary(fun = mean, na.rm = TRUE,
             geom = "point",
             size = 1) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
theme_bw()


ggplot(pc.df, aes(x=id_from_main_protocol, y=area_mm2 ))  +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 1) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) 



ggplot(pc.df, aes(x=length_mm, y=width_mm )) +
  geom_point()

ggplot(pc.df, aes(x=length_mm, y=width_mm, color=as.factor(id_from_main_protocol) )) +
  geom_point(position = position_jitter(width=0.02), alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~as.factor(id_from_main_protocol))
