# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)

# read in files ---
# this is how you do a csv fiele - commas separated values
summary_marvin.df  <- read_excel("data/MYCT4seeds_FLchamber.xls") %>% clean_names()
full_marvin.df <- read_excel("data/MYCT4seeds_FLchamber_complete data.xls") %>% clean_names()

# Cleaning data ----
# so lets clean the data and make it a bit more useable
# note in the full.df 
#    id is a character need to be numeric
#    rename columns
#    date and time are characters - lets make it a date time column - lubridate
#    remove the date time columns - select
#    there is an empty row - remove with filter


# Later down the list is summarization of data
# Data Summary -----
# we need to:
#       get mean and standard deviations by plant and counts

# change data in the dataframe
summary_marvin.df <- summary_marvin.df %>% 
  mutate(id = as.numeric(id))

# Rename columns ----
# get the names
names(summary_marvin.df)
# now get names with no quotes for the command below
noquote(names(summary_marvin.df))

# rename the varaibles ----
summary_marvin.df <- summary_marvin.df %>% 
  rename(
    species          = main_seed,
    seed_number      = main_seeds,
    total_weigth_g   = weight_g,
    mean_weight_g    = tgw_g,
    mean_area        = o_area,
    mean_width       = o_width,
    mean_length      = o_length ,
    mean_circularity = o_circularity,
    mean_lw_ratio   = ol_w_ratio,
  )%>% 
  mutate(id = as.numeric(id))

# Lubridate and paste to make a datetime column
summary_marvin.df <- summary_marvin.df %>% 
  mutate(datetime = paste(date, time, sep=" "))

# Make data time a datetime variable
summary_marvin.df <- summary_marvin.df %>% 
  mutate(datetime = dmy_hm(datetime))

# select to remove junk columns
summary_marvin.df <- summary_marvin.df %>% 
  select(-date, -time)

# Filter out Rows ----
summary_marvin.df <- summary_marvin.df %>% 
  filter(!is.na(id))


# Now lets add in the real names for the plants and what they are
# First list the plants to console to copy here
noquote(summary_marvin.df$id)

summary_marvin.df <- summary_marvin.df %>% 
  mutate(seed_type = case_when(
    id %in% c(4298, 4299, 4300, 4301, 4302, 4303, 4304, 4305) ~ "wildtype",
    id %in% c(4306, 4307, 4308, 4309, 4310, 4311, 4312, 4313) ~ "mutant 11B",
    id %in% c(4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321) ~ "mutatn 8A",
    id %in% c(4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329) ~ "mutant 7B",
    TRUE~"other"))
         
# now lets make each seed_type a factor
summary_marvin.df <- summary_marvin.df %>% 
  mutate(seed_type = as.factor(seed_type))

# Full Marvin Data Summary ----
# first lets get id as a column name as this one annoys me ; )
# and change it to numeric
full_marvin.df <- full_marvin.df %>% 
  rename(id = id_from_main_protocol) %>% 
  mutate(id = as.numeric(id))

# lets change ID to the plant type like above
full_marvin.df <- full_marvin.df %>% 
  mutate(seed_type = case_when(
    id %in% c(4298, 4299, 4300, 4301, 4302, 4303, 4304, 4305) ~ "wildtype",
    id %in% c(4306, 4307, 4308, 4309, 4310, 4311, 4312, 4313) ~ "mutant 11B",
    id %in% c(4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321) ~ "mutatn 8A",
    id %in% c(4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329) ~ "mutant 7B",
    TRUE~"other"))

# Making summary stats
# now lets make a new dataframe that has our own summary stats 
full_marvin_summary.df <- full_marvin.df %>% 
  group_by(id, seed_type) %>% 
  summarise(
    seed_count = length(count_as), # what is there were NA values - 
    seed_count_2 = sum(!is.na(count_as)),
    mean_length_mm = mean(length_mm, na.rm=TRUE),
    stddev_length_mm = sd(length_mm, na.rm=TRUE),
    mean_width_mm = mean(width_mm, na.rm=TRUE),
    stddev_width_mm = sd(width_mm, na.rm=TRUE),
    mean_area_mm2 = mean(area_mm2, na.rm=TRUE),
    stddev_area_mm2 = sd(area_mm2, na.rm=TRUE),
    mean_circularity = mean(circularity, na.rm=TRUE),
    stddev_circularity = sd(circularity, na.rm=TRUE),
    mean_lw_ratio = mean(l_w_ratio, na.rm=TRUE),
    stddev_lw_ratio = sd(l_w_ratio, na.rm=TRUE),
    )
  

# Now the cool thing is to join the files ----
complete_summary_marvin.df <- full_join(full_marvin_summary.df, summary_marvin.df, 
                                        by=c("id", "seed_type"), 
                                        suffix=c("_stats", "_original"))


# plot of data with std dev
full_marvin_summary.df %>% 
  ggplot(aes(seed_type, mean_length_mm, color=seed_type)) +
  geom_point(position=position_jitter(width=0.2))+
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) 


k
