# install new library
install.packages("hms")

# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(hms)


# read in files ----
# this is how you read a csv file - commas separated values
# this file - reduced_mutant_seed_parameters.csv - should be in your data directory
trap.df  <- read_excel("data/Research Project Excel (Data) - Copy.xlsx", sheet="R Code June") %>% 
  clean_names()

# I am going to rename columns a bit ---
trap.df <- trap.df %>% 
  rename(
    date = when_date,
    location = where_six_mile_creek_vs_denman_creek,
    time = time_of_day,
  )

# note there are extra empty rows - we can remove them----
trap.df <- trap.df %>% 
  filter(!is.na(date))

# trap.df  <- read_csv("data/Research Project Excel (Data) - Copy.csv") %>% 
#   clean_names()

# one issue is that the time of day and dates are separate - how can we fix this?
# extract only time ----
trap.df <- trap.df %>% 
  mutate(time = as_hms(time))

# paste date and time together----
trap.df <- trap.df %>% 
  mutate(datetime = ymd_hms(paste(date, time, sep = " ")))

# now to summarize data
# we want the number of animals per month at each site
# we can make a new dataframe (first) or we can graph the data

# Summarize animals by location and month -----
june.df <- trap.df %>% 
  group_by(location) %>% 
  summarize(total_animals = length(organism_type))

# more advanced if there are missing values----
june.df <- trap.df %>% 
  group_by(location) %>% 
  summarize(total_animals = sum(!is.na(organism_type)))

# summarize animals by location and hour -----
june_hour.df <- trap.df %>% 
  group_by(location, time) %>% 
  summarize(total_animals = length(organism_type))

# Summarize animals by type by hour ------
june_animal_hour.df <- trap.df %>% 
  group_by(organism_type, time) %>% 
  summarize(total_animals = sum(!is.na(organism_type)))

# Now lets make the graphs -----
# animals monthly ------
june.df %>% 
  ggplot(aes(location, total_animals)) +
  geom_col()

# animals hourly ------
june_hour.df %>% 
  ggplot(aes(time, total_animals, color=location, fill=location)) +
  geom_bar(stat="identity",  position = position_dodge(preserve = "single"))

# animals type hourly ------
june_animal_hour.df %>% 
  ggplot(aes(time, total_animals, color=organism_type, fill=organism_type)) +
  geom_bar(stat="identity",  position = position_dodge(preserve = "single"))

# Now to do all in one if you wanted
# animals monthly no df ------
trap.df %>% 
  group_by(location) %>% 
  summarize(total_animals = sum(!is.na(organism_type))) %>% 
  ggplot(aes(location, total_animals)) +
  geom_col()
