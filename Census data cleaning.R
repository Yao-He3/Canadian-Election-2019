#### Preamble ####
# Purpose: Prepare and clean the census data downloaded 
# Author: Yao He
# Data: 8 December 2020
# Contact: yaobb.he@mail.utoronto.ca 
# License: ???
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!???


#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)


# Read in the raw data.
raw_data_census <- read.csv("AAu2nY9y.csv")


# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of my interest 
selected_data_census <- 
  raw_data_census %>% 
  select(CASEID,
         agec,
         sex,
         prv,
         ehg3_01b)


#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)


# Remove age below 18 and change the column name "agec" to "age"
selected_data_census$agec <- as.numeric(selected_data_census$agec)
selected_data_census <- 
  selected_data_census %>% 
  filter(agec >= 18 & agec != 96 & agec != 97) %>%
  rename(age = agec)

# Change the sex column from "0, 1" to "Female, Male"
selected_data_census$sex[selected_data_census$sex == 1] <- "Male"
selected_data_census$sex[selected_data_census$sex == 2] <- "Female"
selected_data_census <- 
  selected_data_census %>% 
  na.omit()

  

# Change the education column from numbers to groups
selected_data_census$education[selected_data_census$ehg3_01b == 1] <- "Less than high school diploma or its equivalent" 
selected_data_census$education[selected_data_census$ehg3_01b == 2 |
                                 selected_data_census$ehg3_01b == 3 |
                                 selected_data_census$ehg3_01b == 4 |
                                 selected_data_census$ehg3_01b == 5 |
                                 selected_data_census$ehg3_01b == 6 ] <- "High school diploma or a high school equivalency certificate"
selected_data_census$education[selected_data_census$ehg3_01b == 7|
                                 selected_data_census$ehg3_01b == 8 | 
                                 selected_data_census$ehg3_01b == 9 ] <- "College, CEGEP or other non-university certificate"
selected_data_census$education[selected_data_census$ehg3_01b == 10] <- "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"
selected_data_census$education[selected_data_census$ehg3_01b == 11] <- "University certificate, diploma or degree above the bachelor's level"
selected_data_census$ehg3_01b <- NULL

selected_data_census <- 
  selected_data_census %>% 
  na.omit()



# Change the province column from numbers to groups
selected_data_census$province[selected_data_census$prv == 10] <- "Newfoundland and Labrador"
selected_data_census$province[selected_data_census$prv == 11] <- "Prince Edward Island"
selected_data_census$province[selected_data_census$prv == 12] <- "Nova Scotia"
selected_data_census$province[selected_data_census$prv == 13] <- "New Brunswick"
selected_data_census$province[selected_data_census$prv == 24] <- "Quebec"
selected_data_census$province[selected_data_census$prv == 35] <- "Ontario"
selected_data_census$province[selected_data_census$prv == 46] <- "Manitoba"
selected_data_census$province[selected_data_census$prv == 47] <- "Saskatchewan"
selected_data_census$province[selected_data_census$prv == 48] <- "Alberta"
selected_data_census$province[selected_data_census$prv == 59] <- "British Columbia"
selected_data_census$prv <- NULL

selected_data_census <- 
  selected_data_census %>% 
  na.omit()

# group
selected_data_census <- 
  selected_data_census %>%
  count(age, sex, province, education) %>%
  group_by(age, sex, province, education) 

# Saving the census data as a csv file in my
# working directory
write_csv(selected_data_census, "census_data.csv")




