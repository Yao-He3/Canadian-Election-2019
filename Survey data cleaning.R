#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded 
# Author: Yao He
# Data: 8 December 2020
# Contact: yaobb.he@mail.utoronto.ca 
# License: ???
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!???

install.packages("interactions")
#### Workspace setup ####
library(haven)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(interactions)

# install cesR package
devtools::install_github("hodgettsp/cesR")

# load cesR package and labelled package
library(cesR)
library(labelled)

# call 2019 CES online survey
get_ces("ces2019_web")

# convert values to factor type
raw_data_survey <- to_factor(ces2019_web)


# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)

# Just keep some variables that may be of my interest 
selected_data_survey <- 
  raw_data_survey %>% 
  select(cps19_yob,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_votechoice)


#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# match census variables' name to survey data
# change sex to gender
selected_data_survey <-
  selected_data_survey %>% 
  rename(sex = cps19_gender) %>% 
  filter(sex == "A man" | sex == "A woman") %>%
  mutate(sex = ifelse(sex == "A man", "Male", "Female"))

# Calculate the age from the year of birth
# Match the variable names in survey data to the census data
selected_data_survey$cps19_yob <- as.numeric(selected_data_survey$cps19_yob)
age = 100 - selected_data_survey$cps19_yob
full_selected_data_survey <- cbind(selected_data_survey, age)
full_selected_data_survey <- 
  full_selected_data_survey %>%
  select(age,
         sex,
         cps19_province,
         cps19_education,
         cps19_votechoice) %>%
  rename(province = cps19_province) %>%
  filter(age >= 18)

# Match the education levels to the census data
full_selected_data_survey$education[full_selected_data_survey$cps19_education == "No schooling" |
                                      full_selected_data_survey$cps19_education == "Some elementary school" |
                                      full_selected_data_survey$cps19_education == "Completed elementary school" |
                                      full_selected_data_survey$cps19_education == "Some secondary/ high school"] <- "Less than high school diploma or its equivalent" 
                                    

full_selected_data_survey$education[full_selected_data_survey$cps19_education == "Some technical, community college, CEGEP, College Classique" |
                                      full_selected_data_survey$cps19_education == "Completed secondary/ high school" | 
                                      full_selected_data_survey$cps19_education == "Some university"] <- "High school diploma or a high school equivalency certificate"
full_selected_data_survey$education[full_selected_data_survey$cps19_education == 
                                      "Completed technical, community college, CEGEP, College Classique"] <- 
  "College, CEGEP or other non-university certificate"
full_selected_data_survey$education[full_selected_data_survey$cps19_education == 
                                      "Bachelor's degree"] <- 
  "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"
full_selected_data_survey$education[full_selected_data_survey$cps19_education == 
                                      "Master's degree" | 
                                      full_selected_data_survey$cps19_education == 
                                      "Professional degree or doctorate"] <- 
  "University certificate, diploma or degree above the bachelor's level"

# Replace the previous education column
full_selected_data_survey <- 
  full_selected_data_survey %>%
  select(age,
         sex,
         province,
         education,
         cps19_votechoice)

# Omit the education NA cases in the survey dataset
full_selected_data_survey <- 
  full_selected_data_survey %>%
  na.omit(education)

# Assign value 1 to cases that vote for Liberal
full_selected_data_survey$vote_Trudeau[full_selected_data_survey$cps19_votechoice == "Liberal Party"] <- 1
full_selected_data_survey$vote_Trudeau[full_selected_data_survey$cps19_votechoice != "Liberal Party"] <- 0
full_selected_data_survey$vote_Scheer[full_selected_data_survey$cps19_votechoice == "Conservative Party"] <- 1
full_selected_data_survey$vote_Scheer[full_selected_data_survey$cps19_votechoice != "Conservative Party"] <- 0


full_selected_data_survey <- 
  full_selected_data_survey %>%
  select(age,
         sex,
         province,
         education,
         vote_Trudeau,
         vote_Scheer)


# group
full_selected_data_survey <- 
  full_selected_data_survey %>%
  count(age, sex, province, education, vote_Trudeau, vote_Scheer) %>%
  group_by(age, sex, province, education, vote_Trudeau, vote_Scheer) 

# Saving the census data as a csv file in my
# working directory
write_csv(full_selected_data_survey, "survey_data.csv")




