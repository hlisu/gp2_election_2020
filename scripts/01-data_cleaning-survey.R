#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Haili Su
# Data: 26 October 2020
# Contact: haili.su@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data
raw_data <- read_dta("inputs/data/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(census_region,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         gender,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         age)

# rename some variables 
survey <- reduced_data %>% rename(
  region = census_region,
  reg = registration,
  intention = vote_intention,
  edu = education,
  race = race_ethnicity,
  hisp = hispanic,
  income = household_income,
  )

## Clean up##

#Change voter registration data into binary 1/0 variable
reg_sta <- reduced_data %>% 
  mutate(reg_num = case_when(
    registration == "Registered" ~ 1L,
    registration == "Not registered" ~ 0L,
    registration == "Don't know" ~ 0L,
    TRUE ~ 0L
  )) %>% select(reg_num) %>% pull()

#Change age data into 10-year age groups (except for 18-24 and 75+):
age_gp <- reduced_data %>% 
  mutate(age_gr = case_when(
    18 <= age & age <= 24 ~ "18",
    25 <= age & age <= 34 ~ "25",
    35 <= age & age <= 44 ~ "35",
    45 <= age & age <= 54 ~ "45",
    55 <= age & age <= 64 ~ "55",
    65 <= age & age <= 74 ~ "65",
    75 <= age ~ "75",
    TRUE ~ "NA"
  )) %>% select(age_gr) %>% pull()

#Change Hispanic data in to binary 1/0 (Hispanic or not) variable:
hisp_sta <- reduced_data %>%
  mutate(hisp_num = case_when(
    hispanic == "Not Hispanic" ~ 0L,
    TRUE ~ 1L
  )) %>% select(hisp_num) %>% pull()

#Change education data in to binary 1/0 (college or not) variable:
edu_sta <- reduced_data %>%
  mutate(edu_bi = case_when(
    education == "3rd Grade or less" ~ 0L,
    education == "Middle School - Grades 4 - 8" ~ 0L,
    education == "Completed some high school" ~ 0L,
    education == "High school graduate" ~ 0L,
    education == "Other post high school vocational training" ~ 0L,
    TRUE ~ 1L
  )) %>% select(edu_bi) %>% pull()

#Simplify the voting intention variable:
int_sta <- reduced_data %>%
  mutate(int_s = case_when(
    vote_intention == "Yes, I will vote" ~ "Yes",
    vote_intention == "No, I will not vote but I am eligible" ~ "No",
    vote_intention == "No, I am not eligible to vote" ~ "Not eligible",
    vote_intention == "Not sure" ~ "Not sure",
    TRUE ~ "NA"
  )) %>% select(int_s) %>% pull()

#Simplify the race data by reducing the number of categories:
race_sta <- reduced_data %>%
  mutate(race_sim = case_when(
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "Black, or African American" ~ "Black",
    race_ethnicity == "American Indian or Alaska Native" ~ "Native",
    race_ethnicity == "Asian (Asian Indian)" ~ "API",
    race_ethnicity == "Asian (Chinese)" ~ "API",
    race_ethnicity == "Asian (Filipino)" ~ "API",
    race_ethnicity == "Asian (Japanese)" ~ "API",
    race_ethnicity == "Asian (Korean)" ~ "API",
    race_ethnicity == "Asian (Vietnamese)" ~ "API",
    race_ethnicity == "Asian (Other)" ~ "API",
    race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "API",
    race_ethnicity == "Pacific Islander (Guamainian)" ~ "API",
    race_ethnicity == "Pacific Islander (Samoan)" ~ "API",
    race_ethnicity == "Pacific Islander (Other)" ~ "API",
    race_ethnicity == "Some other race" ~ "Other",
    TRUE ~ "NA"
  )) %>% select(race_sim) %>% pull()

#Simplify the income data in to three categories:
income_sta <- reduced_data %>%
  mutate(income_tri = case_when(
    household_income == "Less than $14,999" ~ "<50K",
    household_income == "$15,000 to $19,999" ~ "<50K",
    household_income == "$20,000 to $24,999" ~ "<50K",
    household_income == "$25,000 to $29,999" ~ "<50K",
    household_income == "$30,000 to $34,999" ~ "<50K",
    household_income == "$35,000 to $39,999" ~ "<50K",
    household_income == "$40,000 to $44,999" ~ "<50K",
    household_income == "$45,000 to $49,999" ~ "<50K",
    household_income == "$50,000 to $54,999" ~ "50K-100K",
    household_income == "$55,000 to $59,999" ~ "50K-100K",
    household_income == "$60,000 to $64,999" ~ "50K-100K",
    household_income == "$65,000 to $69,999" ~ "50K-100K",
    household_income == "$70,000 to $74,999" ~ "50K-100K",
    household_income == "$75,000 to $79,999" ~ "50K-100K",
    household_income == "$80,000 to $84,999" ~ "50K-100K",
    household_income == "$85,000 to $89,999" ~ "50K-100K",
    household_income == "$90,000 to $94,999" ~ "50K-100K",
    household_income == "$95,000 to $99,999" ~ "50K-100K",
    household_income == "$100,000 to $124,999" ~ ">100K",
    household_income == "$125,000 to $149,999" ~ ">100K",
    household_income == "$150,000 to $174,999" ~ ">100K",
    household_income == "$175,000 to $199,999" ~ ">100K",
    household_income == "$200,000 to $249,999" ~ ">100K",
    household_income == "$250,000 and above" ~ ">100K",
    TRUE ~ "NA"
    )) %>% select(income_tri) %>% pull()

#replace values with the mutated variable: 
survey <- survey %>% mutate(reg = reg_sta,
                            age = age_gp,
                            hisp = hisp_sta,
                            race = race_sta,
                            edu = edu_sta,
                            intention = int_sta,
                            income = income_sta)
#create the .csv file:
write.csv(survey, "outputs/data/survey.csv")
