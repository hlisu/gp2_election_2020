#### Preamble ####
# Purpose: Prepare and clean the American Community Survey (2018) data downloaded from IPUMS
# Author: Haili Su
# Data: 26 October 2020
# Contact: haili.su@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data_acs <- read_dta("inputs/data/usa_00003.dta")
# Add the labels
raw_data_acs <- labelled::to_factor(raw_data_acs)

# Just keep some variables that may be of interest:
names(raw_data_acs)

reduced_data <- 
  raw_data_acs %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         educ) %>% mutate(age = as.integer(age))
rm(raw_data_acs)
        
# rename some variables 
post_acs <- reduced_data %>% rename(
  state = stateicp,
  edu = educ,
  gender = sex,
  hisp = hispan,
)

## Clean up##

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

#Capitalize the first letter of gender variable to keep up with our survey data:
gender_cap <- reduced_data %>%
  mutate(gender_c = case_when(
    sex == "female" ~ "Female",
    sex == "male" ~ "Male"
  )) %>% select(gender_c) %>% pull()

#Change Hispanic data in to binary 1/0 (Hispanic or not) variable:
hisp_sta <- reduced_data %>%
  mutate(hisp_num = case_when(
    hispan == "not hispanic" ~ 0L,
    hispan == "not reported" ~ NA_integer_,
    TRUE ~ 1L
  )) %>% select(hisp_num) %>% pull()

#Change education data in to binary 1/0 (college or not) variable:
edu_sta <- reduced_data %>%
  mutate(edu_bi = case_when(
    educ == "n/a or no schooling" ~ 0L,
    educ == "nursery school to grade 4" ~ 0L,
    educ == "grade 5, 6, 7, or 8" ~ 0L,
    educ == "grade 9" ~ 0L,
    educ == "grade 10" ~ 0L,
    educ == "grade 11" ~ 0L,
    educ == "grade 12" ~ 0L,
    TRUE ~ 1L
  )) %>% select(edu_bi) %>% pull()

#Simplify the race data by reducing the number of categories:
race_sta <- reduced_data %>%
  mutate(race_sim = case_when(
    race == "white" ~ "White",
    race == "black/african american/negro" ~ "Black",
    race == "american indian or alaska native" ~ "Native",
    race == "chinese" ~ "API",
    race == "japanese" ~ "API",
    race == "other asian or pacific islander" ~ "API",
    race == "other race, nec" ~ "Other",
    race == "two major races" ~ "Other",
    race == "three or more major races" ~ "Other",
    TRUE ~ "NA"
  )) %>% select(race_sim) %>% pull()

#Change the state names into two-letter abbreviations to keep up with the survey data we use:
state_lab <- reduced_data %>%
  mutate(state_id = case_when(
    stateicp == "alabama" ~ "AL",
    stateicp == "alaska" ~ "AK",
    stateicp == "arizona" ~ "AZ",
    stateicp == "arkansas" ~ "AR",
    stateicp == "california" ~ "CA",
    stateicp == "colorado" ~ "CO",
    stateicp == "connecticut" ~ "CT",
    stateicp == "delaware" ~ "DE",
    stateicp == "florida" ~ "FL",
    stateicp == "georgia" ~ "GA",
    stateicp == "hawaii" ~ "HI",
    stateicp == "idaho" ~ "ID",
    stateicp == "illinois" ~ "IL",
    stateicp == "indiana" ~ "IN",
    stateicp == "iowa" ~ "IA",
    stateicp == "kansas" ~ "KS",
    stateicp == "kentucky" ~ "KY",
    stateicp == "louisiana" ~ "LA",
    stateicp == "maine" ~ "ME",
    stateicp == "maryland" ~ "MD",
    stateicp == "massachusetts" ~ "MA",
    stateicp == "michigan" ~ "MI",
    stateicp == "minnesota" ~ "MN",
    stateicp == "mississippi" ~ "MS",
    stateicp == "missouri" ~ "MO",
    stateicp == "montana" ~ "MT",
    stateicp == "nebraska" ~ "NE",
    stateicp == "nevada" ~ "NV",
    stateicp == "new hampshire" ~ "NH",
    stateicp == "new jersey" ~ "NJ",
    stateicp == "new mexico" ~ "NM",
    stateicp == "new york" ~ "NY",
    stateicp == "north carolina" ~ "NC",
    stateicp == "north dakota" ~ "ND",
    stateicp == "ohio" ~ "OH",
    stateicp == "oklahoma" ~ "OK",
    stateicp == "oregon" ~ "OR",
    stateicp == "pennsylvania" ~ "PA",
    stateicp == "rhode island" ~ "RI",
    stateicp == "south carolina" ~ "SC",
    stateicp == "south dakota" ~ "SD",
    stateicp == "tennessee" ~ "TN",
    stateicp == "texas" ~ "TX",
    stateicp == "utah" ~ "UT",
    stateicp == "vermont" ~ "VT",
    stateicp == "virginia" ~ "VA",
    stateicp == "washington" ~ "WA",
    stateicp == "west virginia" ~ "WV",
    stateicp == "wisconsin" ~ "WI",
    stateicp == "wyoming" ~ "WY",
    stateicp == "district of columbia" ~ "DC",
    TRUE ~ "Others"
  )) %>% select(state_id) %>% pull()

#Use wider census region labels to keep up with the survey data we use:
region_lab <- reduced_data %>% mutate(
  region_new = case_when(
    region == "east north central div" ~ "Midwest",
    region == "west north central div" ~ "Midwest",
    region == "new england division" | region == "middle atlantic division" ~ "Northeast",
    region == "south atlantic division" | region == "east south central div" | region == "west south central div" ~ "South",
    region == "mountain division" | region == "pacific division" ~ "West",
    TRUE ~ "NA"
  )
) %>% select(region_new) %>% pull()

#Replace with the mutated values:
post_acs <- post_acs %>% mutate(age = age_gp,
                            hisp = hisp_sta,
                            race = race_sta,
                            edu = edu_sta,
                            state = state_lab,
                            gender = gender_cap,
                            region = region_lab)

#Create cell counts:
cell_counts <- post_acs %>% group_by(state, gender, as.factor(age), race, hisp, edu) %>% count() %>% rename(age = "as.factor(age)")

#Create a .csv file
write.csv(cell_counts, "cells_post.csv")

         