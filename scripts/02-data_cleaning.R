#### Preamble ####
# Purpose: Cleans the raw data recorded by Hunt Allcot, Luca Braghieri, 
#          Sarah Eichmeyer and Matthew Gentzkow
# Author: Kenneth Chan, 
# Date: 11 February 2023 
# Contact: kenchancf0618@gmail.com
# License: MIT
# Pre-requisites: Digital addiction by Hunt Allcott, Matthew Gentzkow and
#                 Lena Song
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(haven)
library(readxl)
library(janitor)
library(stringr)
library(yaml)

# Load the YAML file
yaml_data <- yaml::yaml.load_file("data/raw_data/config.yaml")

# Access date metadata
start_experiment <- as.Date(mdy(yaml_data$metadata$dates$start_experiment))
midline <- as.Date(mdy(yaml_data$metadata$dates$midline))
endline <- as.Date(mdy(yaml_data$metadata$dates$endline))
daylight_saving_2018 <- as.Date("2018-11-3")


# Extract state information from baseline
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")

cleaned_baseline <- raw_baseline |>
  clean_names() |>
  filter(consent1 == "Continue")
  

# Fill in missing state abbreviations for individuals who qualified for 
# the midline but whose Zip codes weren't found
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Mississippi" & 
                       cleaned_baseline$state == ""] <- "MS"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Wisconsin" & 
                       cleaned_baseline$state == ""] <- "WI"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Utah" & 
                       cleaned_baseline$state == ""] <- "UT"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "California" & 
                       cleaned_baseline$state == ""] <- "CA"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Rhode Island" & 
                       cleaned_baseline$state == ""] <- "RI"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Florida" & 
                       cleaned_baseline$state == ""] <- "FL"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Iowa" & 
                       cleaned_baseline$state == ""] <- "IA"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Ohio" & 
                       cleaned_baseline$state == ""] <- "OH"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Massachusetts" & 
                       cleaned_baseline$state == ""] <- "MA"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Indiana" & 
                       cleaned_baseline$state == ""] <- "IN"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Alabama" & 
                       cleaned_baseline$state == ""] <- "AL"
cleaned_baseline$state[cleaned_baseline$state_prescreen == "Michigan" & 
                       cleaned_baseline$state == ""] <- "MI"

# Drop the state_prescreen variable
cleaned_baseline$state_prescreen <- NULL

social_time <- cleaned_baseline |>
  select(id, leisure_activities2_5, fb_minutes) |>
  # Remove non valid entries
  filter(leisure_activities2_5 != "" & fb_minutes != "")|>
  # Mutate response on other social media to numeric value
  mutate(other_social_minutes = recode(leisure_activities2_5,
                                       "0 minutes" = 0,
                                       "Between 1 and 30 minutes" = 15,
                                       "Between 31 minutes and 1 hour" = 45,
                                       "Between 1 and 2 hours" = 90,
                                       "Between 2 and 3 hours" = 150,
                                       "More than 3 hours" = 180,
                                       .default = -1),
         fb_minutes = as.numeric(fb_minutes),
         sum_social_minutes = fb_minutes + other_social_minutes) |>
  rename(other_social_response = leisure_activities2_5)

#### Save data ####
write_csv(social_time, "data/analysis_data/social_time.csv")

#### TODO: DELETE IT ####
raw_data <- read_dta("data/raw_data/politics_anonymous.dta")
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")
raw_midline <- read_dta("data/raw_data/midline_anonymous.dta")
raw_endline <- read_dta("data/raw_data/endline_anonymous.dta")
raw_postendline <- read_dta("data/raw_data/postendline_anonymous.dta")

