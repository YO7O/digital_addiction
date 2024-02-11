#### Preamble ####
# Purpose: Cleans the raw data recorded 
# Author: 
# Date: 11 February 2023 
# Contact: 
# License: MIT
# Pre-requisites: 
# Any other information needed? 

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


#####################################################
###### Import, clean and reshape SMS responses ######
#####################################################
# Import timezone information
raw_timezones <- read_excel("data/raw_data/us_timezones.xlsx")

statetime <- raw_timezones |>
  mutate(state = str_extract(state, "[A-Z]{2}"),
         timezone = str_extract(timezone, "[A-Z]{3,4}"))


# Extract state information from baseline
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")

cleaned_baseline <- raw_baseline |>
  clean_names()

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

# Import SMS responses
raw_sms_data <- read_dta("data/raw_data/sms_anonymous.dta")

#### TODO: DELETE IT ####
raw_data <- read_dta("data/raw_data/politics_anonymous.dta")
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")
raw_midline <- read_dta("data/raw_data/midline_anonymous.dta")
raw_endline <- read_dta("data/raw_data/endline_anonymous.dta")
raw_postendline <- read_dta("data/raw_data/postendline_anonymous.dta")

