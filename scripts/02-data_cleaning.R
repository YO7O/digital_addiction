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


# Extract state information from baseline, midline and endline
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")
raw_midline <- read_dta("data/raw_data/midline_anonymous.dta")
raw_endline <- read_dta("data/raw_data/endline_anonymous.dta")

# Clean baseline
# filter out response that either decide to not continue, or invalid
cleaned_baseline <- raw_baseline |>
  clean_names() |>
  filter(consent1 == "Continue" & fb_minutes < 1440) |>
  rename(fb_minutes_base = fb_minutes,
         swb_happiness_base = swb_happiness,
         swb_relhappiness_base = swb_relhappiness,
         swb_ideal_base = swb_swl1,
         swb_conditions_base = swb_swl2,
         swb_satisfied_base = swb_swl3,
         swb_lack_companion_base = swb_lnlns1,
         swb_left_out_base = swb_lnlns2,
         swb_isolated_base = swb_lnlns3,
         swb_bored_base = swb_eurhappsvy_4,
         swb_anxious_base = swb_eurhappsvy_5,
         swb_depressed_base = swb_eurhappsvy_6,
         swb_absorbed_worthwhile_base = swb_eurhappsvy_7)

# Clean midline
cleaned_midline <- raw_midline |>
  clean_names() |>
  rename(price1 = first_4weeks_num,
         price2 = second_4weeks_num1) |>
  # Made it to randomization stage in midline
  mutate(randomized = ifelse(wta_understanding1 == "", 0, 1),
         # treatment = 1 if price is offered, 0 if not
         treatment = ifelse(price1 == 102 & wta1 != "" & randomized == 1,
                            1,
                            ifelse(price1 == 0 & wta1 != "" & randomized == 1,
                                   0,
                                   NA)))

# Clean endline
cleaned_endline <- raw_endline |>
  clean_names() |>
  rename(fb_minutes_end = fb_minutes,
         swb_happiness_end = swb_happiness,
         swb_relhappiness_end = swb_relhappiness,
         swb_ideal_end = swb_swl1,
         swb_conditions_end = swb_swl2,
         swb_satisfied_end = swb_swl3,
         swb_lack_companion_end = swb_lnlns1,
         swb_left_out_end = swb_lnlns2,
         swb_isolated_end = swb_lnlns3,
         swb_bored_end = swb_eurhappsvy_4,
         swb_anxious_end = swb_eurhappsvy_5,
         swb_depressed_end = swb_eurhappsvy_6,
         swb_absorbed_worthwhile_end = swb_eurhappsvy_7)


# Select variables that are useful for measuring social media usage
social_time <- cleaned_baseline |>
  select(id, fb_minutes_base) |>
  # Remove non valid entries
  filter(fb_minutes_base != "")|>
  mutate(fb_minutes_base = as.numeric(fb_minutes_base))

# Select variables that are useful for measuring well-being
# Variables that are useful in baseline
subjective_well_being <- cleaned_baseline |>
  select(id,
         swb_happiness_base,
         swb_relhappiness_base,
         swb_ideal_base,
         swb_conditions_base,
         swb_satisfied_base,
         swb_lack_companion_base,
         swb_left_out_base,
         swb_isolated_base,
         swb_bored_base,
         swb_anxious_base,
         swb_depressed_base,
         swb_absorbed_worthwhile_base)

# Variables that are useful in endline
data <- cleaned_endline |>
  select(id,
         swb_happiness_end,
         swb_relhappiness_end,
         swb_ideal_end,
         swb_conditions_end,
         swb_satisfied_end,
         swb_lack_companion_end,
         swb_left_out_end,
         swb_isolated_end,
         swb_bored_end,
         swb_anxious_end,
         swb_depressed_end,
         swb_absorbed_worthwhile_end)

subjective_well_being <- merge(subjective_well_being,
                               data,
                               by = "id",
                               all.x = TRUE)

# Distinguish treatment and control
data <- cleaned_midline |>
  select(id, treatment)

subjective_well_being <- merge(subjective_well_being,
                               data,
                               by = "id",
                               all.x = TRUE)

subjective_well_being <- subjective_well_being |>
  filter(!is.na(treatment),
         !is.na(swb_absorbed_worthwhile_end))


#### Save data ####
write_csv(social_time, "data/analysis_data/social_time.csv")
write_csv(subjective_well_being, "data/analysis_data/subjective_well_being.csv")

#### TODO: DELETE IT ####
raw_data <- read_dta("data/raw_data/politics_anonymous.dta")
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")
raw_midline <- read_dta("data/raw_data/midline_anonymous.dta")
raw_endline <- read_dta("data/raw_data/endline_anonymous.dta")
raw_postendline <- read_dta("data/raw_data/postendline_anonymous.dta")

