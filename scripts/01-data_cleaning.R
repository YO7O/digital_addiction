#### Preamble ####
# Purpose: Cleans the raw data recorded by Hunt Allcot, Luca Braghieri, 
#          Sarah Eichmeyer and Matthew Gentzkow
# Author: Kenneth Chan, Stephanie Xuan Zhao and Siyu Li
# Date: 15 February 2023 
# Contact: chif.chan@mail.utoronto.ca,
#          xuan.zhao@mail.utoronto.ca,
#          siyul.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: Digital addiction by Hunt Allcott, Matthew Gentzkow and
#                 Lena Song
# Any other information needed? Data from original replication package

#### Workspace setup ####
library(tidyverse)
library(haven)
library(readxl)
library(janitor)


# Extract state information from baseline, midline and endline
raw_baseline <- read_dta("data/raw_data/baseline_anonymous.dta")
raw_midline <- read_dta("data/raw_data/midline_anonymous.dta")
raw_endline <- read_dta("data/raw_data/endline_anonymous.dta")

# Clean baseline
# filter out response that either decide to not continue, or invalid
cleaned_baseline <- raw_baseline |>
  clean_names() |>
  filter(consent1 == "Continue") |>
  rename(fb_minutes_base = fb_minutes,
         # Subjective wellness being
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
         swb_absorbed_worthwhile_base = swb_eurhappsvy_7,
         # Facebook opinion
         fb_soclife_base = fb_soclife_1)

# Clean midline
cleaned_midline <- raw_midline |>
  clean_names() |>
  rename(price1 = first_4weeks_num,
         price2 = second_4weeks_num1) |>
  # Made it to randomization stage in midline
  mutate(randomized = ifelse(wta_understanding1 == "", FALSE, TRUE),
         # treatment = TRUE if price is offered, FALSE if not
         treatment = ifelse(price1 == 102 & wta1 != "" & randomized,
                            TRUE,
                            ifelse(price1 == 0 & wta1 != "" & randomized,
                                   FALSE,
                                   NA)))

# Clean endline
cleaned_endline <- raw_endline |>
  clean_names() |>
  rename(fb_minutes_end = fb_minutes,
         # Subjective wellness being
         swb_happiness_end = swb_happiness,
         swb_relhappiness_end = swb_relhappiness,
         swb_ideal_end = swb_swl1,
         swb_conditions_end = swb_swl2,
         swb_satisfied_end = swb_swl3,
         swb_lack_companion_end = swb_lnlns1,
         swb_left_out_end = swb_lnlns2,
         swb_isolated_end = swb_lnlns3,
         swb_bored_end = swb_eurhappsvy_7,
         swb_anxious_end = swb_eurhappsvy_4,
         swb_depressed_end = swb_eurhappsvy_5,
         swb_absorbed_worthwhile_end = swb_eurhappsvy_6,
         # Facebook opinion
         fb_soclife_end = fb_soclife_1)

# Clean enviroment
rm(raw_baseline, raw_midline, raw_endline)


# Select variables that are useful for measuring social media usage
# Variables that are useful in baseline
data1 <- cleaned_baseline |>
  select(id, fb_minutes_base)

# Variables that are useful in endline
data2 <- cleaned_endline |>
  select(id, fb_minutes_end)

social_time_data <- merge(data1, data2) |>
  # Remove non valid entries
  filter(fb_minutes_base != "" & fb_minutes_end != "")|>
  mutate(fb_minutes_base = as.numeric(fb_minutes_base),
         fb_minutes_end = as.numeric(fb_minutes_end),
         # Calculate the difference
         fb_minutes_diff = fb_minutes_end - fb_minutes_base,
         )


# Functions for cleaning well-being response
happiness_numeric <- function(happiness) {
  recode (happiness,
          "1 (not a very happy person)" = -1,
          "2" = -2/3,
          "3" = -1/3,
          "4" = 0,
          "5" = 1/3,
          "6" = 2/3,
          "7 (a very happy person)" = 1,
          .default = NaN
  )
}

rel_happiness_numeric <- function(happiness) {
  recode (happiness,
          "1 (less happy)" = -1,
          "2" = -2/3,
          "3" = -1/3,
          "4" = 0,
          "5" = 1/3,
          "6" = 2/3,
          "7 (more happy)" = 1,
          .default = NaN
  )
}

swl_numeric <- function(swl) {
  recode (swl,
          "Strongly disagree" = -1,
          "Disagree" = -2/3,
          "Slightly disagree" = -1/3,
          "Neither agree nor disagree" = 0,
          "Slightly agree" = 1/3,
          "Agree" = 2/3,
          "Strongly agree" = 1,
          .default = NaN
  )
}

lns_numeric <- function(lns) {
  recode (lns,
          "Hardly ever" = -1,
          "Some of the time" = 0,
          "Often" = 1,
          .default = NaN
  )
}

feel_numeric <- function(feel) {
  recode (feel,
          "1. None or almost none of the time" = -1,
          "1. None or almost none of the times" = -1,
          "2" = -1/3,
          "2." = -1/3,
          "3" = 1/3,
          "3." = 1/3,
          "4. All or almost all of the time" = 1,
          "4. All or almost all of the time." = 1,
          .default = NaN
  )
}

# Select variables that are useful for measuring well-being
# Variables that are useful in baseline
data1 <- cleaned_baseline |>
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
         swb_absorbed_worthwhile_base) |>
  mutate(# Translate the response to numeric
    swb_happiness_base = happiness_numeric(swb_happiness_base),
    swb_relhappiness_base = rel_happiness_numeric(swb_relhappiness_base),
    swb_ideal_base = swl_numeric(swb_ideal_base),
    swb_conditions_base = swl_numeric(swb_conditions_base),
    swb_satisfied_base = swl_numeric(swb_satisfied_base),
    swb_wellbeing_index_base = 
      swb_happiness_base +
      swb_relhappiness_base +
      swb_ideal_base +
      swb_conditions_base +
      swb_satisfied_base,
    swb_lack_companion_base = lns_numeric(swb_lack_companion_base),
    swb_left_out_base = lns_numeric(swb_left_out_base),
    swb_isolated_base = lns_numeric(swb_isolated_base),
    swb_social_index_base =
      swb_lack_companion_base +
      swb_left_out_base +
      swb_isolated_base,
    swb_bored_base = feel_numeric(swb_bored_base),
    swb_anxious_base = feel_numeric(swb_anxious_base),
    swb_depressed_base = feel_numeric(swb_depressed_base),
    swb_absorbed_worthwhile_base = feel_numeric(swb_absorbed_worthwhile_base),
    swb_feeling_index_base =
      swb_bored_base +
      swb_anxious_base +
      swb_depressed_base -
      swb_absorbed_worthwhile_base)

# Variables that are useful in endline
data2 <- cleaned_endline |>
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
         swb_absorbed_worthwhile_end) |>
  mutate(# Translate the response to numeric
    swb_happiness_end = happiness_numeric(swb_happiness_end),
    swb_relhappiness_end = rel_happiness_numeric(swb_relhappiness_end),
    swb_ideal_end = swl_numeric(swb_ideal_end),
    swb_conditions_end = swl_numeric(swb_conditions_end),
    swb_satisfied_end = swl_numeric(swb_satisfied_end),
    swb_wellbeing_index_end = 
      swb_happiness_end +
      swb_relhappiness_end +
      swb_ideal_end +
      swb_conditions_end +
      swb_satisfied_end,
    swb_lack_companion_end = lns_numeric(swb_lack_companion_end),
    swb_left_out_end = lns_numeric(swb_left_out_end),
    swb_isolated_end = lns_numeric(swb_isolated_end),
    swb_social_index_end =
      swb_lack_companion_end +
      swb_left_out_end +
      swb_isolated_end,
    swb_bored_end = feel_numeric(swb_bored_end),
    swb_anxious_end = feel_numeric(swb_anxious_end),
    swb_depressed_end = feel_numeric(swb_depressed_end),
    swb_absorbed_worthwhile_end = feel_numeric(swb_absorbed_worthwhile_end),
    swb_feeling_index_end =
      swb_bored_end +
      swb_anxious_end +
      swb_depressed_end -
      swb_absorbed_worthwhile_end)

merged_data <- merge(data1, data2, by = "id", all.x = TRUE)

# Distinguish treatment and control
treatment_data <- cleaned_midline |>
  select(id, treatment)



subjective_well_being_data <- merged_data |>
  mutate(# Calculate the difference
    swb_happiness_diff = swb_happiness_end - swb_happiness_base,
    swb_relhappiness_diff = swb_relhappiness_end - swb_relhappiness_base,
    swb_ideal_diff = swb_ideal_end - swb_ideal_base,
    swb_conditions_diff = swb_conditions_end - swb_conditions_base,
    swb_satisfied_diff = swb_satisfied_end - swb_satisfied_base,
    swb_wellbeing_index_diff = 
      swb_wellbeing_index_end - swb_wellbeing_index_base,
    swb_lack_companion_diff = 
      swb_lack_companion_end - swb_lack_companion_base,
    swb_left_out_diff = swb_left_out_end - swb_left_out_base,
    swb_isolated_diff = swb_isolated_end - swb_isolated_base,
    swb_social_index_diff = swb_social_index_end - swb_social_index_base,
    swb_bored_diff = swb_bored_end - swb_bored_base,
    swb_anxious_diff = swb_anxious_end - swb_anxious_base,
    swb_depressed_diff = swb_depressed_end - swb_depressed_base,
    swb_absorbed_worthwhile_diff = 
      swb_absorbed_worthwhile_end - swb_absorbed_worthwhile_base,
    swb_feeling_index_diff = swb_feeling_index_end - swb_feeling_index_base) |>
    # Filter out non valid responses
    filter(!is.na(swb_wellbeing_index_diff),
           !is.na(swb_social_index_diff),
           !is.na(swb_feeling_index_base)
           ) 

# Functions for cleaning political response
feel_political <- function(feel) {
  recode (feel,
          .default = (as.numeric(feel) - 50) / 50
  )
}

political_pov <- function(pov) {
  recode (pov,
          "Never" = 0,
          "Once" = 1,
          "Two or three times" = 2,
          "Four times or more" = 4,
          .default = as.numeric(pov)
  )
}

# Select variables that are useful for facebook opinions

# Variables that are useful in baseline
data1 <- cleaned_baseline |>
  select(id,
         fb_soclife_base)

# Variables that are useful in endline
data2 <- cleaned_endline |>
  select(id,
         fb_soclife_end)

merged_data <- merge(data1, data2, by = "id", all.x = TRUE)

opinion_data <- merged_data |>
  filter(fb_soclife_base != "" &
           fb_soclife_end != "" &
           !is.na(fb_soclife_end)) |>
  mutate(fb_soclife_base = as.numeric(fb_soclife_base) - 5,
         fb_soclife_end = as.numeric(fb_soclife_end) - 5,
         fb_soclife_diff = fb_soclife_end - fb_soclife_base)


# Select variables that are useful for political opinions
# Variables that are useful in baseline
data1 <- cleaned_baseline |>
  select(id,
         repdem, 
         libcon,
         pol_feeling_1,
         pol_feeling_2,
         pol_feeling_3,
         rep_pov,
         dem_pov
         ) |>
  filter(repdem != "",
         libcon != "",
         pol_feeling_3 != "",
         dem_pov != "") |>
  mutate(# Translate the response to numeric
    feeling_dem_base = feel_political(pol_feeling_1),
    feeling_rep_base = feel_political(pol_feeling_2),
    feeling_trump_base = feel_political(pol_feeling_3),
    rep_pov_base = political_pov(rep_pov),
    dem_pov_base = political_pov(dem_pov),
    ) |>
  select(-pol_feeling_1, -pol_feeling_2, -pol_feeling_3)


# Variables that are useful in endline
data2 <- cleaned_endline |>
  select(id,
         pol_feeling_1,
         pol_feeling_2,
         pol_feeling_3,
         rep_pov,
         dem_pov) |>
  filter(pol_feeling_3 != "",
         dem_pov != "") |>
  mutate(# Translate the response to numeric
    feeling_dem_end = feel_political(pol_feeling_1),
    feeling_rep_end = feel_political(pol_feeling_2),
    feeling_trump_end = feel_political(pol_feeling_3),
    rep_pov_end = political_pov(rep_pov),
    dem_pov_end = political_pov(dem_pov),) |>
  select(-pol_feeling_1, -pol_feeling_2, -pol_feeling_3)

merged_data <- merge(data1, data2, by = "id", all.x = TRUE)

political_data <- merged_data |>
  mutate(# Calculate the difference
    feeling_dem_diff = feeling_dem_end - feeling_dem_base,
    feeling_rep_diff = feeling_rep_end - feeling_rep_base,
    feeling_trump_diff = feeling_trump_end - feeling_trump_base,
    rep_pov_diff = rep_pov_end - rep_pov_base,
    dem_pov_diff = dem_pov_end - dem_pov_base
  ) |>
  filter(!is.nan(rep_pov_diff),
         !is.nan(dem_pov_diff))
  

#### Save data ####
write_csv(social_time_data,
          "data/analysis_data/social_time_data.csv")
write_csv(subjective_well_being_data,
          "data/analysis_data/subjective_well_being_data.csv")
write_csv(treatment_data,
          "data/analysis_data/treatment_data.csv")
write_csv(opinion_data,
          "data/analysis_data/opinion_data.csv")
write_csv(political_data,
          "data/analysis_data/political_data.csv")

# Clean environment
rm(list = ls())

