#### Preamble ####
# Purpose: Tests... 
# Author: 
# Date: 11 February 2023 
# Contact: 
# License: MIT
# Pre-requisites: 
# Any other information needed? 


#### Workspace setup ####
library(tidyverse)

#### Test data ####
### subjective well being test ###
subjective_well_being_data <- 
  read_csv("data/analysis_data/subjective_well_being_data.csv",
           show_col_types = FALSE)

# Every ideal is in category
subjective_well_being_data |>
  select(swb_ideal_end) |>
  arrange(swb_ideal_end) |>
  distinct() == c(-1, -2/3, -1/3, 0, 1/3, 2/3, 1)

all(subjective_well_being_data$swb_social_index_diff ==
      subjective_well_being_data$swb_social_index_end -
      subjective_well_being_data$swb_social_index_base)

### Treatment data test ###
treatment_data <- read_csv("data/analysis_data/treatment_data.csv",
                           show_col_types = FALSE)

# There are some respondent are not in any group
treatment_data |>
  select(treatment) |>
  arrange(treatment) |>
  distinct() == c(FALSE, TRUE)

### Social time data test ###
social_time_data <- read_csv("data/analysis_data/social_time_data.csv",
                             show_col_types = FALSE)

all(social_time_data$fb_minutes_base >= 0)

all(social_time_data$fb_minutes_end >= 0)

all(social_time_data$fb_minutes_diff ==
      social_time_data$fb_minutes_end -
      social_time_data$fb_minutes_base)
