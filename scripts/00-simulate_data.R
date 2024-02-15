#### Preamble ####
# Purpose: Simulates the analysis data from 01-data_cleaning
# Author: Kenneth Chan, Stephanie Xuan Zhao and Siyu Li
# Date: 15 February 2023 
# Contact: chif.chan@mail.utoronto.ca,
#          xuan.zhao@mail.utoronto.ca,
#          siyul.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: Digital addiction by Hunt Allcott, Matthew Gentzkow and
#                 Lena Song
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####

simulated_facebook_minutes <- 
  tibble(
    id = (1:400),
    fb_minutes = rpois(
      n = 400,
      lambda = 100
    ),
    type = c(
      rep(x = "Control", times = 200),
      rep(x = "Treatment", times = 200)
    )
)

simulated_well_being <-
  tibble(
    id = rep(x = (1:400), times = 16),
    name = rep(x = c(
      "Facebook minutes",
      "Happiness",
      "Relative happiness to peers",
      "Life is ideal",
      "Conditions of life is excellent",
      "Satisfied with life",
      "Well being index",
      "Lack companion",
      "Feeling left out",
      "Feel isolated",
      "Social index",
      "Bored(-1)",
      "Anxious(-1)",
      "Depressed(-1)",
      "Absorbed in doing something worthwhile",
      "Feeling index"
      ),
      times = 400),
    type = c(
      rep(x = "Control", times = 200*16),
      rep(x = "Treatment", times = 200*16)
    ),
    temp_level =
      rpois(
        n = 400*16, lambda = 5
      ), # Draw 6400 times from the poisson distribution
    level = ifelse(temp_level > 5, 5, temp_level) # limit the maximum level
  ) |>
  select(-temp_level)

max(simulated_well_being$level)

# Clean environment
rm(list = ls())


