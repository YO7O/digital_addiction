#### Preamble ####
# Purpose: Replicated graphs from Digital addiction
# Author: Kenneth Chan, 
# Date: 11 February 2023 
# Contact: kenchancf0618@gmail.com
# License: MIT
# Pre-requisites: Digital addiction by Hunt Allcott, Matthew Gentzkow and
#                 Lena Song
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### Load data ####
social_time <- read_csv("data/analysis_data/social_time.csv",
                        show_col_types = FALSE)
subjective_well_being <- 
  read_csv("data/analysis_data/subjective_well_being.csv",
           show_col_types = FALSE)

# Remove extreme cases, make all the social media use time above 800 min to 800.
social_time <- social_time |>
  mutate(sum_social_minutes = 
           ifelse(sum_social_minutes > 800,
                  800,
                  sum_social_minutes),
         fb_minutes =
           ifelse(fb_minutes > 800,
                  800,
                  sum_social_minutes))

###########################################
### Replicating Figure A4 from appendix ###
###########################################
count_participant <- count(social_time)
# Define breaks for grouping (intervals of 25)
breaks <- seq(0,
              ceiling(max(social_time$sum_social_minutes) / 25) * 25,
              by = 25)

# Prepare the data for graph uses
graph_data <- social_time |>
  arrange(sum_social_minutes) |>
  # Group data into intervals
  mutate(sum_social_minutes = cut(sum_social_minutes, breaks = breaks)) |>
  group_by(sum_social_minutes) |>
  # Calculate the fraction of each interval
  count(sum_social_minutes) |>
  mutate(fraction = n / count_participant)

graph_data |>
  ggplot(aes(x = sum_social_minutes, y = fraction$n)) +
  geom_bar(stat = "identity", fill = "darkred", width = 1) +
  scale_x_discrete(label = breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0)) +
  labs(x = "Social media usage (minutes)", y = "Fraction of sample")

# Version with only facebook
# Define breaks for grouping (intervals of 25)
breaks <- seq(0,
              ceiling(max(social_time$sum_social_minutes) / 25) * 25,
              by = 25)

# Prepare the data for graph uses
graph_data <- social_time |>
  arrange(fb_minutes) |>
  # Group data into intervals
  mutate(fb_minutes = cut(fb_minutes, breaks = breaks)) |>
  group_by(fb_minutes) |>
  # Calculate the fraction of each interval
  count(fb_minutes) |>
  mutate(fraction = n / count_participant) |>
  filter(!is.na(fb_minutes))

graph_data |>
  ggplot(mapping = aes(x = fb_minutes, y = fraction$n)) +
  geom_step() +
  scale_x_discrete(label = breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0)) +
  labs(x = "Facebook usage (minutes)", y = "Fraction of sample")


##########################
### Replicate Figure 8 ###
##########################

treatment_data <- subjective_well_being |>
  group_by(treatment) |>
  reframe(
    name = c("Happiness",
             "Relative happiness to peers",
             "Life is ideal",
             "Conditions of life is excellent",
             "Satisfied with life"),
    mean = c(mean(swb_happiness_base) - mean(swb_happiness_end),
             mean(swb_relhappiness_base) - mean(swb_relhappiness_end),
             mean(swb_ideal_base) - mean(swb_ideal_end),
             mean(swb_conditions_base) - mean(swb_conditions_end),
             mean(swb_satisfied_base) - mean(swb_satisfied_end)),
    
    sd = c(sd(swb_happiness_base - swb_happiness_end)
           / sqrt(length(subjective_well_being$id)),
           sd(swb_relhappiness_base - swb_relhappiness_end)
           / sqrt(length(subjective_well_being$id)),
           sd(swb_ideal_base - swb_ideal_end)
           / sqrt(length(subjective_well_being$id)),
           sd(swb_conditions_base - swb_conditions_end)
           / sqrt(length(subjective_well_being$id)),
           sd(swb_satisfied_base - swb_satisfied_end)
           / sqrt(length(subjective_well_being$id)))
  ) |>
  mutate(type = ifelse(treatment, "Treatment", "Control"))



treatment_data |>
  ggplot(aes(x = name, y = mean, color = type)) +
  geom_point(position = position_dodge(width = -0.2)) +
  geom_errorbar(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
                position = position_dodge(width = -0.2),
                width = 0.1) + 
  theme_minimal() +
  ylim(-0.35, 0.35) +
  coord_flip() +
  labs(y = "Difference from baseline (standard deviation)",
     x = "Subjective wellness being",
     color = "Type")
