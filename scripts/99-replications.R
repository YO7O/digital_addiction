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
library(ggpubr)

#### Load data ####
social_time_data <- 
  read_csv("data/analysis_data/social_time_data.csv",
                        show_col_types = FALSE)
subjective_well_being_data <- 
  read_csv("data/analysis_data/subjective_well_being_data.csv",
           show_col_types = FALSE)
treatment_data <- 
  read_csv("data/analysis_data/treatment_data.csv",
           show_col_types = FALSE)

# Combine all data
all_data <- merge(treatment_data,
                    merge(social_time_data,
                          subjective_well_being_data)) |>
  # filter out non valid response
  filter(!is.na(treatment)) |>
  mutate(type = ifelse(treatment, "Treatment", "Control")) |>
  select(-treatment)



###########################################
### Replicating Figure A4 from appendix ###
###########################################
count_participant <- count(social_time_data)
# Define breaks for grouping (intervals of 25)
breaks <- seq(0,
              ceiling(800 / 25) * 25,
              by = 25)

# Prepare the data for graph uses
graph_data <- all_data |>
  arrange(fb_minutes_base) |>
  # Group data into intervals
  mutate(fb_minutes_base = cut(fb_minutes_base, breaks = breaks)) |>
  group_by(fb_minutes_base, type) |>
  # Calculate the fraction of each interval
  count(fb_minutes_base) |>
  mutate(fraction = n / count_participant) |>
  filter(!is.na(fb_minutes_base))

fig8a <- graph_data |>
  ggplot(aes(x = fb_minutes_base, y = fraction$n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  scale_x_discrete(label = breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0)) +
  labs(x = "Baseline Facebook usage (minutes)",
       y = "Fraction of sample",
       fill = "Type")

# end version
graph_data <- all_data |>
  arrange(fb_minutes_end) |>
  # Group data into intervals
  mutate(fb_minutes_end = cut(fb_minutes_end, breaks = breaks)) |>
  group_by(fb_minutes_end, type) |>
  # Calculate the fraction of each interval
  count(fb_minutes_end) |>
  mutate(fraction = n / count_participant) |>
  filter(!is.na(fb_minutes_end))

fig8b <- graph_data |>
  ggplot(aes(x = fb_minutes_end, y = fraction$n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  scale_x_discrete(label = breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0)) +
  labs(x = "Endline Facebook usage (minutes)",
       y = "Fraction of sample",
       fill = "Type")

ggarrange(
  fig8a, fig8b, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)


##########################
### Replicate Figure 8 ###
##########################

graph_data <- all_data |>
  group_by(type) |>
  reframe(
    name = c(
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
      "Bored",
      "Anxious",
      "Depressed",
      "Absorbed in doing something worthwhile",
      "Feeling index"
      ),
    mean = c(mean(fb_minutes_end - fb_minutes_base)
             / sd(fb_minutes_base),
             mean(swb_happiness_end - swb_happiness_base)
             / sd(swb_happiness_base),
             mean(swb_relhappiness_end - swb_relhappiness_base)
             / sd(swb_relhappiness_base),
             mean(swb_ideal_end - swb_ideal_base)
             / sd(swb_ideal_base),
             mean(swb_conditions_end - swb_conditions_base)
             / sd(swb_conditions_base),
             mean(swb_satisfied_end - swb_satisfied_base)
             / sd(swb_satisfied_base),
             mean(swb_wellbeing_index_end - swb_wellbeing_index_base)
             / sd(swb_wellbeing_index_base),
             mean(swb_lack_companion_end - swb_lack_companion_base)
             / sd(swb_lack_companion_base),
             mean(swb_left_out_end - swb_left_out_base)
             / sd(swb_left_out_base),
             mean(swb_isolated_end - swb_isolated_base)
             / sd(swb_isolated_base),
             mean(swb_social_index_end - swb_social_index_base)
             / sd(swb_social_index_base),
             mean(swb_bored_end - swb_bored_base)
             / sd(swb_bored_base),
             mean(swb_anxious_end - swb_anxious_base)
             / sd(swb_anxious_base),
             mean(swb_depressed_end - swb_depressed_base)
             / sd(swb_depressed_base),
             mean(swb_absorbed_worthwhile_end - swb_absorbed_worthwhile_base)
             / sd(swb_absorbed_worthwhile_base),
             mean(swb_feeling_index_end - swb_feeling_index_base)
             / sd(swb_feeling_index_base)
            ),
    
    sd = c(sd(fb_minutes_end - fb_minutes_base)
           / sqrt(length(id))
           / sd(fb_minutes_base),
           sd(swb_happiness_end - swb_happiness_base)
           / sqrt(length(id))
           / sd(swb_happiness_base),
           sd(swb_relhappiness_end - swb_relhappiness_base)
           / sqrt(length(id))
           / sd(swb_relhappiness_base),
           sd(swb_ideal_end - swb_ideal_base)
           / sqrt(length(id))
           / sd(swb_ideal_base),
           sd(swb_conditions_end - swb_conditions_base)
           / sqrt(length(id))
           / sd(swb_conditions_base),
           sd(swb_satisfied_end - swb_satisfied_base)
           / sqrt(length(id))
           / sd(swb_satisfied_base),
           sd(swb_wellbeing_index_end - swb_wellbeing_index_base)
           / sqrt(length(id))
           / sd(swb_wellbeing_index_base),
           sd(swb_lack_companion_end - swb_lack_companion_base)
           / sqrt(length(id))
           / sd(swb_lack_companion_base),
           sd(swb_left_out_end - swb_left_out_base)
           / sqrt(length(id))
           / sd(swb_left_out_base),
           sd(swb_isolated_end - swb_isolated_base)
           / sqrt(length(id))
           / sd(swb_isolated_base),
           sd(swb_social_index_end - swb_social_index_base)
           / sqrt(length(id))
           / sd(swb_social_index_base),
           sd(swb_bored_end - swb_bored_base)
           / sqrt(length(id))
           / sd(swb_bored_base),
           sd(swb_anxious_end - swb_anxious_base)
           / sqrt(length(id))
           / sd(swb_anxious_base),
           sd(swb_depressed_end - swb_depressed_base)
           / sqrt(length(id))
           / sd(swb_depressed_base),
           sd(swb_absorbed_worthwhile_end - swb_absorbed_worthwhile_base)
           / sqrt(length(id))
           / sd(swb_absorbed_worthwhile_base),
           sd(swb_feeling_index_end - swb_feeling_index_base)
           / sqrt(length(id))
           / sd(swb_feeling_index_base)
           )
  ) |>
  mutate(name = factor(name, rev(c(
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
           "Bored",
           "Anxious",
           "Depressed",
           "Absorbed in doing something worthwhile",
           "Feeling index"
           ))))

graph_data |>
  ggplot(aes(x = name, y = mean, color = type)) +
  geom_point(position = position_dodge(width = -0.2)) +
  geom_errorbar(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
                position = position_dodge(width = -0.2),
                width = 0.1) + 
  geom_hline(yintercept = 0) +
  theme_minimal() +
  coord_flip(expand = TRUE) +
  labs(y = "Difference from baseline (standard deviation)",
     x = "Subjective wellness being",
     color = "Type")


# Clean enviroment
rm(list = ls())

