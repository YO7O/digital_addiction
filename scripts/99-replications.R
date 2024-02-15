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

opinion_data <- 
  read_csv("data/analysis_data/opinion_data.csv",
           show_col_types = FALSE)

political_data <-
  read_csv("data/analysis_data/political_data.csv",
           show_col_types = FALSE)

# Combine all data
all_data <- merge(political_data,
            merge(opinion_data,
            merge(treatment_data,
            merge(social_time_data,
                  subjective_well_being_data)))) |>
  # filter out non valid response
  filter(!is.na(treatment)) |>
  mutate(type = ifelse(treatment, "Treatment", "Control")) |>
  select(-treatment)




###########################################
### Replicating Figure A4 from appendix ###
###########################################
count_participant <- count(all_data)
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
  fig8a, fig8b, nrow = 2, common.legend = TRUE, legend = "bottom"
)


##########################################
### Replicate bottom right of Figure 8 ###
##########################################

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
      "Bored(-1)",
      "Anxious(-1)",
      "Depressed(-1)",
      "Absorbed in doing something worthwhile",
      "Feeling index"
      ),
    mean = c(mean(fb_minutes_diff)
             / sd(fb_minutes_base),
             mean(swb_happiness_diff)
             / sd(swb_happiness_base),
             mean(swb_relhappiness_diff)
             / sd(swb_relhappiness_base),
             mean(swb_ideal_diff)
             / sd(swb_ideal_base),
             mean(swb_conditions_diff)
             / sd(swb_conditions_base),
             mean(swb_satisfied_diff)
             / sd(swb_satisfied_base),
             mean(swb_wellbeing_index_diff)
             / sd(swb_wellbeing_index_base),
             mean(swb_lack_companion_diff)
             / sd(swb_lack_companion_base),
             mean(swb_left_out_diff)
             / sd(swb_left_out_base),
             mean(swb_isolated_diff)
             / sd(swb_isolated_base),
             mean(swb_social_index_diff)
             / sd(swb_social_index_base),
             - mean(swb_bored_diff)
             / sd(swb_bored_base),
             - mean(swb_anxious_diff)
             / sd(swb_anxious_base),
             - mean(swb_depressed_diff)
             / sd(swb_depressed_base),
             mean(swb_absorbed_worthwhile_diff)
             / sd(swb_absorbed_worthwhile_base),
             - mean(swb_feeling_index_diff)
             / sd(swb_feeling_index_base)
            ),
    
    sd = c(sd(fb_minutes_diff)
           / sqrt(length(id))
           / sd(fb_minutes_base),
           sd(swb_happiness_diff)
           / sqrt(length(id))
           / sd(swb_happiness_base),
           sd(swb_relhappiness_diff)
           / sqrt(length(id))
           / sd(swb_relhappiness_base),
           sd(swb_ideal_diff)
           / sqrt(length(id))
           / sd(swb_ideal_base),
           sd(swb_conditions_diff)
           / sqrt(length(id))
           / sd(swb_conditions_base),
           sd(swb_satisfied_diff)
           / sqrt(length(id))
           / sd(swb_satisfied_base),
           sd(swb_wellbeing_index_diff)
           / sqrt(length(id))
           / sd(swb_wellbeing_index_base),
           sd(swb_lack_companion_diff)
           / sqrt(length(id))
           / sd(swb_lack_companion_base),
           sd(swb_left_out_diff)
           / sqrt(length(id))
           / sd(swb_left_out_base),
           sd(swb_isolated_diff)
           / sqrt(length(id))
           / sd(swb_isolated_base),
           sd(swb_social_index_diff)
           / sqrt(length(id))
           / sd(swb_social_index_base),
           sd(swb_bored_diff)
           / sqrt(length(id))
           / sd(swb_bored_base),
           sd(swb_anxious_diff)
           / sqrt(length(id))
           / sd(swb_anxious_base),
           sd(swb_depressed_diff)
           / sqrt(length(id))
           / sd(swb_depressed_base),
           sd(swb_absorbed_worthwhile_diff)
           / sqrt(length(id))
           / sd(swb_absorbed_worthwhile_base),
           sd(swb_feeling_index_diff)
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
           "Bored(-1)",
           "Anxious(-1)",
           "Depressed(-1)",
           "Absorbed in doing something worthwhile",
           "Feeling index"
           ))))

graph_data |>
  ggplot(aes(x = name, y = mean, color = type)) +
  geom_point(position = position_dodge(width = -0.2)) +
  geom_errorbar(aes(ymin = mean - abs(1.96 * sd), ymax = mean + abs(1.96 * sd)),
                position = position_dodge(width = -0.2),
                width = 0.2) + 
  geom_hline(yintercept = 0) +
  theme_minimal() +
  coord_flip(expand = TRUE) +
  labs(y = "Difference from baseline (standard deviation)",
     x = "Subjective wellness being",
     color = "Type")

##########################
### Replicate Figure 3 ###
##########################
count_participant <- count(all_data)

# Prepare the data for graph uses
graph_data <- all_data |>
  arrange(fb_soclife_base) |>
  group_by(fb_soclife_base, type) |>
  # Calculate the fraction of each interval
  count(fb_soclife_base) |>
  mutate(fraction = n / count_participant)

fig3a <- graph_data |>
  ggplot(aes(x = fb_soclife_base, y = fraction$n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  theme_minimal() +
  labs(title = "Baseline",
       x = "Facebook usage make social life worse(left)
       or better (right)",
       y = "Fraction of sample",
       fill = "Type")


# end version
graph_data <- all_data |>
  arrange(fb_soclife_end) |>
  group_by(fb_soclife_end, type) |>
  # Calculate the fraction of each interval
  count(fb_soclife_end) |>
  mutate(fraction = n / count_participant)

fig3b <- graph_data |>
  ggplot(aes(x = fb_soclife_end, y = fraction$n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  theme_minimal() +
  labs(title = "Endline",
       x = "Facebook usage make social life worse (left)
       or better (right)",
       y = "Fraction of sample",
       fill = "Type")

ggarrange(
  fig3a, fig3b, nrow = 2, common.legend = TRUE, legend = "bottom"
)

########################
### Political Graphs ###
########################
count_participant <- count(all_data)


### Distribution of republican, democrat and independent ###
graph_data <- all_data |>
  group_by(repdem) |>
  # Calculate the fraction of each interval
  count(repdem) |>
  mutate(fraction = n / count_participant,
         repdem = factor(repdem, c(
           "Democrat (Strongly Democratic)",
           "Democrat (Weakly Democratic)",
           "Independent (Lean toward the Democratic Party)",
           "Independent",
           "Independent (Lean toward the Republican Party)",
           "Republican (Weakly Republican)",
           "Republican (Strongly Republican)"
         )))

graph_data |>
  ggplot(aes(x = repdem, y = fraction$n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  labs(x = "Republican, Democrat or Independent",
       y = "Fraction of sample") +
  coord_flip()

### Distribution of liberal and conservative ###

graph_data <- all_data |>
  group_by(libcon) |>
  # Calculate the fraction of each interval
  count(libcon) |>
  mutate(fraction = n / count_participant,
         libcon = factor(libcon, c(
           "Extremely liberal",
           "Liberal",
           "Slightly liberal",
           "Moderate",
           "Slightly conservative",
           "Conservative",
           "Extremely conservative"
         )))

graph_data |>
  ggplot(aes(x = libcon, y = fraction$n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  labs(x = "Liberal or Conservative",
       y = "Fraction of sample") +
  coord_flip()


# Clean enviroment
rm(list = ls())

