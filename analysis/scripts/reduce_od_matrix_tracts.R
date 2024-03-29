library(tidyverse)
library(udunits2)

# Reading in the raw distance matrix
odm <- read_csv("analysis/data/17031_output_tracts.csv") %>% 
  mutate(walk_dist = ud.convert(walk_dist, "m", "mi")) 

# Aggregating by tract, then getting values with respect to national mean
odm %>%
  group_by(origin) %>%
  summarize(
    count = n(),
    agg_cost = mean(agg_cost),
    walk_dist = mean(walk_dist, na.rm = T)
    ) %>%
  write_csv("analysis/data/17031_output_tracts_unfiltered.csv")

# Same thing, but filtering out any walking longer than 3/4 mile
odm_filtered <- odm %>% filter(walk_dist < 0.75)

odm_filtered_mean_cost <- mean(odm_filtered$agg_cost)
odm_filtered_mean_walk <- mean(odm_filtered$walk_dist, na.rm = T)

odm_filtered %>%
  group_by(origin) %>%
  summarize(
    count = n(),
    agg_cost = mean(agg_cost),
    walk_dist = mean(walk_dist, na.rm = T)
  ) %>%
  write_csv("analysis/data/17031_output_tracts_filtered.csv")


