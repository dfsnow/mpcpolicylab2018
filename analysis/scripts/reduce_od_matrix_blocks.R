library(tidyverse)
library(udunits2)
library(sf)
library(tidycensus)

##### ODM and GTFS #####
# Reading in the raw distance matrix for walking
odm_walking <- read_csv("analysis/data/17031_output_blocks_walking.csv") %>% 
  mutate(walk_dist = ud.convert(walk_dist, "m", "mi"),
         origin = str_pad(origin, 15, "left", "0")
         )

# Loading both GTFS feeds
gtfs <- bind_rows(
  read_csv("analysis/data/cta_gtfs_stop_summary.csv") %>% mutate(stop_id = as.character(stop_id)),
  read_csv("analysis/data/pace_gtfs_stop_summary.csv")
)

# Merging data about each stop to the OD matrix
odm_merged <- odm_walking %>%
  filter(walk_dist <= 0.75) %>%
  left_join(gtfs, by = c("destination" = "stop_id")) %>%
  mutate(accessible = wheelchair_boarding != 2)

# Aggregating by each origin (block)
odm_block_agg <- odm_merged %>%
  mutate_at(
    vars(agg_cost, walk_dist, stop_week_frequency, stop_week_pct_coverage),
    funs(scales::rescale(., to = c(0, 1)))
    ) %>%
  group_by(origin) %>%
  summarize(
    count = sum(accessible, na.rm = T),
    walk_time = mean(1 - agg_cost * accessible, na.rm = T),
    walk_dist = mean(1 - walk_dist * accessible, na.rm = T),
    stop_freq = mean(stop_week_frequency * accessible, na.rm = T),
    stop_coverage = mean(stop_week_pct_coverage * accessible, na.rm = T)
  ) %>%
  mutate_at(
    vars(walk_time:stop_coverage),
    funs(replace(., is.nan(.), 0))
  ) %>%
  mutate(
    count = scales::rescale(count, to = c(0, 1)),
    tract_id = str_sub(origin, 1, 11)
  )
  

##### ODM Transit Scores (Connectivity) #####
# Reading in the matrix for transit + walking
odm_transit <- read_csv("analysis/data/17031_output_blocks_transit.csv")

# Get all stops within 30 min then sum their distances and rescale
odm_block_scores <- odm_transit %>%
  filter(agg_cost <= 30) %>%
  group_by(origin) %>%
  summarize(connectivity = n()) %>%
  mutate(connectivity = scales::rescale(connectivity, to = c(0, 1)))

# Merge tract pop and connectivity, then aggregate to tract level 
odm_tract_agg <- odm_block_agg %>%
  left_join(
    read_csv("analysis/data/block_populations.csv") %>%
      rename(origin = geoid, block_pop = pop) %>%
      mutate(origin = str_pad(origin, 15, "left", "0")
      ),
    by = "origin"
  ) %>%
  mutate(block_pop = replace_na(block_pop, 0)) %>%
  left_join(odm_block_scores, by = "origin") %>%
  mutate(
    index = count + walk_dist + stop_freq + stop_coverage + connectivity
  ) %>%
  group_by(tract_id) %>%
  summarize(
    weighted_index = weighted.mean(index, block_pop)
  ) %>%
  mutate(
    weighted_index = replace(weighted_index, is.nan(weighted_index), 0)
  ) %>%
  rename(origin = tract_id) %>%
  write_csv("analysis/data/17031_output_index.csv")

