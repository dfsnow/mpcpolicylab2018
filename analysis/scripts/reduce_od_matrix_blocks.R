library(tidyverse)
library(udunits2)
library(sf)

##### Fixed Line (count, freq, coverage, dist) #####
# Reading in the raw distance matrix for walking
odm_walking <- read_csv("analysis/data/17031_output_blocks_walking.csv") %>% 
  mutate(walk_dist = ud.convert(walk_dist, "m", "mi"),
         origin = str_pad(origin, 15, "left", "0")
         )

# Loading all GTFS feed stop data
gtfs <- bind_rows(
  read_csv("analysis/data/cta_gtfs_stop_summary.csv") %>% mutate(stop_id = as.character(stop_id)),
  read_csv("analysis/data/pace_gtfs_stop_summary.csv"),
  read_csv("analysis/data/metra_gtfs_stop_summary.csv")
)

# Merging data about each stop to the OD matrix by destination
odm_merged <- odm_walking %>%
  filter(walk_dist <= 0.75) %>%
  left_join(gtfs, by = c("destination" = "stop_id")) %>%
  mutate(accessible = wheelchair_boarding != 2)

# Aggregating stop information by each origin (block)
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

rm(list = c("gtfs", "odm_walking"))
  


##### Fixed Line Transit Scores (connectivity) #####
# Reading in the matrix for transit + walking
odm_transit <- read_csv("analysis/data/17031_output_blocks_transit.csv")

# Get count of all stops within 30 min and rescale
odm_block_scores <- odm_transit %>%
  filter(agg_cost <= 60) %>%
  group_by(origin) %>%
  summarize(connectivity = n()) %>%
  mutate(connectivity = scales::rescale(connectivity, to = c(0, 1))) %>%
  mutate(origin = as.character(origin))

# Merge block population and connectivity scores
odm_block_agg <- odm_block_agg %>%
  full_join(
    read_csv("analysis/data/mpc_block_data.csv") %>%
      rename(origin = geoid) %>%
      mutate(origin = str_pad(origin, 15, "left", "0")
      ),
    by = "origin"
  ) %>%
  left_join(odm_block_scores, by = "origin") %>%
  mutate_all(funs(replace_na(., 0))) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

rm(list = c("odm_transit"))



##### Paratransit Coverage #####
# Load paratransit coverage map (cta and pace)
paratransit <- st_read("analysis/shapefiles/fixed_lines_buffered_merged.shp") %>%
    st_set_crs(4326)
  
# Create indicator if a block is in paratransit coverage area
odm_block_agg <- odm_block_agg %>%
  mutate(p_elig = st_within(odm_block_agg, paratransit) %>% lengths > 0)

#odm_block_agg %>% write_csv("analysis/data/block_agg_temp.csv")
#odm_block_agg <- read_csv("analysis/data/block_agg_temp.csv") %>%
#mutate(geometry = st_geometry(geometry))



##### Dial-a-ride Coverage #####
# Load dial-a-ride service data
dar_data <- read_csv("analysis/data/dar_service_data.csv") %>%
  filter(!is.na(objectid))

# Load dial-a-ride shapefile
dar_shapes <- st_read("analysis/shapefiles/GIS.DIAL_A_RIDE_2018.shp") %>%
  setNames(tolower(colnames(.))) %>%
  select(objectid, objectid_1)

# Merge shapes and service data, keep only relevant data and prefix with d_
dar_merged <- left_join(dar_shapes, dar_data, by = c("objectid", "objectid_1")) %>%
  select(total_service_hours_pct_coverage, eligibility, advance_flexibility, geometry) %>%
  setNames(c(paste0("d_", colnames(.)[-length(colnames(.))]), "geometry")) %>%
  st_transform(crs = 4326)

rm(list = c("dar_data", "dar_shapes"))

# Perform a spatial join of DaR data to all blocks, then drop geometry
odm_block_agg_dar <- odm_block_agg %>%
  st_join(dar_merged, join = st_within)
odm_block_agg_dar$geometry <- NULL

# Get the count + mean eligibility, advanced call in, and pct_coverage per block
odm_block_agg <- odm_block_agg_dar %>%
  group_by(origin) %>%
  summarize(d_count = n()) %>%
  left_join(odm_block_agg_dar, by = "origin") %>%
  mutate(tract_id = as.numeric(tract_id)) %>%
  group_by(origin) %>%
  summarize_all(funs(mean(.))) %>%
  mutate(tract_id = str_pad(tract_id, 11, "left", "0")) %>%
  mutate_at(vars(starts_with("d_")), funs(scales::rescale(., to = c(0, 1))))

#rm(list = c("odm_block_agg_dar"))
  

##### Tract Aggregation #####
odm_tract_agg <- odm_block_agg %>%
  group_by(tract_id) %>%
  summarize_at(
    vars(-block_pop, -tract_pop, -origin),
    funs(weighted.mean(., block_pop)) 
  ) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  rename_at(
    vars(count, walk_dist, stop_freq, stop_coverage, connectivity),
    funs(paste0('f_', .))
  ) %>%
  mutate(
    f_index = f_count + f_walk_dist + f_stop_freq + f_stop_coverage + f_connectivity,
    p_index = p_elig + d_count + d_eligibility + d_advance_flexibility + d_total_service_hours_pct_coverage,
    all_index = f_index + p_index
    ) %>%
  filter(!is.nan(all_index)) %>%
  rename(origin = tract_id) %>%
  write_csv("analysis/data/17031_output_blocks_index.csv")

