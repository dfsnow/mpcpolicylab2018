library(tidyverse)
library(udunits2)
library(sf)

##### Fixed Line (count, freq, coverage, dist) #####
# Reading in the raw distance matrix for walking
odm_walking <- read_csv("analysis/data/17031_output_blocks_walking.csv") %>% 
  mutate(
    walk_dist = ud.convert(walk_dist, "m", "mi"),
    origin = str_pad(origin, 15, "left", "0")
    )

# Loading all GTFS feed stop data
gtfs <- bind_rows(
  read_csv("analysis/data/cta_gtfs_stop_summary.csv") %>%
    mutate(stop_id = as.character(stop_id)),
  read_csv("analysis/data/pace_gtfs_stop_summary.csv"),
  read_csv("analysis/data/metra_gtfs_stop_summary.csv")
  ) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Load the CMAP walkability index, recode, then normalize
walkability <- st_read("analysis/shapefiles/Walkability.shp") %>%
  st_transform(crs = 4326) %>%
  select(Walkabilit, geometry) %>%
  mutate(
    walk_numeric = recode(
      Walkabilit,
      "Very high" = 5,
      "High"      = 4,
      "Moderate"  = 3,
      "Low"       = 2,
      "Very low"  = 1
      ),
    walk_numeric = scales::rescale(
      walk_numeric, to = c(0.2, 1))
    ) %>%
  select(-Walkabilit)

# Spatially merge stop with their walkability scores
gtfs <- gtfs %>%
  st_join(walkability, join = st_within) %>%
  mutate(walk_numeric = replace_na(walk_numeric, 1.0)) %>%
  st_set_geometry(NULL)


# Merging data about each stop to the OD matrix by destination
odm_merged <- odm_walking %>%
  filter(walk_dist <= 0.75) %>%
  left_join(gtfs, by = c("destination" = "stop_id")) %>%
  mutate(accessible = replace_na(wheelchair_boarding != 2, TRUE))

# Aggregating stop information by each origin (block)
odm_block_agg1 <- odm_merged %>%
  group_by(origin) %>%
  summarize(
    count_raw = sum(accessible, na.rm = T),
    count = sum(accessible * walk_numeric, na.rm = T),
    walk_time = mean(agg_cost * accessible * walk_numeric, na.rm = T),
    walk_dist = mean(walk_dist * accessible * walk_numeric, na.rm = T),
    stop_freq = mean(stop_week_frequency * accessible * walk_numeric, na.rm = T),
    stop_coverage = mean(stop_week_pct_coverage * accessible * walk_numeric, na.rm = T)
  ) %>%
  mutate_at(
    vars(count, walk_time, walk_dist, stop_freq, stop_coverage),
    funs(scales::rescale(., to = c(0, 1)))
  ) %>%
  mutate_at(
    vars(walk_time:count),
    funs(replace(., is.nan(.), 0))
  ) 

#rm(list = c("gtfs", "odm_walking"))
  


##### Fixed Line Transit Scores (connectivity) #####
# Reading in the matrix for transit + walking
odm_transit <- read_csv("analysis/data/17031_output_blocks_transit.csv") %>%
  mutate(
    walk_dist = ud.convert(walk_dist, "m", "mi"),
    origin = str_pad(origin, 15, "left", "0")
  )

# Get count of all stops within 60 min and rescale
odm_block_scores <- odm_transit %>%
  filter(agg_cost <= 60 & walk_dist <= 0.75) %>%
  left_join(gtfs, by = c("destination" = "stop_id")) %>%
  mutate(accessible = replace_na(wheelchair_boarding != 2, TRUE)) %>%
  group_by(origin) %>%
  summarize(connected = sum(accessible, na.rm = T))

# Merge block population and connectivity scores
odm_block_agg2 <- odm_block_agg1 %>%
  full_join(
    read_csv("analysis/data/mpc_block_data.csv") %>%
      rename(origin = geoid) %>%
      mutate(origin = str_pad(origin, 15, "left", "0")
      ),
    by = "origin"
  ) %>%
  left_join(odm_block_scores, by = "origin") %>%
  mutate_all(funs(replace_na(., 0))) %>%
  mutate(
    connectivity = connected - count_raw,
    connectivity = scales::rescale(connectivity, to = c(0, 1))
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

rm(list = c("odm_transit"))



##### Paratransit Coverage #####
# Load paratransit coverage map (cta and pace)
paratransit <- st_read("analysis/shapefiles/fixed_lines_buffered_merged.shp") %>%
    st_set_crs(4326)
  
# Create indicator if a block is in paratransit coverage area
odm_block_agg3 <- odm_block_agg2 %>%
  mutate(p_elig = st_within(odm_block_agg2, paratransit) %>% lengths > 0)




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
odm_block_agg4 <- odm_block_agg3 %>%
  st_join(dar_merged, join = st_within) %>%
  st_set_geometry(NULL)

# Get the count + mean eligibility, advanced call in, and pct_coverage per block
odm_block_agg5 <- odm_block_agg4 %>%
  group_by(origin) %>%
  summarize(p_count = n()) %>%
  right_join(odm_block_agg4, by = "origin") %>%
  mutate(tract_id = as.numeric(str_sub(origin, 1, 11))) %>%
  group_by(origin) %>%
  summarize_all(funs(mean(.))) %>%
  mutate(tract_id = str_pad(tract_id, 11, "left", "0")) %>%
  mutate_at(vars(starts_with("d_")), funs(scales::rescale(., to = c(0, 1))))


##### Tract Aggregation #####
odm_tract_agg <- odm_block_agg5 %>%
  group_by(tract_id) %>%
  summarize_at(
    vars(-count_raw, -p_count, -connected, -block_pop, -tract_pop, -origin),
    funs(weighted.mean(., block_pop)) 
  ) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  rename_at(
    vars(count, walk_dist, stop_freq, stop_coverage, connectivity),
    funs(paste0('f_', .))
  ) %>%
  mutate(
    f_index = f_count + f_walk_dist + f_stop_freq + f_stop_coverage + f_connectivity,
    p_index = p_elig + d_advance_flexibility + d_total_service_hours_pct_coverage,
    all_index = f_index + p_index
    ) %>%
  filter(!is.nan(all_index)) %>%
  rename(origin = tract_id) %>%
  write_csv("analysis/data/17031_output_blocks_index.csv")

