library(tidyverse)
library(ggplot2)
library(gtfsr)
library(sf)
library(udunits2)

##### CTA #####
# Download the CTA GTFS feed
if (!file.exists("gtfs/gtfs_cta.zip")) {
  url_cta <- "http://www.transitchicago.com/downloads/sch_data/google_transit.zip"
  download.file(url_cta, "gtfs/gtfs_cta.zip")
}

# Unpack the gtfs feed into a separate dataframe
gtfs_cta <- import_gtfs("gtfs/gtfs_cta.zip", local = TRUE)

# Create an sf dataframe containing static linestrings of routes for CTA
lines_df_cta <- gtfs_cta[["routes_df"]] %>%
  select(route_id, route_long_name) %>%
  inner_join(gtfs_cta$trips_df, by = "route_id") %>%
  distinct(route_id, shape_id) %>%
  left_join(gtfs_cta$shapes_df, by = "shape_id") %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(shape_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  left_join(gtfs_cta$trips_df, by = "shape_id") %>%
  group_by(route_id) %>%
  summarize() %>%
  st_transform(2163) %>%
  st_buffer(dist = ud.convert(0.75, "mi", "m")) %>%
  group_by(route_id) %>%
  st_transform(4326) %>%
  left_join(
    gtfs_cta[["routes_df"]] %>%
      select(route_id, route_short_name, route_long_name, route_type),
    by = "route_id")


##### PACE #####
# Download the PACE GTFS feed
if (!file.exists("gtfs/gtfs_pace.zip")) {
  url_pace <- "http://www.pacebus.com/gtfs/gtfs.zip"
  download.file(url_pace, "gtfs/gtfs_pace.zip")
}

# Import the PACE GTFS feed into a dataframe
gtfs_pace <- import_gtfs("gtfs/gtfs_pace.zip", local = TRUE)

# Create a dataframe of PACE routes 
lines_df_pace <- gtfs_pace[["stop_times_df"]] %>%
  select(trip_id, stop_id) %>%
  left_join(
    gtfs_pace$trips_df %>%
      select(trip_id, route_id),
    by = "trip_id") %>%
  left_join(
    gtfs_pace$stops_df %>%
      select(stop_id, stop_name, stop_lat, stop_lon),
    by = "stop_id") %>%
  left_join(
    gtfs_pace$routes_df %>%
      select(route_id, route_short_name, route_long_name),
    by = "route_id") %>%
  distinct(stop_id, route_id, stop_lon, stop_lat, route_short_name) %>%
  filter(route_short_name != "TAXI") %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(2163) %>%
  st_buffer(dist = ud.convert(0.75, "mi", "m")) %>%
  group_by(route_id) %>%
  summarise() %>%
  st_transform(4326) %>%
  left_join(
    gtfs_pace[["routes_df"]] %>%
      select(route_id, route_short_name, route_long_name, route_type),
    by = "route_id")
