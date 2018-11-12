library(lubridate)
library(tidyverse)
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

# Make a temp dataframe of trips, stops, and routes together
gtfs_cta_temp <- gtfs_cta$stops_df %>% 
  left_join(gtfs_cta$stop_times_df, by = "stop_id") %>%
  left_join(gtfs_cta$trips_df, by = "trip_id") %>%
  left_join(gtfs_cta$calendar_df, by = "service_id") %>%
  mutate(days_per_week = rowSums(select(., monday:sunday))) %>%
  mutate(
    time = as.numeric(hms(arrival_time)),
    start_date = ymd(start_date),
    end_date = ymd(end_date)
  ) %>%
  mutate(service_interval = start_date %--% end_date) %>%
  filter((service_interval > days(14) & now() %within% service_interval) & pickup_type == 0) %>%
  select(-service_interval, -start_date, -end_date) 

# Create a summary dataframe of each stop with pct coverage and frequency
stop_summary_cta <- gtfs_cta_temp %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, route_id, add = TRUE) %>%
  summarize(
    seconds_per_day = max(time) - min(time)
  ) %>%
  ungroup() %>%
  right_join(gtfs_cta_temp) %>%
  mutate(
    seconds_per_week = seconds_per_day * days_per_week,
    all_week_pct_coverage = seconds_per_week / (7 * 24 * 60 * 60)  # number of minutes in a week
  ) %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, route_id, add = TRUE) %>%
  summarize(
    all_week_frequency = n() * mean(days_per_week),
    all_week_pct_coverage = mean(all_week_pct_coverage)
  ) %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, add = TRUE) %>%
  summarize(
    stop_week_frequency = sum(all_week_frequency),
    stop_week_pct_coverage = mean(all_week_pct_coverage) 
  ) %>%
  group_by(stop_id) %>%
  summarize(
    stop_week_frequency = sum(stop_week_frequency),
    stop_week_pct_coverage = sum(stop_week_pct_coverage) 
  ) %>%
  right_join(gtfs_cta$stops_df, by = "stop_id") %>%
  select(starts_with("stop"), contains("wheelchair"), -stop_desc, -stop_code) %>%
  mutate(
    stop_week_pct_coverage = ifelse(
      stop_week_pct_coverage > 1.0, 1.0, stop_week_pct_coverage
    ),
    stop_week_pct_coverage = replace_na(stop_week_pct_coverage, 0),
    stop_week_frequency = replace_na(stop_week_frequency, 0),
    stop_week_frequency = as.integer(stop_week_frequency),
    provider = "CTA"
  ) %>%
  write_csv("analysis/data/cta_gtfs_stop_summary.csv")


##### PACE #####
# Download the PACE GTFS feed
if (!file.exists("gtfs/gtfs_pace.zip")) {
  url_pace <- "http://www.pacebus.com/gtfs/gtfs.zip"
  download.file(url_pace, "gtfs/gtfs_pace.zip")
}

# Import the PACE GTFS feed into a dataframe
gtfs_pace <- import_gtfs("gtfs/gtfs_pace.zip", local = TRUE)

# Make a temp dataframe of trips, stops, and routes together
gtfs_pace_temp <- gtfs_pace$stops_df %>% 
  left_join(gtfs_pace$stop_times_df, by = "stop_id") %>%
  left_join(gtfs_pace$trips_df, by = "trip_id") %>%
  left_join(gtfs_pace$calendar_df, by = "service_id") %>%
  mutate(days_per_week = rowSums(select(., monday:sunday))) %>%
  mutate(
    time = as.numeric(hms(arrival_time)),
    start_date = ymd(start_date),
    end_date = ymd(end_date)
  ) %>%
  mutate(service_interval = start_date %--% end_date) %>%
  filter((service_interval > days(14) & now() %within% service_interval) & pickup_type == 0) %>%
  select(-service_interval, -start_date, -end_date)

# Create a summary dataframe of each stop with pct coverage and frequency
stop_summary_pace <- gtfs_pace_temp %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, route_id, add = TRUE) %>%
  summarize(
    seconds_per_day = max(time) - min(time)
    ) %>%
  ungroup() %>%
  right_join(gtfs_pace_temp) %>%
  mutate(
    seconds_per_week = seconds_per_day * days_per_week,
    all_week_pct_coverage = seconds_per_week / (7 * 24 * 60 * 60)  # number of minutes in a week
    ) %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, route_id, add = TRUE) %>%
  summarize(
    all_week_frequency = n() * mean(days_per_week),
    all_week_pct_coverage = mean(all_week_pct_coverage)
  ) %>%
  group_by_at(vars(monday:sunday)) %>%
  group_by(stop_id, add = TRUE) %>%
  summarize(
    stop_week_frequency = sum(all_week_frequency),
    stop_week_pct_coverage = mean(all_week_pct_coverage) 
  ) %>%
  group_by(stop_id) %>%
  summarize(
    stop_week_frequency = sum(stop_week_frequency),
    stop_week_pct_coverage = sum(stop_week_pct_coverage) 
  ) %>%
  right_join(gtfs_pace$stops_df, by = "stop_id") %>%
  select(starts_with("stop"), -stop_desc) %>%
  mutate(
    stop_week_pct_coverage = ifelse(
      stop_week_pct_coverage > 1.0, 1.0, stop_week_pct_coverage
      ),
    stop_week_pct_coverage = replace_na(stop_week_pct_coverage, 0),
    stop_week_frequency = replace_na(stop_week_frequency, 0),
    stop_week_frequency = as.integer(stop_week_frequency),
    provider = "PACE"
  ) %>% 
  write_csv("analysis/data/pace_gtfs_stop_summary.csv")

  


