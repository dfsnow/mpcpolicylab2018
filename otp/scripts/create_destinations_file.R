library(gtfsr)
library(tidyverse)

cta <- import_gtfs("otp/graphs/17031/17031_cta.zip", local = TRUE)
pace <- import_gtfs("otp/graphs/17031/17031_pace.zip", local = TRUE)

cta_cleaned <- cta$stops_df %>%
  select(stop_id, stop_lon, stop_lat) %>%
  rename(
    GEOID = stop_id,
    Y = stop_lon,
    X = stop_lat
    )

pace_cleaned <- pace$stops_df %>%
  select(stop_id, stop_lon, stop_lat) %>%
  rename(
    GEOID = stop_id,
    Y = stop_lon,
    X = stop_lat
  )

merged_stops <- bind_rows(cta_cleaned, pace_cleaned)

merged_stops %>%
  write_csv("otp/locations/17031-destinations.csv")

  
