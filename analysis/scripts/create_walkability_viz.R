library(tidyverse)
library(udunits2)
library(sf)

# Loading all GTFS feed stop data
gtfs <- bind_rows(
  read_csv("analysis/data/cta_gtfs_stop_summary.csv") %>%
    mutate(stop_id = as.character(stop_id)),
  read_csv("analysis/data/pace_gtfs_stop_summary.csv"),
  read_csv("analysis/data/metra_gtfs_stop_summary.csv")
) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), remove = FALSE, crs = 4326)

# Load the CMAP walkability index, recode, then normalize
walkability <- st_read("analysis/shapefiles/Walkability.shp") %>%
  st_transform(crs = 4326) %>%
  select(Walkabilit, geometry) %>%
  mutate(
    Walkabilit = factor(
      Walkabilit,
      levels = c("Very low", "Low", "Moderate", "High", "Very high")),
    walk_numeric = recode(
      Walkabilit,
      "Very high" = 5,
      "High"      = 4,
      "Moderate"  = 3,
      "Low"       = 2,
      "Very low"  = 1
      )
    )

# Spatially merge stop with their walkability scores
gtfs <- gtfs %>%
  st_join(walkability, join = st_within) %>%
  mutate(walk_numeric = replace_na(walk_numeric, 1.0))

mpc_palette <- "Set1"

mpc_labels <- sprintf(
  paste0(
    "<strong>%s: %s</strong><br/>",
    "Walkability: %s<br/>",
    "Frequency: %g<br/>",
    "Service Span: %g<br/>"),
  gtfs$provider,
  gtfs$stop_name,
  gtfs$Walkabilit,
  round(gtfs$stop_week_frequency, 3),
  round(gtfs$stop_week_pct_coverage, 3)
  ) %>%
  lapply(htmltools::HTML)

mpc_labeloptions <- labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto")

mpc_map_supply_pal_all <- colorFactor(
  palette = mpc_palette,
  domain = gtfs$Walkabilit)

walk_1 <- gtfs %>%
  filter(walk_numeric == 1) %>%
  sample_frac(0.5)

walk_2 <- gtfs %>%
  filter(walk_numeric == 2) %>%
  sample_frac(0.5)

walk_3 <- gtfs %>%
  filter(walk_numeric == 3) %>%
  sample_frac(0.5)

walk_4 <- gtfs %>%
  filter(walk_numeric == 4) %>%
  sample_frac(0.5)

walk_5 <- gtfs %>%
  filter(walk_numeric == 5) 

# Create leaflet maps
mpc_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = walk_1$stop_lon,
    lat = walk_1$stop_lat,
    color = mpc_map_supply_pal_all(walk_1$Walkabilit),
    weight = 1.3,
    fillOpacity = 0.9,
    group = "Very Low"
  ) %>%
  addCircles(
    lng = walk_2$stop_lon,
    lat = walk_2$stop_lat,
    color = mpc_map_supply_pal_all(walk_2$Walkabilit),
    weight = 1.3,
    fillOpacity = 0.9,
    group = "Low"
  ) %>%
  addCircles(
    lng = walk_3$stop_lon,
    lat = walk_3$stop_lat,
    color = mpc_map_supply_pal_all(walk_3$Walkabilit),
    weight = 1.3,
    fillOpacity = 0.9,
    group = "Moderate"
  ) %>%
  addCircles(
    lng = walk_4$stop_lon,
    lat = walk_4$stop_lat,
    color = mpc_map_supply_pal_all(walk_4$Walkabilit),
    weight = 1.3,
    fillOpacity = 0.9,
    group = "High"
  ) %>%
  addCircles(
    lng = walk_5$stop_lon,
    lat = walk_5$stop_lat,
    color = mpc_map_supply_pal_all(walk_5$Walkabilit),
    weight = 1.3,
    fillOpacity = 0.9,
    group = "Very High"
  ) %>%
  addLegend(
    title = "Walkability Score",
    pal = mpc_map_supply_pal_all,
    values = gtfs$Walkabilit,
    position = "topright"
  ) %>%
  addLayersControl(
    overlayGroups = c("Very Low", "Low", "Moderate", "High", "Very High"),
    position = "bottomright"
  ) %>%
  saveWidget("mpc_map_walkability_20181205.html")

