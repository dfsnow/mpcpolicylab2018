library(leaflet)
library(tidyverse)
library(sf)
library(snakecase)
library(htmlwidgets)

# List of the relevant counties
il_counties <- c(
  "031" = "Cook",
  "043" = "DuPage",
  "089" = "Kane",
  "093" = "Kendall",
  "097" = "Lake",
  "111" = "McHenry",
  "197" = "Will"
)

# Load tract data for IL, IN, and WI from census
il_tracts <- st_read("analysis/shapefiles/gz_2010_17_140_00_500k.shp") %>%
  setNames(tolower(names(.))) %>%
  filter(county %in% names(il_counties)) %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

wi_tracts <- st_read("analysis/shapefiles/gz_2010_55_140_00_500k.shp") %>%
  setNames(tolower(names(.))) %>%
  filter(county == "059") %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

in_tracts <- st_read("analysis/shapefiles/gz_2010_18_140_00_500k.shp") %>%
  setNames(tolower(names(.))) %>%
  filter(county == "089") %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

# Combine into one dataframe
mpc_tracts <- il_tracts %>%
  bind_rows(in_tracts) %>% 
  bind_rows(wi_tracts) %>%
  mutate(geometry = st_as_sfc(geometry))

rm(list = c("il_tracts", "in_tracts", "wi_tracts"))

# Load supply files
supply <- read_csv("analysis/data/17031_output_blocks_index.csv") %>%
  mutate(geoid = str_pad(origin, 11, "left", "0")) %>%
  select(-origin)

mpc_merged <- mpc_tracts %>%
  left_join(supply, by = "geoid") %>%
  mutate_at(
    vars(contains("index")),
    funs(replace(., . == 0, NA))
  )

### Creating map helpers and map ###
mpc_palette <- "viridis"

mpc_labels <- sprintf(
  paste0(
    "<strong>%s</strong><br/>",
    "<strong>Full Index: %g</strong><br/>",
    "<font color=\"#4c4c4c\"><strong>Fixed Line: %g</strong></font><br/>",
    "Stop Count: %g<br/>",
    "Frequency: %g<br/>",
    "Service Span: %g<br/>",
    "Walk Distance: %g<br/>",
    "Connectivity: %g<br/>",
    "<font color=\"#4c4c4c\"><strong>Paratransit: %g</strong></font><br/>",
    "Paratransit Available: %g<br/>",
    "DaR Service Span: %g<br/>",
    "DaR Adv. Flexibility: %g"),
  mpc_merged$geoid,
  round(mpc_merged$all_index, 3),
  round(mpc_merged$f_index, 3),
  round(mpc_merged$f_count, 3),
  round(mpc_merged$f_stop_freq, 3),
  round(mpc_merged$f_stop_coverage, 3),
  round(mpc_merged$f_walk_dist, 3),
  round(mpc_merged$f_connectivity, 3),
  round(mpc_merged$p_index, 3),
  round(mpc_merged$p_elig, 3),
  round(mpc_merged$d_total_service_hours_pct_coverage, 3),
  round(mpc_merged$d_advance_flexibility, 3)
  ) %>%
  lapply(htmltools::HTML)

mpc_highlights <- highlightOptions(
  weight = 4.5,
  color = "#ffffff",
  fillOpacity = 1,
  bringToFront = TRUE)

mpc_labeloptions <- labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto")

# Supply map helpers
mpc_map_supply_pal_all <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$all_index,
  n = 10)

mpc_map_supply_pal_fixed <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$f_index,
  n = 10)

mpc_map_supply_pal_para <- colorNumeric(
  palette = mpc_palette,
  domain = mpc_merged$p_index)

# Create leaflet maps
mpc_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_all(mpc_merged$all_index),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Both"
  ) %>%
  addLegend(
    title = "Deciles of <br>Overall Supply Index",
    pal = mpc_map_supply_pal_all,
    values = mpc_merged$all_index,
    position = "topright",
    group = "Both",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_fixed(mpc_merged$f_index),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Fixed Line"
  ) %>%
  addLegend(
    title = "Deciles of <br>Fixed Line Index",
    pal = mpc_map_supply_pal_fixed,
    values = mpc_merged$f_index,
    position = "topright",
    group = "Fixed Line",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_para(mpc_merged$p_index),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Paratransit"
  ) %>%
  addLegend(
    title = "Paratransit<br/>Index",
    pal = mpc_map_supply_pal_para,
    values = mpc_merged$p_index,
    position = "topright",
    group = "Paratransit",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) %>%
  addLayersControl(
    overlayGroups = c("Both", "Fixed Line", "Paratransit"),
    position = "bottomright"
  ) %>%
  hideGroup(c("Fixed Line", "Paratransit")) %>% 
  saveWidget("mpc_map_supply_20181218.html")
