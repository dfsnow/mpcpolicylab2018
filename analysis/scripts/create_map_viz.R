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

mpc_merged <- supply %>%
  left_join()

### Creating map helpers and map ###
mpc_palette <- "viridis"

mpc_labels <- sprintf(
  paste0(
    "<strong>%s</strong><br/>",
    "<strong>Fixed Line Index Values</strong>",
    "Index: %g<br/>",
    "Walk Distance: %g<br/>",
    "Stop Frequency: %g<br/>",
    "Stop Coverage: %g<br/>",
    "Connectivity: %g<br/>"),
  mpc_merged$geoid,
  mpc_merged$index,
  round(mpc_merged$f_walk_dist, 2),
  round(mpc_merged$f_stop_freq, 2),
  round(mpc_merged$f_stop_coverage, 2),
  round(mpc_merged$f_connectivity, 2)
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
mpc_map_supply_pal <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$index,
  n = 7)

# Create leaflet maps
mpc_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal(mpc_merged$index),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Supply"
    ) %>%
  addLegend(
    title = "Deciles of <br>Supply Index",
    pal = mpc_map_supply_pal,
    values = mpc_merged$index,
    position = "topright",
    group = "Supply",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) 
  addLayersControl(
    overlayGroups = c("Supply", "Demand (Amb. Care)", "Demand (Seniors)", "Demand (Disability Density)", "Demand (Disability Percent)"),
    position = "bottomright"
  ) %>%
  hideGroup(c("Demand (Amb. Care)", "Demand (Seniors)", "Demand (Disability Density)", "Demand (Disability Percent)")) 

saveWidget(mpc_map, "mpc_map_20181115.html")
