library(leaflet)
library(tidyverse)
library(sf)
library(snakecase)
library(htmlwidgets)

### Loading Data and Setup ###

normalize <- function(m){
  (m - mean(m, na.rm = T)) / sd(m, na.rm = T)
}

# List of the relevant counties
mpc_counties <- c(
  "031" = "Cook",
  "043" = "DuPage",
  "089" = "Kane",
  "093" = "Kendall",
  "097" = "Lake",
  "111" = "McHenry",
  "197" = "Will"
)

# Load tract data from Census download
il_tracts <- st_read(
  unzip("analysis/shapefiles/gz_2010_17_140_00_500k.zip")) %>%
  setNames(tolower(names(.))) %>%
  filter(county %in% names(mpc_counties)) %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

# Load demand files
demand <- read_csv("analysis/data/mpc_demand_1.csv") %>%
  setNames(to_snake_case(colnames(.))) %>%
  select(-geo_id, -x_1) %>%
  rename(geoid = geo_id_2) %>%
  mutate(geoid = str_pad(geoid, 11, "left", "0"))
  
# Load supply files
supply <- read_csv("analysis/data/17031_output_index.csv") %>%
  mutate(geoid = str_pad(origin, 11, "left", "0")) %>%
  select(-origin)

# Merged supply and demand to tract geometry
mpc_merged <- il_tracts %>%
  left_join(demand, by = "geoid") %>%
  left_join(supply, by = "geoid")

mpc_merged <- mpc_merged %>%
  rename(index = weighted_index)

### Creating map helpers and map ###
mpc_palette <- "viridis"
mpc_labels <- sprintf(
  "<strong>%s</strong><br/># stops < 3/4 mi: %g<br/>%% pop. 65+: %g<br/>%% amb. diff.: %g",
  mpc_merged$geoid,
  mpc_merged$index,
  round(mpc_merged$percent_over_65, 2),
  round(mpc_merged$ambulatory_difficulty_percent, 2)
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
  n = 5)

# Demand map helpers
mpc_map_demand_amb_pal <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$ambulatory_difficulty_percent,
  n = 5)
mpc_map_demand_sen_pal <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$percent_over_65,
  n = 5)

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
    title = "Quintiles of # of<br>Stops (< 0.75 mi.)",
    pal = mpc_map_supply_pal,
    values = mpc_merged$index,
    position = "topright",
    group = "Supply"
  )
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_demand_amb_pal(mpc_merged$ambulatory_difficulty_percent),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Demand (Amb. Care)"
  ) %>%
  addLegend(
    title = "Quintiles of<br>Percent of Pop.<br> with Amubulatory<br>Care Difficulties",
    pal = mpc_map_demand_amb_pal,
    values = na.omit(mpc_merged$ambulatory_difficulty_percent),
    position = "topright",
    group = "Demand (Amb. Care)"
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_demand_sen_pal(mpc_merged$percent_over_65),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Demand (Seniors)"
  ) %>%
  addLegend(
    title = "Quintiles of<br>Percent of Pop.<br>Over 65",
    pal = mpc_map_demand_sen_pal,
    values = na.omit(mpc_merged$percent_over_65),
    position = "topright",
    group = "Demand (Seniors)"
  ) %>%
  setView(lng = -87.70, lat = 41.88, zoom = 10) %>%
  addLayersControl(
    overlayGroups = c("Supply", "Demand (Amb. Care)", "Demand (Seniors)"),
    position = "bottomright"
    ) %>%
  hideGroup("Demand (Amb. Care)") %>%
  hideGroup("Demand (Seniors)")

saveWidget(mpc_map, "mpc_map_20181022.html")
