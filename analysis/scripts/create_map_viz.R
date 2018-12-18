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
mpc_tracts <- st_read("analysis/shapefiles/gz_2010_17_140_00_500k.shp") %>%
  setNames(tolower(names(.))) %>%
  filter(county %in% names(il_counties)) %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

# Load demand files
demand <- read_csv("analysis/data/mpc_demand_3.csv") %>%
  setNames(c("id", "geoid_long", "geoid", "demand_index")) %>%
  select(-id, -geoid_long) %>%
  mutate(
    geoid = str_pad(geoid, 11, "left", "0"),
    demand_index = replace_na(demand_index, 0)
    )

# Load supply files
supply <- read_csv("analysis/data/17031_output_blocks_index.csv") %>%
  mutate(geoid = str_pad(origin, 11, "left", "0")) %>%
  select(-origin) %>%
  rename(supply_index = all_index)

# Merged supply and demand to tract geometry
mpc_merged <- mpc_tracts %>%
  left_join(demand, by = "geoid") %>%
  left_join(supply, by = "geoid") %>%
  mutate_at(
    vars(contains("_index")),
    funs(quantile = ntile(., 10))
    ) %>%
  mutate(
    combined_index_quantile = supply_index_quantile - demand_index_quantile,
    combined_fixed_only = f_index_quantile - demand_index_quantile,
    combined_para_only = p_index_quantile - demand_index_quantile
  )
  

rm(list = c("demand", "supply", "mpc_tracts"))

### Creating map helpers and map ###
mpc_palette <- "RdBu"

mpc_labels <- sprintf(
  paste0(
    "<strong>%s</strong><br/>",
    "<strong>Full Index: %g</strong><br/>",
    "<font color=\"#4c4c4c\"><strong>Supply Index: %g</strong></font><br/>",
    "<font color=\"#4c4c4c\"><strong>Demand Index: %g</strong></font><br/>",
    "<br/>",
    "<font color=\"#4c4c4c\"><strong>Fixed Index: %g</strong></font><br/>",
    "<font color=\"#4c4c4c\"><strong>Para Index: %g</strong></font><br/>"
  ),
  mpc_merged$geoid,
  mpc_merged$combined_index_quantile,
  mpc_merged$supply_index_quantile,
  mpc_merged$demand_index_quantile,
  mpc_merged$combined_fixed_only,
  mpc_merged$combined_para_only
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
mpc_map_supply_pal_all <- colorNumeric(
  palette = mpc_palette,
  domain = -10:10)

# Create leaflet maps
mpc_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_all(mpc_merged$combined_index_quantile),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.85,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Both"
  ) %>%
  addLegend(
    title = "Overall Index",
    pal = mpc_map_supply_pal_all,
    values = mpc_merged$combined_index_quantile,
    position = "topright",
    group = "Both",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_all(mpc_merged$combined_fixed_only),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.85,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Fixed Line"
  ) %>%
  addLegend(
    title = "Fixed Line Index",
    pal = mpc_map_supply_pal_all,
    values = mpc_merged$combined_fixed_only,
    position = "topright",
    group = "Both",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n], 2), sep = " to ", round(cuts[-1], 2))}
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_supply_pal_all(mpc_merged$combined_para_only),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.85,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Paratransit"
  ) %>%
  addLegend(
    title = "Paratransit Index",
    pal = mpc_map_supply_pal_all,
    values = mpc_merged$combined_para_only,
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
  saveWidget("mpc_map_combined_20181218.html")

# Save resulting data
mpc_merged %>%
  st_set_geometry(NULL) %>% 
  select(geoid:combined_index_quantile) %>%
  write_csv("analysis/results/mpc_combined_index.csv")
