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
il_tracts <- st_read(
  unzip("analysis/shapefiles/gz_2010_17_140_00_500k.zip")) %>%
  setNames(tolower(names(.))) %>%
  filter(county %in% names(il_counties)) %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

wi_tracts <- st_read(
  unzip("analysis/shapefiles/gz_2010_55_140_00_500k.zip")) %>%
  setNames(tolower(names(.))) %>%
  filter(county == "059") %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(-geo_id) %>%
  st_transform(crs = 4326)

in_tracts <- st_read(
  unzip("analysis/shapefiles/gz_2010_18_140_00_500k.zip")) %>%
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

# Load demand files
demand <- read_csv("analysis/data/mpc_demand_1.csv") %>%
  setNames(to_snake_case(colnames(.))) %>%
  select(-geo_id, -x_1) %>%
  rename(geoid = geo_id_2) %>%
  mutate(geoid = str_pad(geoid, 11, "left", "0")) %>%
  mutate(percent_with_a_disability = as.numeric(na_if(percent_with_a_disability, "-")))

# Load supply files
supply <- read_csv("analysis/data/17031_output_blocks_index.csv") %>%
  mutate(geoid = str_pad(origin, 11, "left", "0")) %>%
  select(-origin)

# Merged supply and demand to tract geometry
mpc_merged <- mpc_tracts %>%
  left_join(demand, by = "geoid") %>%
  left_join(supply, by = "geoid")

rm(list = c("demand", "supply", "mpc_tracts"))

### Creating map helpers and map ###
mpc_palette <- "viridis"

mpc_labels <- sprintf(
  "<strong>%s</strong><br/>Supply index: %g<br/>%% pop. 65+: %g<br/># amb. diff. per sqkm: %g<br/>%% pop. w/ disability: %g<br/># disability per sqkm: %g",
  mpc_merged$geoid,
  mpc_merged$index,
  round(mpc_merged$percent_over_65, 2),
  round(mpc_merged$number_with_ambulatory_difficulty_per_sq_kilometer, 2),
  round(mpc_merged$percent_with_a_disability, 2),
  round(mpc_merged$number_with_a_disability_per_sq_kilometer, 2)
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
  n = 10)

# Demand map helpers
mpc_map_demand_amb_pal <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$number_with_ambulatory_difficulty_per_sq_kilometer,
  n = 10)
mpc_map_demand_sen_pal <- colorNumeric(
  palette = mpc_palette,
  domain = mpc_merged$percent_over_65)
mpc_map_demand_dis_dens_pal <- colorQuantile(
  palette = mpc_palette,
  domain = mpc_merged$number_with_a_disability_per_sq_kilometer,
  n = 9)
mpc_map_demand_dis_perc_pal <- colorNumeric(
  palette = mpc_palette,
  domain = mpc_merged$percent_with_a_disability)

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
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_demand_amb_pal(mpc_merged$number_with_ambulatory_difficulty_per_sq_kilometer),
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
    title = "Deciles of<br>Number of People<br> with Ambulatory<br>Care Difficulties<br>Per Square Kilometer",
    pal = mpc_map_demand_amb_pal,
    values = na.omit(mpc_merged$number_with_ambulatory_difficulty_per_sq_kilometer),
    position = "topright",
    group = "Demand (Amb. Care)",
    labFormat = function(type, cuts, p) {
      n = length(cuts)
      paste0(round(cuts[-n]), sep = " to ", round(cuts[-1]))}
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
    title = "Percent of Pop.<br>Over 65",
    pal = mpc_map_demand_sen_pal,
    values = na.omit(mpc_merged$percent_over_65),
    position = "topright",
    group = "Demand (Seniors)"
  ) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_demand_dis_dens_pal(mpc_merged$number_with_a_disability_per_sq_kilometer),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Demand (Disability Density)"
  ) %>%
  addLegend(
    title = "Number with Disability<br>per sq. km",
    pal = mpc_map_demand_sen_pal,
    values = na.omit(mpc_merged$percent_over_65),
    position = "topright",
    group = "Demand (Disability Density)"
  ) %>%
  setView(lng = -87.70, lat = 41.88, zoom = 10) %>%
  addPolygons(
    data = st_geometry(mpc_merged),
    fillColor = mpc_map_demand_dis_perc_pal(mpc_merged$percent_with_a_disability),
    color = "#e2e2e2",
    weight = 0.3,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    label = mpc_labels,
    labelOptions = mpc_labeloptions,
    highlightOptions = mpc_highlights,
    group = "Demand (Disability Percent)"
  ) %>%
  addLegend(
    title = "Percent of Pop.<br>with Disability",
    pal = mpc_map_demand_sen_pal,
    values = na.omit(mpc_merged$percent_with_a_disability),
    position = "topright",
    group = "Demand (Disability Percent)"
  ) %>%
  addLayersControl(
    overlayGroups = c("Supply", "Demand (Amb. Care)", "Demand (Seniors)", "Demand (Disability Density)", "Demand (Disability Percent)"),
    position = "bottomright"
  ) %>%
  hideGroup(c("Demand (Amb. Care)", "Demand (Seniors)", "Demand (Disability Density)", "Demand (Disability Percent)")) 

saveWidget(mpc_map, "mpc_map_20181114.html")