library(pao)
# Site de recharge
recharge_arriere <- st_read('analysis/data/PTO/Recharge_shp/arriere_recharge_construction.shp') %>% st_zm()
recharge_bas <- st_read('analysis/data/PTO/Recharge_shp/bas_recharge_construction.shp') %>% st_zm()
recharge_crete <- st_read('analysis/data/PTO/Recharge_shp/crete_recharge_construction.shp') %>% st_zm()

# Dynamique hydro
dynamique_hydro <- st_read("analysis/data/dynamique_hydrosedimentaire_shp/dynamique_hydrosed.shp")

# Bathy
bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m_HS.tif")
bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m.tif")

x <- mape


# Mapview
mapview(recharge_arriere) +
  mapview(recharge_bas) +
  mapview(recharge_crete) +
  mapview(dynamique_hydro)











# source('./maps/maps.R')
library(ceanav)
library(leaflet)
library(leafem)

# Leaflet function
leafBase <- function() {
  # Leaflet map
  leaflet() %>%
  setView(lng = -71.22221599662004, lat = 46.842749767970346, zoom = 7) %>%
  addProviderTiles('CartoDB.Positron', group = 'CartoDB.Positron') %>%
  addProviderTiles('CartoDB.DarkMatter', group = 'CartoDB.DarkMatter') %>%
  addLayersControl(baseGroups = c('CartoDB.Positron','CartoDB.DarkMatter'), position = 'topleft')
}

add <- function(map, data, color, zcol = NULL, group = NULL) {
  addFeatures(map = map, data = data, color = color, zcol = zcol, group = group, fillColor = color,
              weight = 1, smoothFactor = .5, opacity = 1, fillOpacity = .9)
}
