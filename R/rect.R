library(pao)
# Site de recharge
recharge_arriere <- st_read('analysis/data/PTO/Recharge_shp/arriere_recharge_construction.shp', quiet = TRUE) %>% st_zm()
recharge_bas <- st_read('analysis/data/PTO/Recharge_shp/bas_recharge_construction.shp', quiet = TRUE) %>% st_zm()
recharge_crete <- st_read('analysis/data/PTO/Recharge_shp/crete_recharge_construction.shp', quiet = TRUE) %>% st_zm()

# Dynamique hydro
# dynamique_hydro <- st_read("analysis/data/dynamique_hydrosedimentaire_shp/dynamique_hydrosed.shp", quiet = TRUE)

# Bathy
# bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m_HS.tif")
bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m.tif")
# bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(seq(-15,1, by = 1),35))
bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(-15,0,35))
# tmp <- mapedit::editMap()
# tmp <- st_transform(tmp, st_crs(bathymetrie))
# st_write(tmp, 'analysis/data/tmp.geojson', delete_dsn = TRUE)
tmp <- st_read('analysis/data/tmp.geojson', quiet = TRUE)
bathymetrie <- st_intersection(bathymetrie, tmp)
bathymetrie <- st_transform(bathymetrie, st_crs(recharge_crete))

# Centroide du site de recharge le long de la ligne de 0 bathymétrie
xy <- st_centroid(recharge_crete)
xyz <- st_point_on_surface(recharge_crete) %>%
       st_nearest_points(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       st_as_sf()

# Sites autour de la recharge
buf <- st_buffer(xyz, 800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       st_as_sf()

# First rectangle
recharge_west <- bind_rows(buf[1,], xyz) %>%
                 st_union() %>%
                 st_cast("LINESTRING") %>%
                 st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
                 st_as_sf(data.frame(nom = 'Recharge ouest'))

# Second
recharge_east <- bind_rows(buf[2,], xyz) %>%
                 st_union() %>%
                 st_cast("LINESTRING") %>%
                 st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
                 st_as_sf(data.frame(nom = 'Recharge est'))


#
zones <- rbind(recharge_west, recharge_east)

# Export
st_write(zones, './data/zones.geojson')





mv <- mapview(bathymetrie[,1]) +
  # mapview(recharge_arriere, color = '#fba16f') +
  # mapview(recharge_bas, color = '#fba16f') +
  # mapview(recharge_crete, color = '#fba16f') +
  mapview(xyz) +
  mapview(buf)
mv
