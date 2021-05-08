library(pao)
# Site de recharge
# recharge_arriere <- st_read('analysis/data/PTO/Recharge_shp/arriere_recharge_construction.shp', quiet = TRUE) %>% st_zm()
# recharge_bas <- st_read('analysis/data/PTO/Recharge_shp/bas_recharge_construction.shp', quiet = TRUE) %>% st_zm()
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
st_write(bathymetrie, './data/bathymetrie.geojson', delete_dsn = TRUE)

# Centroide du site de recharge le long de la ligne de 0 bathymétrie
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

# Recharge ouest
rw <- bind_rows(buf[1,], xyz) %>%
      st_union() %>%
      st_cast("LINESTRING") %>%
      st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
      st_as_sf(data.frame(nom = 'Recharge ouest'))

# Recharge est
re <- bind_rows(buf[2,], xyz) %>%
      st_union() %>%
      st_cast("LINESTRING") %>%
      st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
      st_as_sf(data.frame(nom = 'Recharge est'))

# Sous-influence est
sfe <- buf[2, ] %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,]

sfe <- sfe %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(sfe) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence est'))


# Référence est
refe <- buf[2, ] %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,]

refe <- refe %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(refe) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence est'))


# Sous-influence ouest
xy <- data.frame(x = -68.42132, y = 49.04103)
quai <- st_as_sf(xy, coords = c('x','y'), crs = 4326) %>%
        st_transform(st_crs(buf))

sfo <- quai %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,]

sfo <- sfo %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(sfo) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence ouest'))

# Référence ouest
refo <- quai %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,]

refo <- refo %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(refo) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence ouest'))

#
zones <- rbind(rw, re, sfe, refe, sfo, refo)
mapview(zones)
# Export
st_write(zones, './data/zones.geojson', delete_dsn = TRUE)
