library(pao)
# Site de recharge
# recharge_arriere <- st_read('analysis/data/PTO/Recharge_shp/arriere_recharge_construction.shp', quiet = TRUE) %>% st_zm()
# recharge_bas <- st_read('analysis/data/PTO/Recharge_shp/bas_recharge_construction.shp', quiet = TRUE) %>% st_zm()
recharge_crete <- st_read('analysis/data/PTO/Recharge_shp/crete_recharge_construction.shp', quiet = TRUE) %>% st_zm()

# Dynamique hydro
# dynamique_hydro <- st_read("analysis/data/dynamique_hydrosedimentaire_shp/dynamique_hydrosed.shp", quiet = TRUE)

# Bathy
# bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m_HS.tif")
# bathy <- read_stars("analysis/data/PTO/Bathy_SHC_2015/Rasters/Bathy_2015_SHC_5m.tif")
# bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(seq(-15,1, by = 1),35))
# bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(-15,0,35))
# tmp <- mapedit::editMap()
# tmp <- st_transform(tmp, st_crs(bathymetrie))
# st_write(tmp, 'analysis/data/tmp.geojson', delete_dsn = TRUE)
# tmp <- st_read('analysis/data/tmp.geojson', quiet = TRUE)
# bathymetrie <- st_intersection(bathymetrie, tmp)
# bathymetrie <- st_transform(bathymetrie, st_crs(recharge_crete))
# st_write(bathymetrie, './data/bathymetrie.geojson', delete_dsn = TRUE)
bathymetrie <- st_read('./data/bathymetrie.geojson')

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
ReO <- bind_rows(buf[1,], xyz) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Recharge ouest'))


# Recharge est
ReE <- bind_rows(buf[2,], xyz) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Recharge est'))


# Sous-influence est
SiE <- buf[2, ] %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,]

SiE <- SiE %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(SiE) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence est'))


# Référence est
RfE <- buf[2, ] %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,]

RfE <- RfE %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(RfE) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence est'))

# Sous-influence ouest
xy <- data.frame(x = -68.42132, y = 49.04103)
quai <- st_as_sf(xy, coords = c('x','y'), crs = 4326) %>%
        st_transform(st_crs(buf))

SiO <- quai %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,]

SiO <- SiO %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(SiO) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence ouest'))

# Référence ouest
RfO <- quai %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,]

RfO <- RfO %>%
       st_buffer(800) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathymetrie) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(RfO) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence ouest'))

# Export zones
zones <- rbind(ReO, ReE, SiE, RfE, SiO, RfO)
mapview(zones)
# Export
st_write(zones, './data/zones.geojson', delete_dsn = TRUE)


# =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
# Create sub zones
create_segments <- function(x, d) {
  x %>%
  st_union() %>%
  st_cast("LINESTRING") %>%
  st_segmentize(d) %>%
  st_cast("POINT") %>%
  st_sf()
}


subzones <- function(x, name, r = FALSE, pt = FALSE) {
  dat <- st_cast(x, "POINT")
  p1 <- create_segments(dat[c(1,2), ], 800/3)
  p2 <- create_segments(dat[c(4,3), ], 800/3)

  if (r) {
    p3 <- create_segments(rbind(p1[1,], p2[1,]), 25)
    p4 <- create_segments(rbind(p1[2,], p2[2,]), 25)
    p5 <- create_segments(rbind(p1[3,], p2[3,]), 25)
    p6 <- create_segments(rbind(p1[4,], p2[4,]), 25)
  } else {
    p3 <- create_segments(rbind(p1[4,], p2[4,]), 25)
    p4 <- create_segments(rbind(p1[3,], p2[3,]), 25)
    p5 <- create_segments(rbind(p1[2,], p2[2,]), 25)
    p6 <- create_segments(rbind(p1[1,], p2[1,]), 25)
  }

  if (pt) {
    p3 <- p3[nrow(p3):1, ]
    p4 <- p4[nrow(p4):1, ]
    p5 <- p5[nrow(p5):1, ]
    p6 <- p6[nrow(p6):1, ]
  }

  # Proche
  ## 50-75m
  pr50 <- rbind(p6[3:4, ], p5[4:3,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-50')))

  ## 75-100m
  pr75 <- rbind(p6[4:5, ], p5[5:4,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-75')))

  ## 225-250
  pr225 <- rbind(p6[10:11, ], p5[11:10,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-225')))

  # Moyenne
  ## 50-75m
  mo50 <- rbind(p5[3:4, ], p4[4:3,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-50')))

  ## 75-100m
  mo75 <- rbind(p5[4:5, ], p4[5:4,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-75')))

  ## 225-250
  mo225 <- rbind(p5[10:11, ], p4[11:10,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-225')))

  # Éloigné
  ## 50-75m
  el50 <- rbind(p4[3:4, ], p3[4:3,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-50')))

  ## 75-100m
  el75 <- rbind(p4[4:5, ], p3[5:4,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-75')))

  ## 225-250
  el225 <- rbind(p4[10:11, ], p3[11:10,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-225')))

  # Bind all
  rbind(pr50, pr75, pr225 ,mo50, mo75, mo225, el50, el75, el225)
}

ReOsub <- subzones(ReO, 'ReO', r = TRUE)
ReEsub <- subzones(ReE, 'ReE', r = FALSE)
SiEsub <- subzones(SiE, 'SiE', r = FALSE)
RfEsub <- subzones(RfE, 'RfE', r = FALSE)
SiOsub <- subzones(SiO, 'SiO', r = TRUE)
RfOsub <- subzones(RfO, 'RfO', r = TRUE, pt = TRUE)

sous_zones <- rbind(ReOsub, ReEsub, SiEsub, RfEsub, SiOsub, RfOsub)
st_write(sous_zones, './data/sous_zones.geojson', delete_dsn = TRUE)


# =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
# Random sampling function
sampling <- function(x, ns = 5, wd = 2) {
  tmp <- list()
  for(i in 1:ns) {
    # Random sample
    tmp[[i]] <- st_sample(x, 1) %>%
                st_sf(data.frame(name = paste0(x$name, '-', i)))

    # Remove 2m buffer around sampled point
    x <- st_buffer(tmp[[i]], 2) %>%
         st_difference(x, .)
  }

  bind_rows(tmp)
}

# Samples
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
st_write(samples, './data/samples.geojson', delete_dsn = TRUE)

mapview(zones) + mapview(sous_zones) + mapview(samples)
