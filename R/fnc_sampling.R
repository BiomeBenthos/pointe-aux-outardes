# Random sampling function
sampling_point <- function(x, ns = 10, wd = 10) {
  tmp <- list()
  for(i in 1:ns) {
    # Random sample
    tmp[[i]] <- sf::st_sample(x, 1) %>%
                sf::st_sf(data.frame(name = paste0(x$name, '-', i)))

    # Remove 2m buffer around sampled point
    x <- sf::st_buffer(tmp[[i]], wd) %>%
         sf::st_difference(x, .)
  }

  tmp <- do.call(rbind, tmp)

}

sampling <- function(year,
                     poly = sf::st_read('./data/spatial/sous_zones.geojson'),
                     ns = 10,
                     wd = 10) {
  
  # Samples
  samples <- list()
  for(i in 1:nrow(poly)) {
    samples[[i]] <- sampling_point(poly[i, ])
  }
  samples <- do.call(rbind, samples)

  # To WGS84
  samples <- sf::st_transform(samples, "EPSG:4326")
  
  # Split between points and supplementary points
  samples_main <- samples[grep("-[1-5]$", samples$name),]
  samples_main_df <-  samples_main |>
    terra::vect() |>
      as.data.frame(x = _, geom = "XY")
  colnames(samples_main_df) <- c("station_id", "longitude", "latitude")
  samples_supp <- samples[c(grep("-[6-9]$", samples$name),grep("10$", samples$name)) |> sort(),]
  samples_supp_df <-  samples_supp |>
    terra::vect() |>
      as.data.frame(x = _, geom = "XY")
  colnames(samples_supp_df) <- c("station_id", "longitude", "latitude")

  output_geojson <- here::here(sprintf("data/sampling_pts/%d/sampling_pts_pao_%d.geojson", year, year))
  output_main_gpx <- here::here(sprintf("data/sampling_pts/%d/sampling_pts_pao_%d.gpx", year, year))
  output_supp_gpx <- here::here(sprintf("data/sampling_pts/%d/sampling_pts_pao_%d_supp.gpx", year, year))
  output_main_csv <- here::here(sprintf("data/sampling_pts/%d/sampling_pts_pao_%d.csv", year, year))
  output_supp_csv <- here::here(sprintf("data/sampling_pts/%d/sampling_pts_pao_%d_supp.csv", year, year))

  if(!dir.exists(here::here(sprintf("data/sampling_pts/%d", year)))) {
    dir.create(here::here(sprintf("data/sampling_pts/%d", year)))
  }

  sf::st_write(samples, output_geojson, delete_dsn = TRUE)
  sf::st_write(samples_main, output_main_gpx, delete_dsn = TRUE)
  sf::st_write(samples_supp, output_supp_gpx, delete_dsn = TRUE)
  write.csv(samples_main_df, output_main_csv, row.names = FALSE)
  write.csv(samples_supp_df, output_supp_csv, row.names = FALSE)
}


## Check points
#mapview::mapview(sous_zones) + 
#  mapview::mapview(samples)
