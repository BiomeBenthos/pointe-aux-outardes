library(sf)
library(tidyverse)
library(magrittr)
#library(pao)
sous_zones <- st_read('./data/sous_zones.geojson')
source('./R/fnc_sampling.R')

# Samples 2021
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
# st_write(samples, './data/samples_2021.geojson', delete_dsn = TRUE)


# Samples 2022
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
#st_write(samples, './data/samples_2022.geojson', delete_dsn = TRUE)

# Samples 2023
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
st_write(samples, './data/samples_2023.geojson', delete_dsn = TRUE)

