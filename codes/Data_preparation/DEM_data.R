# Elevation data preparation
# Author: Jon Went
# Date : 22.08.2023

# ---- 

rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

# --- load shapefile of routes -----

# get buffer distance
buffer_distance <- 500

#get routes
BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>%
  st_transform(5070) %>%
  st_buffer(buffer_distance)

plot(BBS_routes)

BBS_routes_epsg4326 <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>%
  st_transform(4326) %>%
  st_buffer(buffer_distance)

plot(BBS_routes_epsg4326)

# build buffered routes
# BBS_routes_buffered <- BBS_routes %>%
#   st_buffer(buffer_distance) ; rm(BBS_routes, buffer_distance)

#---- load elevation data ----

# Data from *SRTM* sourced automatically from raster package, 90m gridded data, https://srtm.csi.cgiar.org/
# raster::getData('alt', country='USA', path="data/elevation")

# keep only contigous mainland USA
elevation <- raster("data/elevation/USA1_msk_alt.grd")
# plot(elevation)
# attr(elevation, "srs")

# crs(elevation) <- "+init=epsg:5070"
elevation_epsg5070 <- projectRaster(elevation, crs = crs(BBS_routes))

plot(elevation_epsg5070)
# crs(elevation_data.tmp) <- "+init=epsg:5070"
# attr(elevation_epsg5070, "srs")

# ----- Extract data ----

elevation_data_epsg5070 <- tibble(partition = BBS_routes$partition,
                         elev.mean = exact_extract(elevation_epsg5070, BBS_routes, "mean"),
                         elev.sd = exact_extract(elevation_epsg5070, BBS_routes, "stdev")) %>%
  mutate(elev.mean = round(elev.mean, 3),
         elev.sd = round(elev.sd, 3))

elevation_data_epsg4326 <- tibble(partition = BBS_routes_epsg4326$partition,
                         elev.mean = exact_extract(elevation, BBS_routes_epsg4326, "mean"),
                         elev.sd = exact_extract(elevation, BBS_routes_epsg4326, "stdev")) %>%
  mutate(elev.mean = round(elev.mean, 3),
         elev.sd = round(elev.sd, 3))

save(elevation_data_epsg5070, file = "data/elevation/elevation_espg5070.rda")
save(elevation_data_epsg4326, file = "data/elevation/elevation_espg4326.rda")
