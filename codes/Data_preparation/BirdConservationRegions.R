# Bird conservation regions of North America
# Author Jon Went, jwent@ethz.ch
# Date: 06.09.2023

# ---- 
# library(terra)
# library(sf)
library(tidyverse)

# ---- 
BCR <- sf::read_sf(dsn = "/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BCR_Terrestrial/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY == "USA", PROVINCE_S != "ALASKA", PROVINCE_S != "HAWAIIAN ISLANDS") %>% sf::st_transform(4326)

plot(BCR)

# get buffer distance
buffer_distance <- 500

#get routes
BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>% sf::st_transform(4326)  %>%
  sf::st_buffer(buffer_distance); rm(buffer_distance)

plot(BBS_routes)

intersections <- sf::st_intersection(BBS_routes$partition, BCR$BCRNAME)
