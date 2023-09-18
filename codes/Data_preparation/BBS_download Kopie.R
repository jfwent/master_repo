# download NA BBS data
# Author: Jon Went, jwent@ethz.ch
# Date: 04.09.2023

# ---- libraries ----

# library(rdataretriever)
# library(DBI)
library(tidyverse)

# reticulate::use_virtualenv("myenv")
# reticulate::py_install("retriever") 
# system("python --version") 3.11.5

#----- 
# Don't run again
# rdataretriever::datasets()
# rdataretriever::install_sqlite(dataset='breed-bird-survey',file='bbs_sqlite.db', debug=FALSE, use_cache=TRUE)

# ---- 

source("other_studies/SCAT-main/Data_management.R")
bbs <- get_bbs_data()

# bbs_db <- dbConnect(RSQLite::SQLite(), 'bbs.sqlite.db')
# surveys <- tbl(bbs_db, "breed_bird_survey_counts")
# sites <- tbl(bbs_db, "breed_bird_survey_routes")

ttt <- bbs %>%
  filter(year %in% 2000:2002 | year %in% 2017:2019) %>%
  rename(aou = species_id) %>%
  rowwise() %>%
  mutate(sep = ifelse(length(site_id) <= 4, 1, 2))
  # separate(site_id, into = c("StateNum", "Route"), sep = ifelse(length(site_id) == 4, 1, 2), remove = F)

species <- read.csv("data/bbs_species.csv")
species$animal_jetz <- paste0(species$genus, "_", species$species)
species <- species %>%
  as_tibble() %>%
  dplyr::select(aou, animal_jetz)

ttt <- ttt %>% left_join(species, by = "aou"); rm(species)

# Get locations of surveys
locations <- dplyr::select(ttt, site_id, long, lat) %>% distinct()
sp::coordinates(locations) <- c("long", "lat")
raster::crs(locations) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
mapview::mapview(locations)

routes.shp <- sf::read_sf("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS/Breeding Bird Survey Route Locations for Lower 48 States/data/commondata/data0/bbsrtsl020Copy.shp") %>%
  rename(RouteName = RTENAME)
mapview::mapview(routes.shp)

usbbs.routes <- read.csv('/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS/routes_info.csv') %>%
  mutate(site_id = (StateNum*1000+Route))
usbbs.routes$StateNum <- stringr::str_pad(usbbs.routes$StateNum, width=2, side="left", pad="0")
usbbs.routes$Route <- stringr::str_pad(usbbs.routes$Route, width=3, side="left", pad="0")
usbbs.routes$U_S_R_I <- paste0(usbbs.routes$StateNum, "_", usbbs.routes$Route)

us.routes <- usbbs.routes %>% filter(CountryNum == 840)

tttt <- ttt %>% left_join(us.routes, by = "site_id") %>% na.omit()
length(unique(tttt$Route))

locations <- dplyr::select(tttt, site_id, long, lat) %>% distinct()
sp::coordinates(locations) <- c("long", "lat")
raster::crs(locations) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
mapview::mapview(locations)

usbbs.routes.start <- usbbs.routes %>% filter(CountryNum==840) %>%
  filter(U_S_R_I %in% tttt$U_S_R_I) %>%
  select(Latitude, Longitude, U_S_R_I, RouteName)

usbbs.routes.start.polyline <- usbbs.routes.start %>%
  left_join(routes, by = "RouteName") %>%
  na.omit()

# length(unique(usbbs.routes.start$U_S_R_I))
