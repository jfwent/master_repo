# Protected areas
# Author: Jon Went, jwent@ethz.ch
# Date: 06.09.2023

# ---- libraries ----

library(tidyverse)

# protected_areas <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/WDPA_WDOECM_Sep2023_Public_USA_csv/WDPA_WDOECM_Sep2023_Public_USA_csv.csv")

# pr_01_points <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_0/WDPA_WDOECM_Sep2023_Public_USA_shp-points.shp") %>%
#   filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")
# pr_02_points <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_1/WDPA_WDOECM_Sep2023_Public_USA_shp-points.shp") %>%
#   filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")
# pr_03_points <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_2/WDPA_WDOECM_Sep2023_Public_USA_shp-points.shp") %>%
#   filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")

pr_poly_01 <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_0/WDPA_WDOECM_Sep2023_Public_USA_shp-polygons.shp") %>%
  filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")
pr_poly_02 <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_1/WDPA_WDOECM_Sep2023_Public_USA_shp-polygons.shp") %>%
  filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")
pr_poly_03 <- sf::read_sf(dsn = "/Users/jonwent/Downloads/WDPA_WDOECM_Sep2023_Public_USA_shp/WDPA_WDOECM_Sep2023_Public_USA_shp_2/WDPA_WDOECM_Sep2023_Public_USA_shp-polygons.shp") %>%
  filter(SUB_LOC != "US-AK", SUB_LOC != "US-HI")


