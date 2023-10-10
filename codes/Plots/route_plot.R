# Make route plot


# ---- library ----

library(tidyverse)
library(mapview)
library(leaflet)
library(sf)
library(sfheaders)
# library(rnaturalearth)
# library(rnaturalearthdata)

# ---- 

seg_final <- unique(abund.min40.lc$segment)

bbs.routes.segmented.final <- sf::st_read("/Users/jonwent/Downloads/5_segments_final.shp")

bbs.routes.segmented.final <- bbs.routes.segmented.final %>%
  separate(partition, into = c("statenum", "route", "segnum"), sep = "_", remove = F) %>%
  na.omit() %>%
  rename(segment = partition) %>%
  filter(segnum %in% c(1,3,5),
         segment %in% seg_final)

# mapview(bbs.routes.segmented.final,
#         zcol = "segment",
#         graticules = T,
#         col.regions = "blue",
#         projection = "+proj=utm +zone=33 +datum=WGS84",
#         legend = F)

epsg_5070 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "
# ---- 

graticules <- st_make_grid(usa, n = c(18, 9))

st_crs(graticules) <- st_crs("+proj=longlat +datum=WGS84")

graticules <- st_transform(graticules, crs = st_crs(usa))

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
U_S <- worldmap[worldmap$name == 'United States',]

US_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,
                          ymin = 30, ymax = 73)

ggplot() + geom_sf(data = U_S) + theme_bw()

ggplot() +
  # geom_sf(data = graticules, color = "grey50", size = 0.25, linetype = 2) +
  geom_sf(data = usa,
          fill = "white", color = "grey90", size = 5
          ) +
  geom_sf(data = bbs.routes.segmented.final,
          aes(fill = segment), color = "black", size = 5
          ) +
  # st_graticule(crs = st_crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "),
  #              datum = NA) +
  coord_sf(crs = st_crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "),
           datum = NA, ndiscr=1000
           ) +
  theme_bw()
