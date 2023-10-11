# Make route plot


# ---- library ----

library(tidyverse)
library(ggplot2)
library(patchwork)
# library(mapview)
# library(leaflet)
library(sf)
# library(sfheaders)
# library(rnaturalearth)
# library(rnaturalearthdata)

# ---- 
seg_final <- unique(abund.min40.lc$segment)

seg_tibble <- tibble(segs_fin = seg_final) %>%
  separate(segs_fin, sep = "_", into = c("state", "route", "seg"), remove = F) %>%
  unite(state, route,col= "U_S_R_I")

length(unique(seg_tibble$U_S_R_I))

bbs.routes.segmented.final <- sf::st_read("/Users/jonwent/Downloads/5_segments_final.shp")

epsg_5070 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "

bbs.routes <- bbs.routes.segmented.final %>%
  separate(partition, into = c("statenum", "route", "segnum"), sep = "_", remove = F) %>%
  na.omit() %>%
  rename(segment = partition) %>%
  separate(segment, sep = "_", into = c("state", "route", "seg"), remove = F) %>%
  unite(state, route,col= "U_S_R_I") %>%
  filter(
    U_S_R_I %in% seg_tibble$U_S_R_I
    # state %in% seg_tibble$state,
    # route %in% seg_tibble$route
    ) %>%
  st_transform(crs = epsg_5070)

unique(bbs.routes$route)

bbs.routes.buffered <- bbs.routes %>% st_buffer(500)

route39 <- bbs.routes %>% filter(U_S_R_I %in% "53_019")

route_39_seg <- bbs.routes %>% filter(U_S_R_I %in% "53_019", 
                                      seg %in% c(1,3,5))

route_39_seg_buf <- route_39_seg %>% st_buffer(500)

# mapview(bbs.routes.segmented.final,
#         zcol = "segment",
#         graticules = T,
#         col.regions = "blue",
#         projection = "+proj=utm +zone=33 +datum=WGS84",
#         legend = F)
# ---- USA map ----

usa_state <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- st_as_sf(maps::map("usa", fill=TRUE, plot =FALSE))

usa_5070 <- st_transform(usa, crs = epsg_5070)

disp_win_5070 <- st_sfc(st_point(c(-125, -66)), st_point(c(24, 50)),
                         crs = 5070)

disp_win_proj <- st_transform(disp_win_5070, crs = epsg_5070)

disp_win_coord <- st_coordinates(disp_win_5070)

xmin_wgs84 <- -110
xmax_wgs84 <- -109
ymin_wgs84 <- 47.5
ymax_wgs84 <- 48.5

xmin_5070 <- st_transform(st_sfc(st_point(c(xmin_wgs84, ymin_wgs84)), crs = 4326), crs = 5070)
xmax_5070 <- st_transform(st_sfc(st_point(c(xmax_wgs84, ymax_wgs84)), crs = 4326), crs = 5070)
ymin_5070 <- st_transform(st_sfc(st_point(c(xmin_wgs84, ymin_wgs84)), crs = 4326), crs = 5070)
ymax_5070 <- st_transform(st_sfc(st_point(c(xmax_wgs84, ymax_wgs84)), crs = 4326), crs = 5070)

bbox_5070 <- st_bbox(c(xmin_5070, ymin_5070, xmax_5070, ymax_5070), crs = 5070)

bbox_data <- data.frame(
  xmin = xmin_5070,
  xmax = xmax_5070,
  ymin = ymin_5070,
  ymax = ymax_5070
)

xmin_5070 <- bbox_5070$xmin
xmax_5070 <- bbox_5070$xmax
ymin_5070 <- bbox_5070$ymin
ymax_5070 <- bbox_5070$ymax

ttt <- pgirmess::bbox2sf(e = -110, w = -109, n = 48.5, s = 47.5)

# mapview::mapview(bbox_wgs84)

usa_map_segments <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0
          ) +
  geom_sf(data = bbs.routes,
          aes(fill = U_S_R_I), color = "grey50", linewidth = 0.3
          ) +
  geom_sf(data = ttt, fill = NA, linewidth = 0.3, color = "grey50", alpha = 0.8) +
  # geom_rect(aes(xmin = xmin_5070, xmax = xmax_5070, ymin = ymin_5070, ymax = ymax_5070),
  #           color = "grey10", fill = NA,
  #           linewidth = 0.6
  #           )  +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

usa_map_segments

# ---- segments map ----

mapview::mapview(route39)
mapview::mapview(bbox_5070, add = T)

route39_plot <- ggplot() +
  geom_sf(data = route_39_seg_buf,
          fill = "grey60", color = "grey60", linewidth = 0.3, alpha = 1
  ) +
  geom_sf(data = route39,
          aes(fill = segment), color = "grey40", linewidth = 0.7
  ) +
  geom_sf(data = route_39_seg,
          aes(fill = segment), color = "grey80", linewidth = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1, 
    # size = 6
    ))

route39_plot

# ---- final plot ----

map_plot <-  route39_plot + usa_map_segments

map_plot

ggsave("figures/routes_map.png",plot = map_plot, width = 8, height = 6, dpi = 300)

# ==== Baustelle ----

# coord_sf(
#   # xlim = c(-125, -66),
#   # ylim = c(24, 50),
#   xlim = disp_win_coord[,'X'],
#   ylim = disp_win_coord[,'Y'],
#   # crs = epsg_5070,
#   crs = st_crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "),
#   # datum = epsg_5070,
#   # ndiscr=1000
#   expand = FALSE
#          ) +

# graticules <- st_make_grid(usa, n = c(6, 6))
# st_crs(graticules) <- st_crs("+proj=longlat +datum=WGS84")
# graticules <- st_transform(graticules, crs = st_crs(usa))

# worldmap <- ne_countries(scale = 'medium', type = 'map_units',
#                          returnclass = 'sf')
# U_S <- worldmap[worldmap$name == 'United States',]
# 
# US_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,
#                           ymin = 30, ymax = 73)

# ggplot() + geom_sf(data = U_S) + theme_bw()