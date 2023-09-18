# Get the routes for the BBS data
#Author: Jon Went, jwent@ethz.ch
# Date: 12.09.2023

# --- 
library(tidyverse)

# install.packages("devtools")
# devtools::install_github("r-spatial/lwgeom")

# --- load BBS data

load("data/Lica/BBS_partition_abundance.rda")

load("data/BBS.full.stable.min10.rda")
load("data/BBS.full.stable.min40.rda")

load("data/d.abund.min40.rda")

all.bird.segments <- sort(unique(BBS_partition_abundance$partition)) # 2976 segments
bird.min10.segments <- sort(unique(BBS.stable.full.min10$segment)) # 2937 segments
bird.min40.segments <- sort(unique(BBS.stable.full.min40$segment)) # 2937 segments

d.abund.min40.segments <- sort(unique(d.abund.min40$segment)) # 2379 segments

# ---- load route data and keep only the ones for which we have BBS data ----

# bbs.routes.segmented <- sf::st_read("/Users/jonwent/Downloads/3_routes_segmented.shp") %>%
#   group_by(U_S_R_I) %>%
#   mutate(SegNum = 1:5)

bbs.routes.segmented.final <- sf::st_read("/Users/jonwent/Downloads/5_segments_final.shp")

bbs.routes.segmented.final <- bbs.routes.segmented.final %>%
  separate(partition, into = c("statenum", "route", "segnum"), sep = "_", remove = F) %>%
  na.omit() %>%
  filter(segnum %in% c(1,3,5))

# length(bbs.routes.segmented.final$partition) # 2880 segments

# mapview::mapview(bbs.routes.segmented.final)


# # see common segments ----
# 
# shp.segments <- sort(unique(bbs.routes.segmented.final$partition)) # 2880 segments
# 
# common.all <- intersect(all.bird.segments, shp.segments) # 1779 segments
# 
# common.min10 <- intersect(bird.min10.segments, shp.segments) # 1779 segments
# 
# common.min40 <- intersect(shp.segments, bird.min40.segments) # 1779 segments
# 
# common.d.abund.min40 <- intersect(shp.segments, d.abund.min40.segments) # 1698 segments
# 
# 
# 
# # see what way around better to keep more info ----
# 
# # first d.abund calculation and then join the bioclim data
# 
# d.abund.min40$common <- d.abund.min40$segment %in% common.d.abund.min40
# 
# d.abund.tmp <- d.abund.min40 %>% filter(common == T)
# 
# length(unique(d.abund.tmp$segment)) # 1698 segments
# 
# length(unique(d.abund.tmp$animal_jetz)) # 143 species
# 
# # second: first join bioclim data and then calculate d.abund
# 
# BBS.stable.full.min40$common <- BBS.stable.full.min40$segment %in% common.min40
# 
# min40.tmp <- BBS.stable.full.min40 %>% filter(common == T)
# 
# length(unique(min40.tmp$segment)) # 1779 segments
# 
# length(unique(min40.tmp$animal_jetz)) # 143 species
# 
# min40.tmp <- min40.tmp %>%
#   arrange(animal_jetz, segment) %>%
#   group_by(animal_jetz, segment) %>%
#   mutate(d.abund =  abund.geom.mean - lag(abund.geom.mean)
#          ) %>%
#   filter(all(abund.geom.mean !=0)
#          ,
#          !is.na(d.abund)
#   ) %>%
#   na.omit()
# 
# length(unique(min40.tmp$segment)) # 1698 segments
# 
# length(unique(min40.tmp$animal_jetz)) # 143 species
# 
# hist(min40.tmp$d.abund)
# 
# 
# # and with the current segment selection 
# 
# bioclim.complete.cases <- bioclim.df %>% na.omit() 
# # summary(bioclim.complete.cases)
# 
# common.min40.now <- sort(intersect(bioclim.complete.cases$segment, BBS.stable.full.min40$segment)) # 1890 segments
# 
# BBS.stable.full.min40$common <- BBS.stable.full.min40$segment %in% common.min40.now
# 
# min40.tmp <- BBS.stable.full.min40 %>% filter(common == T)
# 
# length(unique(min40.tmp$segment)) # 1890 segments
# 
# length(unique(min40.tmp$animal_jetz)) # 143 species
# 
# min40.tmp <- min40.tmp %>%
#   arrange(animal_jetz, segment) %>%
#   group_by(animal_jetz, segment) %>%
#   # select(-abund.mean, -abund.median, -abund.var) %>%
#   mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
#   mutate(d.abund =  abund.geom.mean - lag(abund.geom.mean)
#   ) %>%
#   filter(all(abund.geom.mean !=0)
#          ,
#          # !is.na(d.abund)
#   ) %>%
#   mutate(d.abund.int = as.integer(d.abund))
#   # na.omit()
# 
# hist(min40.tmp$d.abund.int)
# 
# length(unique(min40.tmp$segment)) # 1642 segments
# 
# length(unique(min40.tmp$animal_jetz)) # 143 species


# ===== Baustelle -----
# bbs.routes.segmented$segment <- paste0(bbs.routes.segmented$U_S_R_I, "_", bbs.routes.segmented$SegNum)
# mapview::mapview(bbs.routes.segmented)
# 
# bbs.routes.tmp <- bbs.routes.segmented %>% filter(!(SegNum %in% c(2, 4)))
# mapview::mapview(bbs.routes.tmp)
# 
# bbs.routes.buffered <- bbs.routes.tmp %>% sf::st_transform(4326) %>%
#     sf::st_buffer(500)
# 
# mapview::mapview(bbs.routes.buffered)
# 
# # length(bbs.routes.tmp$segment) # 3420 segments
# 
# # length(unique(bbs.routes.raw$U_S_R_I)) # 3229 routes without BBS filter
# # length(unique(bbs.routes.raw.shp$U_S_R_I)) # 905 routes with BBS filter
# 
# bbs.routes.info <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS/routes_info.csv")
# 
# bbs.routes.info$StateNum <- str_pad(bbs.routes.info$StateNum, width=2, side="left", pad="0")  
# bbs.routes.info$Route <- str_pad(bbs.routes.info$Route, width=3, side="left", pad="0")
# bbs.routes.info$U_S_R_I <- paste0(bbs.routes.info$StateNum, "_", bbs.routes.info$Route)
# 
# routes.shp <- subset(bbs.routes.raw.shp, RTELENG > 32186) # longer than 20 miles, 902 kept
# routes.shp <- subset(routes.shp, RTELENG < 48280) # shorter than 30 miles, 846 kept


# bbs.routes.start <- bbs.routes.info %>% filter(CountryNum==840) %>%
#   filter(U_S_R_I %in% BBS_partition_abundance$U_S_R_I) %>%
#   select(Latitude, Longitude, U_S_R_I)

# length(unique(bbs.routes.start$U_S_R_I)) # 992 routes with route info

# # segmentize ----
# 
# routes.to.segment <- sf::st_sf(id = 1:nrow(routes.shp),
#                                geometry = sf::st_sfc(lapply(1:nrow(routes.shp),
#                                                             function(x) sf::st_geometrycollection())))
# 
# routes.to.segment <- sf::st_set_geometry(routes.to.segment, routes.shp$geometry)
# 
# U_S_R_I_names <- routes.shp$U_S_R_I
# 
# num_segments <- 5
# 
# result_list <- list()
# 
# for (i in 1:nrow(routes.to.segment)) {
#   
#   # Extract the geometry from the current row
#   geom <- routes.to.segment[i, ]$geometry
#   
#   name.tmp <- routes.to.segment$U_S_R_I[i]
#   
#   # Convert to linestring so we can segmentize
#   geom <- geom %>% sf::st_cast("LINESTRING")
#   
#   # Create the specification data frame for the segments
#   out_spec <- data.frame(
#     ID = rep(i, num_segments),
#     newID = 1:num_segments,
#     start = seq(0, (num_segments - 1) / num_segments, length.out = num_segments),
#     end = seq(1 / num_segments, 1, length.out = num_segments)
#   )
#   
#   # Create a list to store the segmented geometries
#   out_geo <- list()
#   
#   # Loop through each segment and store it in the list
#   for (j in 1:nrow(out_spec)) {
#     out_geo[[out_spec$newID[j]]] <- lwgeom::st_linesubstring(geom, out_spec$start[j], out_spec$end[j])[[1]]
#   }
#   
#   # Create an sf object with the specified data frame and segmented geometries
#   result <- st_sf(out_spec, st_sfc(out_geo))
#   
#   # Add the result to the list
#   result_list[[name.tmp]] <- result
# }
# 
# final_result <- do.call(rbind, result_list) %>% filter(!(newID %in% c(2, 4)))
# 
# length(final_result$ID) # 2538 segments
# 
# mapview::mapview(final_result)
# 
# 
# final_result.buffered <- final_result %>% 
#   sf::st_transform(4326) %>%
#   sf::st_buffer(500)
# 
# 
# mapview::mapview(final_result.buffered)
# 

# ----
# bbs.routes.segmented.start <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes segmented/routes_start_points_5yr_3time.shp")

# get buffer distance
# buffer_distance <- 500
# 
# #get routes
# BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
#   mutate(route = stringr::str_sub(partition,1, 6)) %>% sf::st_transform(4326) %>%
#   sf::st_buffer(buffer_distance)

# rm(buffer_distance)

# routes.tmp <- BBS_routes %>%
#   separate(partition, into = c("StateNum", "Route1","SegNum"), sep = "_", remove = F)
# 
# tttt <- routes.tmp %>%
#   filter(is.na(SegNum))%>%
#   mutate(SegNum.man = 5:1) %>%
#   unite(partition, route, SegNum.man, sep = "_", remove = F) %>%
#   select(-SegNum.man) %>%
#   mutate(SegNum = 5:1) %>%
#   relocate(partition)
# 
# routes.tmp <- rbind(routes.tmp, tttt); rm(tttt) 
# 
# routes.tmp <- routes.tmp %>% na.omit()
# 
# ttt <- routes.tmp %>% filter(!(SegNum %in% c(2, 4))) # 2001 segments

