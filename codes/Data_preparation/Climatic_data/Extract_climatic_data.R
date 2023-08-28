# Extract climatic data
# Author: Jon Went, jwent@ethz.ch
# Date: 28.08.2023

rm(list = ls())

# ---- libraries

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

# --- load shapefile of routes -----

# get buffer distance
buffer_distance <- 500

#get routes
BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>% st_transform(5070)  %>%
  st_buffer(buffer_distance)

rm(buffer_distance)

# --- load tmax data ---- 

# Set the directory path where your .tif files are located
tmax.tif_folder <- "/Users/jonwent/polybox/Master_thesis/Climate_data/tasmax"

# List all .tif files in the folder
tmax.tif_files <- list.files(tmax.tif_folder, pattern = "\\.tif$", full.names = TRUE)

# Load and stack the .tif files
tmax.stack <- stack(tmax.tif_files)
# attr(tmax.stack, "srs")

# transform to same crs as BBS data (EPSG 5070)
tmax.stack_epsg5070 <- projectRaster(tmax.stack, crs = crs(BBS_routes))

# ----- load, stack and crop percipitation data ----

# Set the directory path where your .tif files are located
pr.tif_folder <- "/Users/jonwent/polybox/Master_thesis/Climate_data/percipitation"

# List all .tif files in the folder
pr.tif_files <- list.files(pr.tif_folder, pattern = "\\.tif$", full.names = TRUE)

# Load and stack the .tif files
pr.stack <- stack(pr.tif_files)
attr(pr.stack, "srs")

# transform to same crs as BBS data (EPSG 5070)
pr.stack_epsg5070 <- projectRaster(pr.stack, crs = crs(BBS_routes))

# crop to contiguous USA coordinates
# Define the bounding box coordinates for the contiguous USA in EPSG 5070
xmin <- -124.6639
ymin <- 25.1362
xmax <- -69.06614
ymax <- 48.97914

# Create the cropping extent
crop_extent <- extent(xmin, xmax, ymin, ymax)

pr.cropped <- crop(pr.stack, crop_extent)
pr.stack.cropped <- stack(pr.cropped)
# crs(pr.stack.cropped) <- "+init=epsg:5070"
# 
# attr(pr.stack, "srs")
# attr(pr.stack.cropped, "srs")

rm(pr.stack, pr.cropped)


# ----- extract the climatic data ------

# n_cores <- parallel::detectCores()

# get segment
segments <- unique(BBS_routes_buffered$partition)

pr_df_list <- list()

for (layer_idx in 1:24) {
  
  current_layer <- pr.stack.cropped[[layer_idx]]
  
  layer_stats_df <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  colnames(layer_stats_df) <- c("Route", "Mean", "Median", "Variance")
  
  for(s in 1:length(segments)) {
    
    print(paste0("working on segment ", s, "/",length(segments),". Layer ", layer_idx, "/", 24, " ..."))
    
    segment.temp <- BBS_routes_buffered %>% filter(partition == segments[s])
    
    extracted.pr <- exact_extract(current_layer, segment.temp)
    temp.partition <- segment.temp$partition
    
    temp.shp <- tibble("partition" = temp.partition)
    
    # Calculate statistics for each entry in extracted.tmax and store them
    for (i in seq_along(extracted.pr)) {
      
      entry <- extracted.pr[[i]] %>% as.data.frame() %>% unlist()
      
      stats <- c(mean = mean(entry), median = median(entry), variance = var(entry))
      
      layer_stats_df <- rbind(layer_stats_df, c(temp.partition[i], stats))
    }
  }
  
  # Store the data frame with statistics for the current layer
  pr_df_list[[layer_idx]] <- layer_stats_df
}


# --- Baustelle 

pr_data <- tibble(partition = BBS_routes_buffered$partition,
                         pr.mean = exact_extract(pr.stack[[1]], BBS_routes_buffered, "mean"),
                         pr.sd = exact_extract(pr.stack[[1]], BBS_routes_buffered, "stdev"))
