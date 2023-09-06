# (Mu et al., 2020) Human footprint index data preparation
# Author: Jon Went
# Date : 22.08.2023

# ----
rm(list=ls())

library(tidyverse)
library(raster)
library(utils)
library(sf)
library(stars)
library(mapview)
library(exactextractr)

#---- load Mu et al., 2020 Human Footprint data -----

# Get directory name
zip_dir <- "/Users/jonwent/Downloads/16571064"

# Get zip file names
zip_files <- list.files(path = zip_dir, pattern = ".zip", full.names = TRUE)

# Store results
hfp_list <- list()

for (zip_file in zip_files) {
  # Extract the year from the zip file name
  year <- gsub("^.*([0-9]{4}).zip$", "\\1", basename(zip_file))
  
  # Create a directory for extracted files
  exdir <- file.path(zip_dir, year)
  dir.create(exdir, showWarnings = FALSE)
  
  # Unzip the .zip file into the directory
  unzip(zipfile = zip_file, exdir = exdir)
  
  # Get a list of TIFF files in the extracted directory
  tiff_files <- list.files(path = exdir, pattern = ".tif$", full.names = TRUE)
  
  # Loop through each TIFF file
  for (tiff_file in tiff_files) {
    # Load the TIFF file using the raster package
    tiff_data <- raster(tiff_file)
    
    # Print the name of the loaded TIFF
    print(paste("Loaded:", basename(tiff_file)))
    
    # Store results
    hfp_list[[paste0("hfp", year)]] <- tiff_data
  }
}

rm(tiff_data)

# --- load shapefile of routes -----

#get routes
# BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp")

BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>% st_transform(5070)

# get buffer distance
buffer_distance <- 500

# build buffered routes
BBS_routes_buffered <- BBS_routes %>%
  st_buffer(buffer_distance)

# view results
# leafsync::latticeview(mapview::mapview(BBS_routes),
#                       mapview::mapview(BBS_routes_buffered))

# --- extract data from raster ----

# create raster stack
hfp_stack <- raster::stack(hfp_list)

# transform to same crs as BBS data (EPSG 5070)
crs(hfp_stack) <- "+init=epsg:5070"

# get routes
routes <- unique(BBS_routes_buffered$route)

statistics_df_list <- list()

for (layer_idx in 1:21) {
  
  current_layer <- hfp_stack[[layer_idx]]
  
  print(paste0("working on layer ", layer_idx, "/", length(hfp_stack), " ..."))
  
  layer_stats_df <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  colnames(layer_stats_df) <- c("Route", "Mean", "Median", "Variance")
  
  for(r in seq(1,length(routes),2) ) {
    
    print(paste0("working on route ", r, "/",length(routes)," ..."))
    
    segment.temp <- BBS_routes_buffered %>% filter(route == routes[r] | route == routes[r+1])
    
    extracted.hfp <- exact_extract(current_layer, segment.temp)
    temp.partition <- segment.temp$partition
    
    temp.shp <- tibble("partition" = temp.partition)
    
    # extracted.t1 <- lapply(extracted.hfp, function(x){ x %>% dplyr::select(hfp2000) %>% c() %>% unlist()})

    # Calculate statistics for each entry in extracted.hfp and store them
    for (i in seq_along(extracted.hfp)) {
      
      entry <- extracted.hfp[[i]] %>% as.data.frame() %>% unlist()
      
      stats <- c(mean = mean(entry), median = median(entry), variance = var(entry))
      
      layer_stats_df <- rbind(layer_stats_df, c(temp.partition[i], stats))
    }
  }
  
  # Store the data frame with statistics for the current layer
  statistics_df_list[[layer_idx]] <- layer_stats_df
}

tmp <- statistics_df_list

names(tmp) <- 2000:2020

new_colnames <- c("segment", "hfp.mean", "hfp.median", "hfp.variance")

tmp_renamed <- lapply(tmp, function(df) {
  colnames(df) <- new_colnames
  return(df)
})

hfp_df <- data.frame()

for (i in seq_along(tmp_renamed)) {
  # Get the current entry's data frame
  current_df <- tmp_renamed[[i]]
  
  # Add a new column "year" with the name of the entry
  current_df$year <- names(tmp_renamed)[i]
  
  # Append the current data frame to the combined data frame
  hfp_df <- rbind(combined_df, current_df)
}

hfp_df <- hfp_df %>%
  relocate(year, .before = segment) %>%
  mutate(
    hfp.mean = as.numeric(hfp.mean),
    hfp.median = as.numeric(hfp.median),
    hfp.variance = as.numeric(hfp.variance)
  ) %>%
  mutate(
    hfp.mean = round(hfp.mean, 3),
    hfp.median = round(hfp.median, 3),
    hfp.variance = round(hfp.variance, 3)
  )

hfp.t1 <- hfp_df %>%
  filter(year %in% 2000:2002) %>%
  group_by(segment) %>%
  summarize(hfp.mean = round(mean(hfp.mean),3),
         hfp.median = round(median(hfp.median),3),
         hfp.var = round(var(hfp.variance),3)) %>%
  mutate(year = 2001) %>%
  relocate(year)

hfp.t2 <- hfp_df %>%
  filter(year %in% 2018:2020) %>%
  group_by(segment) %>%
  summarize(hfp.mean = round(mean(hfp.mean),3),
            hfp.median = round(median(hfp.median),3),
            hfp.var = round(var(hfp.variance),3)) %>%
  mutate(year = 2019) %>%
  relocate(year)

hfp.full <- rbind(hfp.t1, hfp.t2)

# summary(hfp.t1)
# summary(hfp.t2)

save(hfp_df, file = "data/human_footprint_df.rda")
save(hfp.full, file = "data/hfp_t1_t2.rda")
