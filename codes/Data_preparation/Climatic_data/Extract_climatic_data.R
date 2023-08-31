# Extract climatic data
# Author: Jon Went, jwent@ethz.ch
# Date: 28.08.2023

# ---- libraries ----

library(tidyverse)

# _______________ crop and project climate data ----

# ---- set crop extent and crs ----

# Define the bounding box coordinates for the contiguous USA in EPSG 4326
xmin <- -124.6639
ymin <- 25.1362
xmax <- -69.06614
ymax <- 48.97914

# Create the cropping extent
# crop_extent <- as(extent(xmin, xmax, ymin, ymax), 'SpatialPolygons')
# crs(crop_extent) <- "+proj=longlat +datum=WGS84 +no_defs"
crop_extent <- raster::extent(xmin, xmax, ymin, ymax)

crs_4326 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

rm(xmin, ymin, xmax, ymax)

# --- write function ----

process_and_save_rasters <- function(folder_path, crop_extent.fun, crs_target) {
  
  # List all .tif files in the folder
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  
  i = 1
  
  # Loop through each TIFF file
  for (tif_file in tif_files) {
    
    print(paste0("working on ", i, "/", length(tif_files), " ..."))
    
    # Load the .tif file
    raster.now <- raster(tif_file)
    
    # Crop to the specified extent
    raster.cropped <- crop(raster.now, crop_extent)
    
    # Project the cropped raster
    raster.projected <- projectRaster(raster.cropped, crs = crs_target)
    
    # save the raster layer
    file_name <- basename(tif_file)  # Get the original file name
    file_name_without_extension <- tools::file_path_sans_ext(file_name)  # Remove extension
    processed_file_name <- paste0(file_name_without_extension, "_processed.tif")  # Add "_processed" to the name
    output_path <- file.path(folder_path, processed_file_name)  # Create the output file path
    
    writeRaster(raster.projected, filename = output_path, format = "GTiff")
    
    i = i + 1
  }
  
  rm(raster.cropped, raster.projected, tif_file, tif_files, raster.now, i)
  gc()  # Perform garbage collection to free up memory
}

# --- loop over folders ----

# List of folder names
folders <- c("cmi", "swb", "tmax", "tmin", "precipitation")

i <- 1

# Loop over the folders
for (folder in folders) {
  folder_path.tmp <- file.path("/Users/jonwent/polybox/Master_thesis/Climate_data", folder)
  
  print(paste0("working on folder ", i, "/", length(folders), " ..."))
  
  # Call the extract_climate_data function for each folder
  process_and_save_rasters(folder_path = folder_path.tmp,
                           crop_extent.fun = crop_extent, crs_target = crs_4326)
  
  i = i + 1
}

rm(crop_extent, crs_4326, folder, i, folders)


# _______________ extract the climatic data

# --- load routes shapefile -----

# get buffer distance
buffer_distance <- 500

#get routes
BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master_thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>% sf::st_transform(4326)  %>%
  sf::st_buffer(buffer_distance)

rm(buffer_distance)

# --- write function ----

extract_climate_data <- function(folder_path, BBS_routes_df = BBS_routes, num_cores = 4){
  
  library(foreach)
  
  # Set the directory path where your .tif files are located
  tif_folder <- folder_path
  
  # List all .tif files in the folder
  tif_files <- list.files(tif_folder, pattern = "\\_processed.tif$", full.names = TRUE)
  
  stack.tmp <- raster::stack(tif_files)
  
  list.tmp <- list()
  
  # Initialize a cluster with the specified number of cores
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  result_list <- foreach::foreach(i = 1:raster::nlayers(stack.tmp)) %dopar% {
    
    # print(paste0("working on ", i, "/", raster::nlayers(stack.tmp), " ..."))
    
    # Extract time period from file name using regular expression
    file_name <- tools::file_path_sans_ext(basename(tif_files[i]))
    time_period <- sub(".*_(\\w+)_\\d+_processed$", "\\1", file_name)
    
    df.tmp <- tibble::tibble(segment = BBS_routes_df$partition,
                         mean = exactextractr::exact_extract(stack.tmp[[i]], BBS_routes_df, "mean",
                                                                 progress = F),
                         median = exactextractr::exact_extract(stack.tmp[[i]], BBS_routes_df, "median",
                                                                   progress = F),
                         stdev = exactextractr::exact_extract(stack.tmp[[i]], BBS_routes_df, "stdev",
                                                               progress = F),
                         max = exactextractr::exact_extract(stack.tmp[[i]], BBS_routes_df, "max",
                                                            progress = F),
                         min = exactextractr::exact_extract(stack.tmp[[i]], BBS_routes_df, "min",
                                                            progress = F),
                         time_period = time_period)
    
    # list.tmp[[i]] <- df.tmp
    
    df.tmp
  }
  
  parallel::stopCluster(cl)  # Stop the parallel cluster
  
  # Combine the results into a single data frame
  df.tmp <- do.call(rbind, result_list)
  
  # names(list.tmp) <- tif_files
  
  # Add a column with the corresponding file names
  # df.tmp$file <- rep(tif_files, each = nrow(BBS_routes_df))
  
  # df.tmp <- list.tmp %>% 
  #   reshape2::melt(id.vars = c("segment", "mean", "median", "stdev"),
  #                  value.name = "value")
  
  return(df.tmp)
}

# ---- loop over folders ----

# List of folder names
folders <- c("cmi", "swb", "tmax", "tmin", "precipitation")

# Create an empty list to store the results
result_list <- list()

i = 1

# Loop over the folders
for (folder in folders) {
  folder_path.tmp <- file.path("/Users/jonwent/polybox/Master_thesis/Climate_data", folder)
  
  print(paste0("working on folder ", i, "/", length(folders), " ..."))
  
  # Call the extract_climate_data function for each folder
  result <- extract_climate_data(folder_path = folder_path.tmp)
  
  # Store the result in the list
  result_list[[folder]] <- result
  
  i = i + 1
}

pr.df <- extract_climate_data(folder_path = "/Users/jonwent/polybox/Master_thesis/Climate_data/precipitation")
cmi.df <- result_list$cmi
swb.df <- result_list$swb
tmax.df <- result_list$tmax
tmin.df <- result_list$tmin

rm(result_list, result)

# ----- set time periods ---- 

cmi_df <- cmi.df %>%
  mutate(time_period.tmp = gsub("CHELSA_cmi_|_V.2.1_processed", "", time_period)) %>%
  separate(time_period.tmp, sep = "_", into = c("month", "year"), remove = T) %>%
  relocate(year, month) %>%
  select(-time_period)

pr_df <- pr.df %>%
  mutate(time_period.tmp = gsub("CHELSA_pr_|_V.2.1_processed", "", time_period)) %>%
  separate(time_period.tmp, sep = "_", into = c("month", "year"), remove = T) %>%
  relocate(year, month) %>%
  select(-time_period)

tmax_df <- tmax.df %>%
  mutate(time_period.tmp = gsub("CHELSA_tasmax_|_V.2.1_processed", "", time_period)) %>%
  separate(time_period.tmp, sep = "_", into = c("month", "year"), remove = T) %>%
  relocate(year, month) %>%
  select(-time_period)

tmin_df <- tmin.df %>%
  mutate(time_period.tmp = gsub("CHELSA_tasmin_|_V.2.1_processed", "", time_period)) %>%
  separate(time_period.tmp, sep = "_", into = c("month", "year"), remove = T) %>%
  relocate(year, month) %>%
  select(-time_period)

swb_df <- swb.df %>%
  mutate(time_period.tmp = gsub("CHELSA_swb_|_V.2.1_processed", "", time_period)) %>%
  rename(year = time_period.tmp) %>%
  relocate(year) %>%
  select(-time_period)

# ----- save dataframes ---- 

save(cmi_df, file="data/Climate/cmi_df.rda")
save(pr_df, file="data/Climate/pr_df.rda")
save(swb_df, file="data/Climate/swb_df.rda")
save(tmax_df, file="data/Climate/tmax_df.rda")
save(tmin_df, file="data/Climate/tmin_df.rda")

# ========================== Baustelle ----

# --- load, crop and project data ---- 

# Set the directory path where your .tif files are located
tmax.tif_folder <- "/Users/jonwent/polybox/Master_thesis/Climate_data/tmin"

# List all .tif files in the folder
tmax.tif_files <- list.files(tmax.tif_folder, pattern = "\\.tif$", full.names = TRUE)

tmax.raster <- raster::raster(tmax.tif_files[2])

tmax.raster.cropped <- raster::crop(tmax.raster, crop_extent)

mapview::mapview(tmax.raster)
mapview::mapview(tmax.raster.cropped)

crs_4326 <- CRS("+proj=longlat +datum=WGS84 +no_defs")

i = 1

# Loop through each TIFF file
for (tif_file in tmax.tif_files) {
  
  print(paste0("working on ", i, "/", length(tmax.tif_files), " ..."))
  
  # Load the .tif file
  tmax_raster <- raster(tif_file)
  
  # Crop to the specified extent
  tmax_cropped <- crop(tmax_raster, crop_extent)
  
  # Project the cropped raster
  tmax_projected <- projectRaster(tmax_cropped, crs = crs_4326)
  
  # save the raster layer
  # save the raster layer
  file_name <- basename(tif_file)  # Get the original file name
  file_name_without_extension <- tools::file_path_sans_ext(file_name)  # Remove extension
  processed_file_name <- paste0(file_name_without_extension, "_processed.tif")  # Add "_processed" to the name
  output_path <- file.path(tmax.tif_folder, processed_file_name)  # Create the output file path
  
  writeRaster(tmax_projected, filename = output_path, format = "GTiff")
  
  i = i + 1
}

rm(tmax_cropped, tmax_projected, tif_file, tmax.tif_files, tmax.tif_folder, tmax_raster, i, crs_4326)
rm(filen_name, file_name_without_extension, processed_file_name, output_path)


# ----- extract climatic data ---- 
# Set the directory path where your .tif files are located
swb.tif_folder <- "/Users/jonwent/polybox/Master_thesis/Climate_data/swb"

# List all .tif files in the folder
swb.tif_files <- list.files(swb.tif_folder, pattern = "\\_processed.tif$", full.names = TRUE)

swb.stack <- stack(swb.tif_files)

swb.list <- list()

for(i in 1:nlayers(swb.stack)){
  
  print(paste0("working on ", i, "/", nlayers(swb.stack), " ..."))
  
  swb.df.tmp <- tibble(partition = BBS_routes$partition,
                       swb.mean = exactextractr::exact_extract(swb.stack[[i]], BBS_routes, "mean",
                                                               progress = F),
                       swb.median = exactextractr::exact_extract(swb.stack[[i]], BBS_routes, "median",
                                                                 progress = F),
                       swb.sd = exactextractr::exact_extract(swb.stack[[i]], BBS_routes, "stdev",
                                                             progress = F))
  
  swb.list[[i]] <- swb.df.tmp
  
}

rm(swb.df.tmp, i)

# names(swb.list) <- tools::file_path_sans_ext(basename(swb.tif_files))
# names(swb.list) <- c(1996:2001, 2014:2018)

swb.df <- swb.list %>% 
  reshape2::melt(id.vars = c("partition", "swb.mean", "swb.median", "swb.sd"),
                 # variable.name = c("mean", "median", "sd"),
                 value.name = "value") %>%
  rename(mean = swb.mean,
         median = swb.median, 
         stdev = swb.sd,
         year = L1,
         segment = partition) %>%
  relocate(year, .before = "segment")


# ------ extraction function ------
# Set the directory path where your .tif files are located
tmax.tif_folder <- "/Users/jonwent/polybox/Master_thesis/Climate_data/tmax"

# List all .tif files in the folder
tmax.tif_files <- list.files(tmax.tif_folder, pattern = "\\_processed.tif$", full.names = TRUE)

tmax.stack <- stack(tmax.tif_files)

plot(tmax.stack[[1]])


extract_data <- function(data_folder, bbs_routes_data, num_cores = 6) {
  
  library(foreach)
  library(raster)
  library(tibble)
  library(dplyr)
  
  tif_files <- list.files(data_folder, pattern = "\\_processed.tif$", full.names = TRUE)
  raster.stack <- stack(tif_files)
  
  segments <- unique(bbs_routes_data$partition)
  
  raster_df_list <- list()
  
  # Initialize a cluster with the specified number of cores
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  for (layer_idx in 1:raster::nlayers(raster.stack)) {
    current_layer <- raster.stack[[layer_idx]]
    
    layer_stats_df <- data.frame(matrix(NA, nrow = 0, ncol = 4))
    colnames(layer_stats_df) <- c("segment", "mean", "median", "variance")
    
    foreach(s = 1:length(segments)) %dopar% {
      print(paste0("working on segment ", s, "/", length(segments), " in layer ",
                   layer_idx, "/", raster::nlayers(raster.stack), " ..."))
      
      segment.temp <- dplyr::filter(bbs_routes_data, partition == segments[s])
      extracted.data <- exactextractr::exact_extract(current_layer, segment.temp)
      temp.partition <- segment.temp$partition
      temp.shp <- tibble("partition" = temp.partition)
      
      for (i in seq_along(extracted.data)) {
        # entry <- extracted.data[[i]] %>% as.data.frame() %>% unlist()
        entry <- unlist(as.data.frame(extracted.data[[i]]))
        stats <- c(mean = mean(entry), median = median(entry), variance = var(entry))
        
        layer_stats_df <- rbind(layer_stats_df, c(temp.partition[i], stats))
      }
    }
    
    raster_df_list[[layer_idx]] <- layer_stats_df
  }
  
  stopCluster(cl)  # Stop the parallel cluster
  
  names(raster_df_list) <- names(raster.stack)
  
  raster_df_list_named <- lapply(raster_df_list, function(df) {
    colnames(df) <- c("segment", "mean", "median", "variance")
    df
  })
  
  raster_df <- reshape2::melt(raster_df_list_named, varnames = "segment")
  raster_df <- dplyr::rename(raster_df, year = L1)
  raster_df <- dplyr::relocate(year, .before = "segment")
  
  # raster_df$year <- as.integer(gsub("X", "", names(raster_df)[1:nlayers(raster.stack)]))
  # raster_df <- raster_df[, c("year", "segment", "variable", "value")]
  
  return(raster_df)
}


# n_cores <- parallel::detectCores()

# get segment
segments <- unique(BBS_routes$partition)

swb_df_list <- list()

for (layer_idx in 1:nlayers(swb.stack)) {
  
  current_layer <- swb.stack[[layer_idx]]
  
  layer_stats_df <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  colnames(layer_stats_df) <- c("Segment", "Mean", "Median", "Variance")
  
  for(s in 1:length(segments)) {
    
    print(paste0("working on segment ", s, "/",length(segments)," in layer ",
                 layer_idx, "/", nlayers(swb.stack), " ..."))
    
    segment.temp <- BBS_routes %>% filter(partition == segments[s])
    
    extracted.swb <- exactextractr::exact_extract(current_layer, segment.temp)
    temp.partition <- segment.temp$partition
    
    temp.shp <- tibble("partition" = temp.partition)
    
    # Calculate statistics for each entry in extracted.tmax and store them
    for (i in seq_along(extracted.swb)) {
      
      entry <- extracted.swb[[i]] %>% as.data.frame() %>% unlist()
      
      stats <- c(mean = mean(entry), median = median(entry), variance = var(entry))
      
      layer_stats_df <- rbind(layer_stats_df, c(temp.partition[i], stats))
    }
  }
  
  # Store the data frame with statistics for the current layer
  swb_df_list[[layer_idx]] <- layer_stats_df
}

names(swb_df_list) <- c(1996:2001, 2014:2018)

swb_df_list_named <- lapply(swb_df_list, function(df) {
  colnames(df) <- c("segment", "mean", "median", "variance")
  df
})

swb_df <- reshape2::melt(swb_df_list_named, varnames = "segment")

swb_df <- swb_df %>%
  rename(year = L1) %>%
  relocate(year, .before = "segment")
