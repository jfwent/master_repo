# Download climatic data
# Author: Jon Went, jwent@ethz.ch
# Date: 24.08.2023

# ---- libraries ----

rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

# ---- download percipitation data ----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/"

# Define the years and months to download data
# for all years we have land use data for
# no percipitation data for months 07-12 in 2019!
# ---> only use wettest month?
  # which would be dependent on the location but generally either in fall/winter (nov-feb) for the northwest
  # and spring-summer for the southeastern parts

years <- c(2001, 2018)
months <- 01:12

# Initialize an empty raster stack
stack_list <- list()

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url <- paste0(base_url, "CHELSA_pr_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_pr_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Download and save the file
    download.file(url, destfile = filename, mode = "wb")
    
    # Load the downloaded raster data
    raster_data <- raster(filename)
    
    # Append the raster to the list
    stack_list[[length(stack_list) + 1]] <- raster_data
  }
}

# ----- download monthly maximum temperature data for hottest months (may-august) ----

# do we take the average for all the years we have averaged the bird data for?

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/"

# define years and months of interest
years <- c(2001, 2019)
months <- 05:08

# Initialize an empty raster stack
tasmax_stack_list <- list()

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url <- paste0(base_url, "CHELSA_tasmax_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_tasmax_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Download and save the file
    download.file(url, destfile = filename, mode = "wb")
    
    # Load the downloaded raster data
    raster_data <- raster(filename)
    
    # Append the raster to the list
    tasmax_stack_list[[length(tasmax_stack_list) + 1]] <- raster_data
  }
}

# Create a raster stack from the list of rasters
tasmax.stack.t1 <- stack(tasmax_stack_list[1:3])
tasmax.stack.t2 <- stack(tasmax_stack_list[4:6])

# transform to same crs as BBS data (EPSG 5070)
crs(tasmax.stack.t1) <- "+init=epsg:5070"
crs(tasmax.stack.t2) <- "+init=epsg:5070"

# ------ download monthly minimum temperature data for coldest months (december - february) -----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmin/"

years <- c(2001, 2019)
months <- c(1,2,12)

# Initialize an empty raster stack
tasmin_stack_list <- list()

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url <- paste0(base_url, "CHELSA_tasmin_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_tasmin_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Download and save the file
    download.file(url, destfile = filename, mode = "wb")
    
    # Load the downloaded raster data
    raster_data <- raster(filename)
    
    # Append the raster to the list
    tasmin_stack_list[[length(tasmin_stack_list) + 1]] <- raster_data
  }
}

# Create a raster stack from the list of rasters
tasmin.stack.t1 <- stack(tasmin_stack_list[1:3])
tasmin.stack.t2 <- stack(tasmin_stack_list[4:6])

# transform to same crs as BBS data (EPSG 5070)
crs(tasmin.stack.t1) <- "+init=epsg:5070"
crs(tasmin.stack.t2) <- "+init=epsg:5070"


