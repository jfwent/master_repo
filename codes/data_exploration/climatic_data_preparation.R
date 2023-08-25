# Download and prepare climatic data
# Author: Jon Went, jwent@ethz.ch
# Date: 24.08.2023

# ---- libraries ----

rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

# ---- load percipitation data ----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/"

# Define the years and months to download data
years <- c(2001, 2019)
years <- 2018
months <- 01:12

# Initialize an empty raster stack
stack_list <- list()
stack_list2 <- list()

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
    stack_list2[[length(stack_list) + 1]] <- raster_data
  }
}

# Create a raster stack from the list of rasters
pr_stack <- stack(stack_list)
pr_2018_stack <- stack(stack_list2)
