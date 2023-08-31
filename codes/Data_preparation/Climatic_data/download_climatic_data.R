# Download climatic data
# Author: Jon Went, jwent@ethz.ch
# Date: 24.08.2023

# ----- download monthly maximum temperature data for hottest months (june-august) ----

# Define the base URL for CHELSA tmax data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/"

# define years and months of interest
years <- c(1996:2001, 2014:2019)
months <- 06:08

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url.tmp <- paste0(base_url, "CHELSA_tasmax_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_tasmax_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Set the desired directory path
    download_directory <- "/Users/jonwent/polybox/Master_thesis/Climate_data/tmax"
    
    # Combine the directory path and filename
    full_path <- file.path(download_directory, filename)
    
    # Download and save the file
    download.file(url.tmp, destfile = full_path, mode = "wb")
  }
}

# ------ download monthly minimum temperature data for coldest months (december - february) -----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmin/"

years <- c(1996:2001, 2014:2019)
months <- c(1:2,12)

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url.tmp <- paste0(base_url, "CHELSA_tasmin_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_tasmin_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Set the desired directory path
    download_directory <- "/Users/jonwent/polybox/Master_thesis/Climate_data/tmin"
    
    # Combine the directory path and filename
    full_path <- file.path(download_directory, filename)
    
    # Download and save the file
    download.file(url.tmp, destfile = full_path, mode = "wb")
  }
}

# ------ download annual soil water balance data -----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/annual/swb/"

years <- c(1996:2001, 2014:2019)

# Loop through the years and months and download data
for (year in years) {
  
  # Create the full URL for the current year and month
  url.tmp <- paste0(base_url, "CHELSA_swb_", year, "_V.2.1.tif")
  
  # Define the filename to save the downloaded file
  filename <- paste0("CHELSA_swb_", year, "_V.2.1.tif")
  
  # Set the desired directory path
  download_directory <- "/Users/jonwent/polybox/Master_thesis/Climate_data/swb"
  
  # Combine the directory path and filename
  full_path <- file.path(download_directory, filename)
  
  # Download and save the file
  download.file(url.tmp, destfile = full_path, mode = "wb")
  
}

# ------ download monthly climate moisture index data -----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/cmi/"

years <- c(1996:2001, 2014:2019)
months <- c(1:12)

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    # Create the full URL for the current year and month
    url.tmp <- paste0(base_url, "CHELSA_cmi_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_cmi_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Set the desired directory path
    download_directory <- "/Users/jonwent/polybox/Master_thesis/Climate_data/cmi"
    
    # Combine the directory path and filename
    full_path <- file.path(download_directory, filename)
    
    # Download and save the file
    download.file(url.tmp, destfile = full_path, mode = "wb")
  }
}

# ---- download precipitation data ----

# Define the base URL for CHELSA pr data
base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/"

years <- c(1996:2001, 2014:2019)
months <- 01:12

# Set the desired timeout value in seconds (e.g., 300 seconds)
timeout_value <- 300

# Loop through the years and months and download data
for (year in years) {
  for (month in months) {
    
    # Create the full URL for the current year and month
    url.tmp <- paste0(base_url, "CHELSA_pr_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Define the filename to save the downloaded file
    filename <- paste0("CHELSA_pr_", sprintf("%02d", month), "_", year, "_V.2.1.tif")
    
    # Set the desired directory path
    download_directory <- "/Users/jonwent/polybox/Master_thesis/Climate_data/precipitation"
    
    # Combine the directory path and filename
    full_path <- file.path(download_directory, filename)
    
    # Download and save the file
    download.file(url.tmp, destfile = full_path, mode = "wb", timeout = timeout_value)
  }
}
