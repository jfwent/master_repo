rm(list=ls())

## Set up

library(tibble); library(purrr); library(dplyr); library(tidyr)
# library(brms)

### Data

#  Data: Load, Clean up, Merge --------------------------------------------

# Load and convert to tibble for easier management
landuse <- as_tibble(read.delim('data/Lica/bbs.land.years.txt', sep = ""))
ecoregions <- as_tibble(read.delim('data/Lica/US_ecoregions.txt', sep = ""))
abundance <- as_tibble(read.delim('data/Lica/BBS.partition.abundance.txt', sep = ""))

# set up all unique species, land cover labels, years, segments, IDs, and species names (according to Jetz)
# we will need them for the clean up

unique_segments_landuse <- unique(landuse$partition)
unique_segments_abund <- unique(abundance$partition)
years_landuse <- unique(landuse$year)
years_abund <- unique(abundance$year)
unique_sp <- unique(abundance$animal_jetz)


## landuse data clean up
landuse$percentage <- rowSums(landuse[, 4:11]) # calculate sum percentage of specified land use
landuse_subset <- landuse[landuse$percentage >= 60,] # if less than 60% of land use per segment are characterized in total, discard entry

## abundance clean up
#check for double entries per unique_ID per species



# merge data sets
df_merged <- merge(landuse, abundance, by="unique_ID")