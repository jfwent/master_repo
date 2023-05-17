rm(list=ls())

## Set up

library(tibble); library(purrr); library(dplyr); library(tidyr)
# library(brms)

### Data

#  Data: Load, Clean up, Merge --------------------------------------------

# Load and convert to tibble for easier management
load('data/Lica/bbs_land_years.rda')
land <- as_tibble(bbs_land_years); rm(bbs_land_years)
ecoreg <- as_tibble(read.delim('data/Lica/US_ecoregions.txt', sep = ""))
load('data/Lica/BBS_partition_abundance.rda')
abund <- as_tibble(BBS_partition_abundance); rm(BBS_partition_abundance)

## landuse data clean up
land$percent <- rowSums(land[, 4:11]) # calculate sum percentage of specified land use
land_sub <- land[land$percent >= 60,] # if less than 60% of land use per segment are characterized in total, discard entry
# 154 entries don't fulfill requirements and are discarded

## abundance clean up
#check for multiple entries for each species+year+partition combination and discard the ones with multiple entries

dups <- duplicated(abund[, c("animal_jetz", "year", "partition")]) | 
  duplicated(abund[, c("animal_jetz", "year", "partition")], fromLast = TRUE)
abund_sub <- abund[!dups,] # 7701 entries were duplicated and discarded

## get missing data from 2020-2021


# save cleaned data
save(land_sub, file = "landuse_subset.rda")
save(abund_sub, file = "abundance_subset.rda")
