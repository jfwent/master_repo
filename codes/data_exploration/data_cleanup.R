rm(list=ls())

## Set up

library(tibble); library(purrr); library(dplyr); library(tidyr)
# library(brms)

### Data

#  Data: Load, Clean up, Merge --------------------------------------------

bbs_filt_AOU <- read.delim("/Users/jonwent/polybox/Master thesis/bbs.filt.AOU.txt", sep="")
save(bbs_filt_AOU, file = "bbs_filt_AOU.rda")
BBS_partition_abundance <- read.delim("/Users/jonwent/polybox/Master thesis/BBS.partition.abundance.txt", sep="")
save(BBS_partition_abundance, file = "BBS_partition_abundance.rda")
BBS_land_years <- read.delim("/Users/jonwent/polybox/Master thesis/BBS.land.years.txt", sep="")
save(BBS_land_years, file = "BBS_land_years.rda")

# Load and convert to tibble for easier management
load('data/Lica/BBS_land_years.rda')
land <- as_tibble(BBS_land_years); rm(bbs_land_years)
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
