##### Legacy models exploration
##########################################


### The goal of this is to explore different set-ups of legacy models, according to Bird diversity~Land cover
### I want to compare 2 models to each other:
#1. Bird diversity in 2019 explained by land cover in 2019. This model is according to no legacy effects or rapid change.
#2. Bird diversity in 2019 explained by past land cover (for example 2001). This model is according to legacy effects.
#3. If time permits I want to build models of each year's bird diversity data with the corresponding land cover data (2001-2019),
# and predict to 2021 to see how the prediction strength of the models changes.


###===================
# Load the data
library(dplyr); library(tidyr)
rm(list=ls())
load("data/Lica/BBS_land_years.rda")
load("data/Lica/BBS_partition_abundance.rda")
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

land <- BBS_land_years; #rm(BBS_land_years)
abund <- BBS_partition_abundance; rm(BBS_partition_abundance)

land <- land %>%
  rowwise() %>%
  mutate(urban = sum(urban.low, urban.high)) %>%
  select(-urban.low, -urban.high, -route)

land_eco <- merge(land, ecoreg, by="partition", all.x = T, all.y = F)

land <- land_eco %>%
  select(-FID, -Kilometers)

land <- land %>%
  group_nest(year)

rm(land_eco)
