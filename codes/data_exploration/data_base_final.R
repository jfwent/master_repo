# create data base for subsequent modelling at segment level
# Author: Jon Went
# Date: 27.07.2023

rm(list=ls())

# ----- libraries ----

library(tidyverse)

#---- load BBS data ----

load("data/Lica/BBS_partition_abundance.rda")
load("data/Lica/BBS_partition_abundance_2021.rda")

# ---- load land use data ----

load("data/land_use_pxsum.rda")

load("data/land_use_area.rda")

load("data/Lica/BBS_land_years.rda")
land_use_pct <- BBS_land_years %>%
  rename(segment = partition)

# ---- load ecoregion file ----

eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition)

# ---- load Bird et al., 2020 Cons. Biol data ----

load("data/Bird_full_df.rda")

# ---- join BBS data frames ----

bbs2021 <- bbs2021 %>%
  select(-segment)

BBS_full <- BBS_partition_abundance %>%
  bind_rows(bbs2021) %>%
  select(-c(3:8)) %>%
  relocate(partition, .before = AOU) %>%
  rename(segment = partition)

# --- add ecoregions ----

BBS_full_eco <- BBS_full %>%
  left_join(eco_txt, by = "segment") %>%
  rename(ecoregion = Ecoregion) %>%
  relocate(ecoregion, .after = segment)

# ---- add Bird et al data ---- 

BBS_full_eco_bird <- BBS_full_eco %>%
  left_join(Bird_full_df, by = "animal_jetz")

# ---- add land use data ----

BBS_final_pct <- BBS_full_eco_bird %>%
  left_join(land_use_pct, by = c("segment", "year"))

BBS_final_area <- BBS_full_eco_bird %>%
  left_join(land_use_area, by = c("segment", "year", "ecoregion"))

BBS_final_pxsum <- BBS_full_eco_bird %>%
  left_join(land_use_pxsum_eco, by = c("segment", "year", "ecoregion"))

# --- save data sets ----

save(BBS_final_area, file = "data/database/BBS_final_area.rda")
save(BBS_final_pxsum, file = "data/database/BBS_final_pxsum.rda")
save(BBS_final_pct, file="data/database/BBS_final_pct.rda")
