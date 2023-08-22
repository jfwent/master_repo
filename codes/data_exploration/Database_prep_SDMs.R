# Build data sets to use in the SDM approach
# Author : Jon Went
# Date: 02.08.2023

library(tidyverse)

# --- load data ----

rm(list=ls())

load("data/Lica/BBS_partition_abundance.rda")
load("data/Lica/BBS_partition_abundance_2021.rda")
load("data/land_use_pxsum_clustered.rda")
load("data/Bird_full_df.rda")
eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition,
         ecoregion = Ecoregion)

# load("/Users/jonwent/Desktop/ETHZ/master_thesis/Climate_data/merged_data_2001_2019_names.rda") corrupted file
# anyway the resolution of the data is (4x4)km so not useful for us.

bbs2021 <- bbs2021 %>%
  select(-segment) %>%
  rename(segment = partition)

BBS_df <- BBS_partition_abundance %>%
  rename(segment = partition) %>%
  bind_rows(bbs2021) %>%
  left_join(eco_txt, by = "segment") %>%
  left_join(Bird_full_df, by = "animal_jetz") %>%
  left_join(land_use_pxsum_complete, by = c("ecoregion","segment", "year"))

rm(BBS_partition_abundance, bbs2021, Bird_full_df, land_use_pxsum_complete, eco_txt)



#---- Climatic data: Forget about it for now. We would have (1x1)km resolution data for years 1979-2013



# --- presence-absence column ----

BBS_pa <- BBS_df %>%
  mutate(PA = ifelse(seg_abundance != 0, 1, 0)) %>%
  group_by(segment, animal_jetz) %>%
  filter(year %in% c(2000:2002)) %>%
  summarize(PA_tot = sum(PA)) %>%
  mutate(PA_stable = ifelse(all(PA_tot != 1), 
                            0,
                            1)) %>%
  ungroup()


stable_birds <- BBS_df %>%
  filter(year %in% c(2000:2002)) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(avg_abundance = mean(seg_abund)) %>%
  mutate(avg_abund = round(avg_abundance,1)) %>%
  dplyr::select(-avg_abundance)

BBS_pa %>%
  filter(
         animal_jetz == "Agelaius_phoeniceus",
         segment == "02_001_1"
         ) %>%
  arrange(segment, animal_jetz, year) %>%
  dplyr::select(year, animal_jetz, seg_abundance, segment, PA_stable)

