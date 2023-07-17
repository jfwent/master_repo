#Alpha diversity calculations years 2000-2002
#Author: Jon Went, jwent@ethz.ch
#Date: 11.06.2023

#----- load libraries ---- 

library(tidyverse)
library(vegan)

# ----- load data ---- 
rm(list=ls())

#land use data
load("data/land_use_clustered.rda")
land_clustered <- combined_df; rm(combined_df)

load("data/Lica/BBS_land_years.rda")
ecoregions_txt <- read.table("data/Lica/US_ecoregions.txt", header = T)

#bird data
load("data/Lica/BBS_partition_abundance.rda")
BBS_df <- BBS_partition_abundance; rm(BBS_partition_abundance)

BBS_df <- BBS_df %>%
  rename(segment = partition)

# stable species matrix
load("data/stable_species_mat.rda")
species_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)


# ----- prepare land use data  ---- 

land_sub <- BBS_land_years %>%
  #filter(year %in% c(2000:2002)) %>% # only land use data is available for 2001
  arrange(partition) %>%
  rename(segment = partition) %>%
  mutate(urban = urban.low+urban.high,
         other = 100 - (urban.high+urban.low+forest+grass+wet+barren+pasture+crop)) %>%
  select(-urban.low, -urban.high)

land_sub_clust <- land_clustered %>%
  #filter(year %in% c(2000:2002)) %>%
  arrange(segment) %>%
  select(segment, cluster, cluster_nr, ecoregion, year)

land_merged <- merge(land_sub, land_sub_clust, by= c("segment", "year"))

matches <- land_sub$segment %in% land_sub_clust$segment
sum(matches == FALSE) # 48 segments missing across 8 years = 6 segments/year

missing_segs <- subset(land_sub, !matches)
# There are fewer segments in the clustered dataframe bc only ecoregions with more than one segment could be clustered
# add cluster 1 for ecoregion-segments where only 1 segment in ecoregion

missing_segs_merged <- merge(missing_segs, ecoregions_txt, by.x = "segment", by.y = "partition")

missing_segs_merged <- missing_segs_merged %>%
  rename(ecoregion = Ecoregion) %>%
  mutate(cluster_nr = 1) %>%
  unite("cluster", ecoregion, cluster_nr, remove = F) %>%
  select(-FID, -Kilometers, -route) %>%
  relocate(c(urban, other), .before = cluster)

land_complete <- bind_rows(land_merged, missing_segs_merged)
rm(missing_segs, missing_segs_merged, land_sub, land_sub_clust, matches, land_merged, land_clustered)

#---- add land use data to BBS data ----

full_df <- left_join(BBS_df, land_complete, by = c("segment", "year"))

# ------ get species+segments with abundance > 0 for all years 2000-2001-2002 ----

# melt matrix into long df
stable_df <- reshape2::melt(species_mat, varnames = c("segment", "animal_jetz"), value.name = "avg_abundance")

#filter all segments with avg_abundance == 0
stable_df <- stable_df %>%
  filter(avg_abundance > 0)

# ------ calculate alpha diversity first three years -----

stable_full_df <- left_join(stable_df, land_complete[land_complete$year == 2001,], by = "segment")

BBS_stable_alpha_seg <- stable_df %>%
  group_by(segment) %>%
  summarize(richness = specnumber(avg_abundance),
            shannon = diversity(avg_abundance, index = "shannon"),
            simpson = diversity(avg_abundance, index = "simpson"),
            invsimpson = diversity(avg_abundance, index = "invsimpson"),
            n_birds = sum(avg_abundance))

BBS_stable_alpha_cluster <- stable_full_df %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarize(richness = specnumber(avg_abundance),
            shannon = diversity(avg_abundance, index = "shannon"),
            simpson = diversity(avg_abundance, index = "simpson"),
            invsimpson = diversity(avg_abundance, index = "invsimpson"),
            n_birds = sum(avg_abundance),
            n_segments = length(segment))

length(unique(land_clustered[land_clustered$year == 2001,]$cluster))

#---- merge land + BBS data sets for years 2000:2002

land_first <- land_complete %>%
  filter(year %in% 2000:2002)

full_stable_segments_df <- merge(BBS_stable_alpha_seg, land_first, by.x = "partition", by.y = "segment")

# ----- get stable species -----

stable_species <- unique(BBS_stable$animal_jetz)

# ---- 

# ---- get land use data for years 2003:2019 -----

land_later <- land_complete %>%
  filter(year %in% 2003:2019)

