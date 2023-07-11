#Alpha diversity calculations + data base creation
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
ecoregions_txt <- read.table("data/Lica/US_ecoregions.txt")

BBS_land_txt <- read.table("/Users/jonwent/polybox/Master thesis/BBS.land.years.txt")

#bird data
load("data/Lica/BBS_partition_abundance.rda")
BBS_df <- BBS_partition_abundance; rm(BBS_partition_abundance)

BBS_df <- BBS_df %>%
  rename(segment = partition)

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
sum(matches == FALSE)

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

BBS_stable <- BBS_df %>%
  filter(year %in% c(2000:2002)) %>%
  group_by(animal_jetz, segment) %>%
  summarize(avg_seg_abundance = if (all(seg_abundance > 0)) mean(seg_abundance) else NA,
            sd_seg_abundance = if (all(seg_abundance > 0)) sd(seg_abundance) else NA) %>%
  filter(!is.na(avg_seg_abundance))

BBS_stable_full <- left_join(BBS_stable, land_complete[land_complete$year == 2001,], by = "segment")

sum(is.na(BBS_stable_full$cluster))

seg_land_first <- unique(BBS_land_txt[BBS_land_txt$year == 2001,]$partition)
seg_BBS_first <- unique(BBS_df[BBS_df$year %in% 2001,]$segment)

# ------ calculate alpha diversity -----

BBS_stable_alpha_seg <- BBS_stable %>%
  group_by(partition) %>%
  summarize(richness = specnumber(avg_seg_abundance),
            shannon = diversity(avg_seg_abundance, index = "shannon"),
            simpson = diversity(avg_seg_abundance, index = "simpson"),
            invsimpson = 1/simpson)

BBS_stable_alpha_clust <- BBS_stable_full %>%
  group_by(cluster) %>%
  summarize(richness = specnumber(avg_seg_abundance),
            shannon = diversity(avg_seg_abundance, index = "shannon"),
            simpson = diversity(avg_seg_abundance, index = "simpson"),
            invsimpson = 1/simpson)

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

