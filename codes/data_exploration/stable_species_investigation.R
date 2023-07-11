# investigate the stable species
# Author: Jon Went, jwent@ethz.ch
# Date: 06.06.2023

# load libraries ------
library(vegan)
library(dplyr)
library(tidyverse)

# load data ------
rm(list=ls())

#stable species matrix
load("data/stable_species_mat.rda")
species_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)

load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

segments_all <- rownames(species_mat)
ecoregion_df <- land[which(land$segment %in% segments_all),]
ecoregion_df <- subset(ecoregion_df, year == 2001)
ecoregion_df <- ecoregion_df %>% select(ecoregion, cluster_nr, cluster, segment)
ecoregion_names <- unique(ecoregion_df$ecoregion)
cluster_names <- unique(ecoregion_df$cluster)

load("data/Lica/BBS_partition_abundance.rda")

# --- calculate the alpha diversity indices for stable species for average abundance of years 2000, 2001, 2002
stable_species <- colnames(species_mat)
first_years <- 2000:2002

BBS_sub_first <- BBS_partition_abundance[BBS_partition_abundance$animal_jetz %in% stable_species, ]
BBS_sub_first <- BBS_sub_first[BBS_sub_first$year %in% first_years,]
BBS_sub_first <- BBS_sub_first[BBS_sub_first$partition %in% segments_all,]

land_sub <- land[land$segment %in% segments_all, ]
land_sub <- land_sub[land_sub$year == 2001,]
land_sub <- land_sub %>%
  select(c(segment, ecoregion, cluster))
BBS_sub_first <- merge(BBS_sub_first, land_sub, by.x = "partition", by.y = "segment")

averaged_df <- BBS_sub_first %>%
  filter(year %in% first_years) %>%
  group_by(animal_jetz, partition) %>%
  summarize(avg_seg_abundance = mean(seg_abundance),
            sd_seg_abundance = sd(seg_abundance)) %>%
  filter(avg_seg_abundance > 0)

av_merged <- merge(averaged_df, land_sub, by.x = "partition", by.y = "segment")

seg_first <- unique(averaged_df$partition)
animal_first <- unique(averaged_df$animal_jetz)

library(reshape2)

# Assuming your matrix is named 'abundance_matrix'
long_df <- melt(species_mat, varnames = c("segment", "animal_jetz"), value.name = "avg_abundance")

long_df_sub <- long_df %>%
  filter(avg_abundance > 0)

long_merged <- merge(long_df_sub, land_sub, by = "segment")

stable_birds_long <- unique(long_df_sub$animal_jetz)
stable_seg_long <- unique(long_df_sub$segment)

# ----- reinvestigate the stable species -----
first_years <- 2000:2002

BBS_stable <- BBS_partition_abundance %>%
  filter(year %in% first_years) %>%
  group_by(animal_jetz, partition) %>%
  summarize(avg_seg_abundance = if (all(subset(seg_abundance, year %in% first_years) > 0)) mean(seg_abundance) else NA,
            sd_seg_abundance = if (all(subset(seg_abundance, year %in% first_years) > 0)) sd(seg_abundance) else NA) %>%
  filter(!is.na(avg_seg_abundance))

BBS_stable_2 <- BBS_partition_abundance %>%
  filter(year %in% first_years) %>%
  group_by(animal_jetz, partition) %>%
  summarize(avg_seg_abundance = if (all(seg_abundance > 0)) mean(seg_abundance) else NA,
            sd_seg_abundance = if (all(seg_abundance > 0)) sd(seg_abundance) else NA) %>%
  filter(!is.na(avg_seg_abundance))

BBS_stable_merged <- merge(BBS_stable, land_sub, by.x = "partition", by.y = "segment")

stable_birds <- unique(BBS_stable$animal_jetz)
seg_stable_control <- unique(BBS_stable$partition)

all_birds <- unique(BBS_partition_abundance$animal_jetz)

rm(list=ls())

