# calculate the alpha diversity per cluster
# Author: Jon Went, jwent@ethz.ch
# Date: 06.06.2023

# load libraries ------
library(vegan)
library(dplyr)
library(tidyverse)

# load data ------
rm(list=ls())
load("data/stable_species_mat.rda")
species_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)

load("data/abund_mat_list.rda")
abund_list <- res_abund; rm(res_abund)

source("codes/functions/sort_matrix.R")
abund_list_sorted <- lapply(abund_list, sort_matrix)

abund_2000 <- abund_list_sorted[[1]]

load("data/abund_clusters_mat.rda")
# this is the abundance matrix, averaged for species present across years 2000-2002
# with clusters as rows and species as columns

load("data/beta_div_clusters_df.rda")
# this are the diversity matrices calculated for the stable species averaged across 2000-2002
# calculated for segments within clusters

load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

segments_all <- rownames(species_mat)
ecoregion_df <- land[which(land$segment %in% segments_all),]
ecoregion_df <- subset(ecoregion_df, year == 2001)
ecoregion_df <- ecoregion_df %>% select(ecoregion, cluster_nr, cluster, segment)
ecoregion_names <- unique(ecoregion_df$ecoregion)
cluster_names <- unique(ecoregion_df$cluster)

load("data/Lica/BBS_partition_abundance.rda")

load("data/Lica/BBS_land_years.rda")
ecoregions_txt <- read.table("data/Lica/US_ecoregions.txt", header = T)

# get land use data ---- 

land_sub <- BBS_land_years %>%
  #filter(year %in% c(2000:2002)) %>% # only land use data is available for 2001
  arrange(partition) %>%
  rename(segment = partition) %>%
  mutate(urban = urban.low+urban.high,
         other = 100 - (urban.high+urban.low+forest+grass+wet+barren+pasture+crop)) %>%
  select(-urban.low, -urban.high)

land_sub_clust <- land %>%
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
save(land_complete, file= "data/land_use_clustered_complete_df.rda")
rm(missing_segs, missing_segs_merged, land_sub, land_sub_clust, matches, land_merged)

# calculate the alpha diversity for the stable species first three years ------

# melt matrix into long df
stable_df <- reshape2::melt(species_mat, varnames = c("segment", "animal_jetz"), value.name = "avg_abundance")

#filter all segments with avg_abundance == 0
stable_df <- stable_df %>%
  filter(avg_abundance > 0)

stable_full_df <- left_join(stable_df, land_complete[land_complete$year == 2001,], by = "segment")

stable_full_df %>%
  filter(cluster %in% "Southern_and_Baja_California_Pine-Oak_Mountains_1")

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
            n_segments = length(unique(segment)))

BBS_stable_alpha_ecoregion <- stable_full_df %>%
  filter(!is.na(cluster)) %>%
  group_by(ecoregion) %>%
  summarize(richness = specnumber(avg_abundance),
            shannon = diversity(avg_abundance, index = "shannon"),
            simpson = diversity(avg_abundance, index = "simpson"),
            invsimpson = diversity(avg_abundance, index = "invsimpson"),
            n_birds = sum(avg_abundance),
            n_segments = length(unique(segment)))


# Calculate the diversity matrices for the stable species for segments within clusters for years 2003-2019----

stable_species <- colnames(species_mat)
years <- 2003:2019

BBS_sub <- BBS_partition_abundance %>%
  filter(animal_jetz %in% stable_species,
         year %in% years,
         partition %in% segments_all)

BBS_sub_full <- BBS_sub %>%
  rename(segment = partition,
         route = U_S_R_I) %>%
  left_join(land_complete[land_complete$year %in% 2003:2019,],
                          by = c("segment", "year", "route"))

# ---- for-loop here ----

# prepare storage
alpha_div_clusters <- list()
alpha_div_clusters_test <- list()
alpha_div_segments <- list()
alpha_div_ecoregions <- list()

for(i in seq_along(years)){
  year.now <- years[i]
  BBS.now <- BBS_sub_full %>%
    filter(year == year.now)
  
  alpha_div_clusters[[i]] <- BBS.now %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster) %>%
    summarize(richness = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n_birds = sum(seg_abundance),
              n_segments = length(unique(segment))) %>%
    pivot_longer(cols = c(richness, shannon, invsimpson, simpson),
                 names_to = "metric")
  
  alpha_div_segments[[i]] <- BBS.now %>%
    group_by(segment) %>%
    summarize(richness = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n_birds = sum(seg_abundance)) %>%
    pivot_longer(cols = c(richness, shannon, invsimpson, simpson),
                 names_to = "metric")
  
  alpha_div_ecoregions[[i]] <- BBS.now %>%
    filter(!is.na(ecoregion)) %>%
    group_by(ecoregion) %>%
    summarize(richness = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n_birds = sum(seg_abundance),
              n_segments = length(unique(segment))) %>%
    pivot_longer(cols = c(richness, shannon, invsimpson, simpson),
                 names_to = "metric")
}

# rename
names(alpha_div_clusters) <- years
names(alpha_div_segments) <- years
names(alpha_div_ecoregions) <- years

# have a look at one entry to check if everything is as expected
alpha_div_clusters_2004 <- alpha_div_clusters[[2]]

#---- convert lists to data frames

alpha_div_cluster_df <- bind_rows(lapply(names(alpha_div_clusters), function(entry_name) {
  data <- alpha_div_clusters[[entry_name]]
  data$year <- entry_name  # Add the name column to the data frame
  data <- pivot_wider(data = data, names_from = metric, values_from = value)
  return(data)
}))

alpha_div_segments_df <- bind_rows(lapply(names(alpha_div_segments), function(entry_name) {
  data <- alpha_div_segments[[entry_name]]
  data$year <- entry_name  # Add the name column to the data frame
  data <- pivot_wider(data = data, names_from = metric, values_from = value)
  return(data)
}))

alpha_div_ecoregion_df <- bind_rows(lapply(names(alpha_div_ecoregions), function(entry_name) {
  data <- alpha_div_ecoregions[[entry_name]]
  data$year <- entry_name  # Add the name column to the data frame
  data <- pivot_wider(data = data, names_from = metric, values_from = value)
  return(data)
}))

# save the data ------

save(BBS_stable_alpha_cluster, file = "data/alpha_div_cluster_stable.rda")
save(BBS_stable_alpha_seg, file = "data/alpha_div_seg_stable.rda")
save(BBS_stable_alpha_ecoregion, file = "data/alpha_div_ecoregion_stable.rda")

save(alpha_div_cluster_df, file = "data/alpha_div_cluster_df.rda")
save(alpha_div_ecoregion_df, file = "data/alpha_div_ecoregion_df.rda")
save(alpha_div_segments_df, file = "data/alpha_div_segments_df.rda")