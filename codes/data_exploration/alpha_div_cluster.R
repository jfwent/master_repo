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

# code here ------

# Calculate the diversity matrices for the stable species for segments within clusters for years 2003-2019

stable_species <- colnames(species_mat)
years <- 2003:2019

BBS_sub <- BBS_partition_abundance[BBS_partition_abundance$animal_jetz %in% stable_species, ]
BBS_sub <- BBS_sub[BBS_sub$year %in% years,]
BBS_sub <- BBS_sub[BBS_sub$partition %in% segments_all,]

land_sub <- land[land$segment %in% segments_all, ]
land_sub <- land_sub[land_sub$year == 2001,]
land_sub <- land_sub %>%
  select(c(segment, ecoregion, cluster))

BBS_sub_full <- merge(BBS_sub, land_sub, by.x = "partition", by.y = "segment")

# ---- 

# prepare storage
alpha_div_clusters <- list()
alpha_div_segments <- list()
alpha_div_ecoregions <- list()

for(i in seq_along(years)){
  year.now <- years[i]
  BBS.now <- BBS_sub_full[BBS_sub_full$year == year.now,]
  
  alpha_div_clusters[[i]] <- BBS.now %>%
  group_by(cluster) %>%
    summarize(sobs = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n = sum(seg_abundance)) %>%
    pivot_longer(cols = c(sobs, shannon, invsimpson, simpson),
                 names_to = "metric")
  
  alpha_div_segments[[i]] <- BBS.now %>%
    group_by(partition) %>%
    summarize(sobs = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n = sum(seg_abundance)) %>%
    pivot_longer(cols = c(sobs, shannon, invsimpson, simpson),
                 names_to = "metric")
  
  alpha_div_ecoregions[[i]] <- BBS.now %>%
    group_by(ecoregion) %>%
    summarize(sobs = specnumber(seg_abundance),
              shannon = diversity(seg_abundance, index = "shannon"),
              simpson = diversity(seg_abundance, index = "simpson"),
              invsimpson = diversity(seg_abundance, index = "invsimpson"),
              n = sum(seg_abundance)) %>%
    pivot_longer(cols = c(sobs, shannon, invsimpson, simpson),
                 names_to = "metric")
}

# rename
names(alpha_div_clusters) <- years
names(alpha_div_segments) <- years
names(alpha_div_ecoregions) <- years

# have a look at one entry to check if everything is as expected
alpha_div_clusters_2003 <- alpha_div_clusters[[1]]

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

save(alpha_div_cluster_df, file = "data/alpha_div_cluster_df.rda")
save(alpha_div_ecoregion_df, file = "data/alpha_div_ecoregion_df.rda")
save(alpha_div_segments_df, file = "data/alpha_div_segments_df.rda")