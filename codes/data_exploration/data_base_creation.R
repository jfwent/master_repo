# create base data frame for all subsequent modelling
# Author: Jon Went, jwent@ethz.ch
# Date: 07.06.2023

#--------- load libraries -------
rm(list=ls())

# ---- load land use data ----
load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

#--------- cluster level data base -----
# load data 
load("data/alpha_div_cluster_df.rda")

alpha_div_cluster_sub <- alpha_div_cluster_df %>%
  filter(year >= 2004) %>%
  rename(richness = sobs, sum_abundance = n)

# summarize land use data
land_cluster <- land %>%
  group_by(year, cluster) %>%
  summarize(
    mean_urban = mean(urban),
    sd_urban = sd(urban),
    mean_barren = mean(barren),
    sd_barren = sd(barren),
    mean_wet = mean(wet),
    sd_wet = sd(wet),
    mean_pasture = mean(pasture),
    sd_pasture = sd(pasture),
    mean_grass = mean(grass),
    sd_grass = sd(grass),
    mean_other = mean(other),
    sd_other = sd(other)
  ) %>%
  filter(year >= 2003 & year <= 2019)

cluster_full_df <- merge(alpha_div_cluster_sub, land_cluster, by = c("cluster", "year"))

# -------- segment level data base -------
load("data/alpha_div_segments_df.rda")

alpha_div_seg_sub <- alpha_div_segments_df %>%
  filter(year >= 2004) %>%
  rename(segment = partition, richness = sobs, sum_abundance = n)

land_sub <- land %>%
  filter(year >= 2003 & year <= 2019)

segments_full_df <- merge(alpha_div_seg_sub, land_sub, by = c("segment", "year"))

#--------- ecoregion level data base -------
load("data/alpha_div_ecoregion_df.rda")

alpha_div_eco_sub <- alpha_div_ecoregion_df %>%
  filter(year >= 2004) %>%
  rename(richness = sobs, sum_abundance = n)

# summarize land use across ecoregion
land_ecoregion <- land %>%
  separate(cluster, into = c("ecoregion", "number"),
           sep = "_\\d+$", remove = FALSE) %>%
  group_by(year, ecoregion) %>%
  summarize(
    mean_urban = mean(urban),
    sd_urban = sd(urban),
    mean_barren = mean(barren),
    sd_barren = sd(barren),
    mean_wet = mean(wet),
    sd_wet = sd(wet),
    mean_pasture = mean(pasture),
    sd_pasture = sd(pasture),
    mean_grass = mean(grass),
    sd_grass = sd(grass),
    mean_other = mean(other),
    sd_other = sd(other)
  ) %>%
  filter(year >= 2003 & year <= 2019)

ecoregion_full_df <- merge(alpha_div_eco_sub, land_ecoregion, by = c("ecoregion", "year"))

# ----- add data from cumulated first three years -----


# ----- save the data frames -----

save(ecoregion_full_df, file= "data/ecoregion_full_df.rda")
save(segments_full_df, file= "data/segments_full_df.rda")
save(cluster_full_df, file= "data/cluster_full_df.rda")
