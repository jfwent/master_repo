# create base data frame for all subsequent beta diversity modelling
# Author: Jon Went, jwent@ethz.ch
# Date: 07.06.2023

#--------- load libraries -------
rm(list=ls())

# ---- load land use data ----
load("data/land_use_clustered_complete_df.rda")

#--------- transform land use data -----

# summarize land use data across clusters
land_cluster <- land_complete %>%
  select(-route, cluster_nr) %>%
  mutate( # transform the pixels to area
    across(
      .cols = c(forest, grass, pasture, crop, wet, barren, urban, other),
      .fns = list(
        area = \(x) (x/100)*(30*30)
      ),
      .names = '{col}.area'
    )
  ) %>%
  group_by(year, cluster) %>%
  summarize(
    across(
      .cols = contains("."),
      .fns = c(
        mean = \(x) mean(x, na.rm = T),
        variance = \(x) var(x, na.rm = T),
        stdev = \(x) sd(x, na.rm = T),
        sum = \(x) sum(x, na.rm = T)
      ),
      .names = '{col}--{.fn}'
    )
  ) %>%
  pivot_longer(
    -c(year, cluster),
    names_to = c("land_cover","stat"),
    names_sep = "--",
    values_to = "area_in_m2"
  ) %>%
  nest(land_cover = -c(year, cluster))

# summarize land use data across ecoregions
land_ecoregion <- land_complete %>%
  select(-route, cluster_nr) %>%
  mutate( # transform the pixels to area
    across(
      .cols = c(forest, grass, pasture, crop, wet, barren, urban, other),
      .fns = list(
        area = \(x) (x/100)*(30*30)
      ),
      .names = '{col}.area'
    )
  ) %>%
  group_by(year, ecoregion) %>%
  summarize(
    across(
      .cols = contains("."),
      .fns = c(
        mean = \(x) mean(x, na.rm = T),
        variance = \(x) var(x, na.rm = T),
        stdev = \(x) sd(x, na.rm = T),
        sum = \(x) sum(x, na.rm = T)
      ),
      .names = '{col}--{.fn}'
    )
  ) %>%
  pivot_longer(
    -c(year, ecoregion),
    names_to = c("land_cover","stat"),
    names_sep = "--",
    values_to = "area_in_m2"
  ) %>%
  nest(land_cover = -c(year, ecoregion))

#-----cluster level data base ----
# load data 
load("data/alpha_div_cluster_df.rda")
load("data/alpha_div_cluster_stable.rda")

cluster_stable_df <- merge(BBS_stable_alpha_cluster, land_cluster[land_cluster$year == 2001,], by = c("cluster"))

cluster_full_df <- merge(alpha_div_cluster_df, land_cluster, by = c("cluster", "year"))

# -------- segment level data base -------
load("data/alpha_div_segments_df.rda")
load("data/alpha_div_seg_stable.rda")


# 
segments_stable_df <- merge(BBS_stable_alpha_seg, land_complete[land_complete$year == 2001,], by = "segment")

segments_full_df <- merge(alpha_div_segments_df, land_complete, by = c("segment", "year"))

#--------- ecoregion level data base -------
load("data/alpha_div_ecoregion_df.rda")
load("data/alpha_div_ecoregion_stable.rda")

ecoregion_stable_df <- merge(BBS_stable_alpha_ecoregion, land_ecoregion[land_ecoregion$year == 2001,], by = "ecoregion")

ecoregion_full_df <- merge(alpha_div_ecoregion_df, land_ecoregion, by = c("ecoregion", "year"))

# ----- save the data frames -----

save(ecoregion_stable_df, file = "data/database/ecoregion_stable_df.rda")
save(segments_stable_df, file = "data/database/segments_stable_df.rda")
save(cluster_stable_df, file = "data/database/cluster_stable_df.rda")

save(ecoregion_full_df, file= "data/database/ecoregion_full_df.rda")
save(segments_full_df, file= "data/database/segments_full_df.rda")
save(cluster_full_df, file= "data/database/cluster_full_df.rda")
