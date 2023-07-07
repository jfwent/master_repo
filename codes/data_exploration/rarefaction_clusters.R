### Rarefaction of species
# Date: 20.05.2023
# Author: Jon Went, jwent@ethz.ch

#---------- load libraries --------
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

#---------- load data -------------
rm(list=ls())
load("data/land_use_clustered.rda")
land_use <- combined_df; rm(combined_df)
load("data/stable_species_mat.rda")
stable_spec <- t(stable_species_mat_filtered)
load("data/abund_clusters_mat.rda")
clusters <- t(abund_clusters_mat)

#---------- code here -------------

# get number of individuals
spec_nr <- specnumber(abund_clusters_mat)
min(rowSums(abund_clusters_mat))
raremax <- round(min(rowSums(abund_clusters_mat)))

mean(rowSums(abund_clusters_mat))
median(rowSums(abund_clusters_mat))
sd(rowSums(abund_clusters_mat))

#---------- rarefaction of cluster of ecoregions ----
# rarefy the data

mode(abund_clusters_mat) <- "integer"

clust_rare <- rarefy(abund_clusters_mat, raremax) %>%
  as_tibble(rownames = "Group") %>%
  select(Group, vegan=value)

count_species <- function(row) {
  sum(row > 0)
}

species_counts <- apply(abund_clusters_mat, 1, count_species)
clust_rare$observed <- species_counts

save(clust_rare, file = "data/clust_rare.rda")

old <- options(pillar.sigfig = 7)

clust_drare <- drarefy(abund_clusters_mat, raremax) %>%
  as_tibble(rownames = "Group") %>%
  pivot_longer(-Group)

save(clust_drare, file = "data/clust_species_probability.rda")

plot(spec_nr, clust_rare, xlab = "Observed No. of Species",
     ylab = "Rarefied No. of Species")

clust_rarecurve <- rarecurve(abund_clusters_mat, step = 50)

map_dfr(clust_rarecurve, bind_rows) %>%
  bind_cols(Group = rownames(abund_clusters_mat),.) %>%
  pivot_longer(-Group) %>%
  drop_na() %>%
  mutate(n_spec = as.numeric(str_replace(name, "N", ""))) %>%
  select(-name) %>%
  ggplot(aes(x= n_spec, y = value, group = Group)) +
  geom_vline(xintercept = raremax, color = "gray")+
  theme_classic()+
  geom_line()


#---------- rarefaction of segments ------------

# get number of individuals
spec_nr_seg <- specnumber(stable_species_mat_filtered)
raremax_seg <- min(rowSums(stable_species_mat_filtered)) # only 1.666

mean(rowSums(stable_species_mat_filtered))
median(rowSums(stable_species_mat_filtered))
sd(rowSums(stable_species_mat_filtered))

mode(stable_species_mat_filtered) <- "integer"

seg_rare <- rarefy(stable_species_mat_filtered, 25)
plot(spec_nr_seg, seg_rare, xlab = "Observed No. of Species",
     ylab = "Rarefied No. of Species")

seg_drare <- drarefy(stable_species_mat_filtered, raremax_seg) %>%
  as_tibble(rownames = "Group") %>%
  pivot_longer(-Group)

seg_rarecurve <- rarecurve(stable_species_mat_filtered, step = 50)

map_dfr(seg_rarecurve, bind_rows) %>%
  bind_cols(Group = rownames(stable_species_mat_filtered),.) %>%
  pivot_longer(-Group) %>%
  drop_na() %>%
  mutate(n_spec = as.numeric(str_replace(name, "N", ""))) %>%
  select(-name) %>%
  ggplot(aes(x= n_spec, y = value, group = Group)) +
  geom_vline(xintercept = raremax, color = "gray")+
  theme_classic()+
  geom_line()
