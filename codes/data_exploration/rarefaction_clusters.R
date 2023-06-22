### Rarefaction of species
# Date: 20.05.2023
# Author: Jon Went, jwent@ethz.ch

#---------- load libraries --------
library(vegan)

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
spec_nr_seg <- specnumber(stable_species_mat_filtered)
raremax <- min(rowSums(abund_clusters_mat))
raremax_seg <- min(rowSums(stable_species_mat_filtered))

mean(rowSums(stable_species_mat_filtered))
median(rowSums(stable_species_mat_filtered))
sd(rowSums(stable_species_mat_filtered))

mean(rowSums(abund_clusters_mat))
median(rowSums(abund_clusters_mat))
sd(rowSums(abund_clusters_mat))

# rarefy the data
mode(stable_species_mat_filtered) <- "integer"
mode(abund_clusters_mat) <- "integer"
clust_rare <- rarefy(abund_clusters_mat, raremax)
plot(spec_nr, clust_rare, xlab = "Observed No. of Species",
     ylab = "Rarefied No. of Species")

seg_rare <- rarefy(stable_species_mat_filtered, 100)
plot(spec_nr_seg, seg_rare, xlab = "Observed No. of Species",
     ylab = "Rarefied No. of Species")

#---------- rarefaction of cluster of ecoregions ----

