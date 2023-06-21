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

#---------- code here -------------

#---------- rarefaction of cluster of ecoregions ----

