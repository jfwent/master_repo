# Result presentations

##

rm(list=ls())


#----- data sets -----

load("data/land_use_clustered.rda")
land_use_clustered <- combined_df;rm(combined_df)

load("data/delta_land_use_clustered.rda")

load("data/apparitions_list_coppee.rda")
load("data/extinctions_list_coppee.rda")
apparitions_2000_2001 <- apparition_list[[1]]
extinctions_2000_2001 <- extinction_list[[1]]


load("data/beta_div_list.rda") # old beta diversity list, calculated at the segment level for the whole USA
beta_div_2000 <- beta_div_list[[1]]
# sim and jtu --> spatial turnover
# sne and jne --> nestedness
# sor --> total dissimilarity
# jac --> dissimilarity accounting for beta diversity (?), Jaccard pair-wise dissim

# the beta diversity calculated at the segment level for habitat clusters
load("data/beta_div_clusters_df.rda")

# the beta diversity calculated at the segment level for ecoregions
load("data/beta_div_ecoregions_df.rda")

# the beta diversity calculated at the cluster level for ecoregions
load("data/beta_div_cluster_in_ecoregion_df.rda")

# Species, filtered for the ones present in 2000, 2001 and 2002
load("data/stable_species_mat.rda")
# I averaged their abundance per segment across the three years
# to discuss:
# - median or mean?
# - filter these for terrestrial birds?
# - Group: non-migratory passerines? Habitat specialists? Feeding niches? Generation time?

# added the Bird et al., 2020 Cons Biol data to the BBS survey data
load("data/BBS_Bird_df.rda")
load("data/BBS_Bird_stable_df.rda")

# rarified the clusters
load("data/clust_rare.rda")

# calculated the probability to observe species in cluster
load("data/clust_species_probability.rda")

# 

#------ plots -----

# really busy plots about the 
load("figures/ecoreg_plots.rda")
Ecoreg_plots[[1]]

load("figures/beta_diversity_plot_list.rda")
# sqrt(sim --> turnover/simpson pair-wise dissimilarity)
# vs sqrt(sne --> nestedness fraction of Sorensen pair-wise dissimilarity)

#beta diversity:
# comparing the beta diversity calculated for clusters at segment and at cluster level
# --> figure/beta_div_ecoreg_levels.tiff

# dominant habitats in clusters
load("figures/dominant_habitat_plots_abbrev.rda") # with abbreviated cluster names
load("figures/dominant_habitats_plots.rda") # with full cluster names

dominant_habitat_plots[[3]]
#habitat switches --> figures/hab_switch_fig.tiff




