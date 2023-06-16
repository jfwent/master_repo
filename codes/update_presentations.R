# Result presentations

##

rm(list=ls())


#data sets
load("data/land_use_clustered.rda")
load("data/delta_land_use_clustered.rda")
load("data/apparitions_list_coppee.rda")
load("data/extinctions_list_coppee.rda")
load("data/beta_div_list.rda")

apparitions_2000_2001 <- apparition_list[[1]]
extinctions_2000_2001 <- extinction_list[[1]]
beta_div_2000 <- beta_div_list[[1]]
# sim and jtu --> spatial turnover
# sne and jne --> nestedness
# sor --> total dissimilarity
# jac --> dissimilarity accounting for beta diversity (?), Jaccard pair-wise dissim

#plots
load("figures/ecoreg_plots.rda")
Ecoreg_plots[[1]]

load("figures/beta_diversity_plot_list.rda")
# sqrt(sim --> turnover/simpson pair-wise dissimilarity)
# vs sqrt(sne --> nestedness fraction of Sorensen pair-wise dissimilarity)

