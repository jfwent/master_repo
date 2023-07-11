## prepare R session to be able to start with the model building
# Author: Jon Went, jwent@ethz.ch
# Date: 07.06.2023

# -------- load libraries --------
rm(list=ls())
library(tidyverse)
library(brms)

# -------- load data -------------

load("data/ecoregion_full_df.rda")
load("data/segments_full_df.rda")
load("data/cluster_full_df.rda")
