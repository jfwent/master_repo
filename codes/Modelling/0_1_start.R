## prepare R session to be able to start with the model building
# Author: Jon Went, jwent@ethz.ch
# Date: 07.06.2023

# -------- load libraries --------
rm(list=ls())
library(tidyverse)
library(brms)

# -------- load data -------------

load("data/database/ecoregion_stable_df.rda")
load("data/database/segments_stable_df.rda")
load("data/database/cluster_stable_df.rda")

load("data/database/ecoregion_full_df.rda")
load("data/database/segments_full_df.rda")
load("data/database/cluster_full_df.rda")

# --------- what more does the start need? -----