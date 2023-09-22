# Land use variable PCA
# Author: Jon Went
# Date: 22.09.2023

# ---- libraries ----
library(tidyverse)
library(ade4)
library(factoextra)

# ---- load data ----

load("data/BBS.full.stable.min40.rda")

lc.df <- BBS.stable.full.min40 %>%
  select(-c(contains("mean")), abund.geom.mean, hfp.mean) %>%
  relocate(abund.geom.mean, .after = animal_jetz)

# ----- 