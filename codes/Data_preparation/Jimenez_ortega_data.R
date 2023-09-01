# Jiménez-Ortega et al., 2020 data preparation
# Author: Jon Went
# Date : 22.08.2023

# --- 
library(readxl)
library(tidyverse)

rm(list = ls())
# ----- 

brain_df <- read_xlsx("other_studies/Jimenez-Ortega_2020/Brain_size_and_life_history_variables_in_birds.xlsx")
str(brain_df)

# -----

load("data/Lica/BBS_partition_abundance.rda")
BBS_df <- BBS_partition_abundance; rm(BBS_partition_abundance)

BBS_species <- unique(BBS_df$animal_jetz)

#----

brain_sub <- brain_df %>%
  filter(species %in% BBS_species,
         !is.na(species)) %>%
  rename(ORDER = order,
         Family = family,
         animal_jetz = species) %>%
  select(-references, -sample.size)

# ---- 

BBS_brain <- BBS_df %>%
  left_join(brain_sub, by = c("ORDER", "Family", "animal_jetz"))

# ---- 

save(brain_sub, file = "data/Jimenez_Ortega_et_al/brain_sub.rda")
save(BBS_brain, file = "data/Jimenez_Ortega_et_al/BBS_brain.rda")
