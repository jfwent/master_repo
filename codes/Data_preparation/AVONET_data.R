# Avonet data preparation
# Author: Jon Went
# Date: 22.08.2023

# ------ libraries -----
library(readxl)
library(tidyverse)

rm(list = ls())

#------ read data ----

AVONET_BirdLife <- read_xlsx("other_studies/AVONET_2022/AVONET_Supplementary_dataset_1.xlsx",
                             sheet = "AVONET1_BirdLife")
AVONET_eBird <- read_xlsx("other_studies/AVONET_2022/AVONET_Supplementary_dataset_1.xlsx",
                          sheet = "AVONET2_eBird")
AVONET_BirdTree <- read_xlsx("other_studies/AVONET_2022/AVONET_Supplementary_dataset_1.xlsx",
                             sheet = "AVONET3_BirdTree")

# ----- load BBS data ----

load("data/Lica/BBS_partition_abundance.rda")
BBS_df <- BBS_partition_abundance; rm(BBS_partition_abundance)

# ---- 
# get rid of spaces in bird species entries
AVONET_BirdLife$animal_jetz <- gsub(" ", "_", AVONET_BirdLife$Species1)
AVONET_eBird$animal_jetz <- gsub(" ", "_", AVONET_eBird$Species2)
AVONET_BirdTree$animal_jetz <- gsub(" ", "_", AVONET_BirdTree$Species3)

# ---- 

BBS_species <- unique(BBS_df$animal_jetz)

AVONET_eBird_sub <- AVONET_eBird %>%
  filter(animal_jetz %in% BBS_species)

AVONET_BirdLife_sub <- AVONET_BirdLife %>%
  filter(animal_jetz %in% BBS_species)

AVONET_BirdTree_sub <- AVONET_BirdTree %>%
  filter(animal_jetz %in% BBS_species) %>%
  select(-c(Species3, Total.individuals, Male, Female, Unknown,
            Mass.Refs.Other, Traits.inferred, Inference,
            Reference.species, Species.Status, Complete.measures)) %>%
  rename(Family = Family3,
         ORDER = Order3)

# ----- 

save(AVONET_BirdLife_sub, file = "data/AVONET/AVONET_BirdLife.rda")
save(AVONET_BirdTree_sub, file = "data/AVONET/AVONET_BirdTree.rda")
save(AVONET_eBird_sub, file = "data/AVONET/AVONET_eBird.rda")

BBS_avonet <- BBS_df %>% left_join(AVONET_BirdTree_sub, by = c("animal_jetz", "ORDER", "Family"))

save(BBS_avonet, file= "data/AVONET/BBS_AVONET.rda")
