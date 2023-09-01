# Preparation: Elton Traits/Functional bird data
# Author: Jon Went, jwent@ethz.ch
# Date: 1.9.2023

# ---- libraries

library(tidyverse)

# ----- load BBS data ----

load("data/Lica/BBS_partition_abundance.rda")

BBS.df <- BBS_partition_abundance %>%
  rename(segment = partition) %>%
  select(year, segment, animal_jetz, seg_abundance); rm(BBS_partition_abundance)

BBS_species <- unique(BBS.df$animal_jetz)

# --- load Functional data ----

elton_traits <- read.delim("other_studies/Elton_traits/BirdFuncDat.txt")

FuncDat <- elton_traits %>%
  mutate(animal_jetz = gsub(" ", "_", Scientific)) %>%
  relocate(animal_jetz) %>%
  select(-c(2:10, 22:24, 33:35, 38:41)) %>%
  filter(!is.na(Diet.Inv),
         animal_jetz %in% BBS_species)

# --- 
save(FuncDat, file = "data/fun_traits/FuncDat.rda")
