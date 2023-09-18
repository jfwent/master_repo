# Habitat breadth
# Author: Jon Went, jwent@ethz.ch
# Date: 07.09.2023

# ---- library ---- 
library(tidyverse)

# ---- 
load("data/Lica/BBS_partition_abundance.rda")

BBS.birds <- unique(BBS_partition_abundance$animal_jetz); rm(BBS_partition_abundance)

habitat_breadth.raw <- read.delim("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/Habitat_breadth_index.txt")

habitat_breadth <- habitat_breadth.raw %>% filter(Class == "AVES") %>%
  mutate(species_name = gsub("-", "_", Species)) %>%
  relocate(species_name) %>%
  filter(species_name %in% BBS.birds) %>%
  rename(hab_breadth = Habitat.breadth..Within.class.co.occurrence.index..multiplicative.beta.) %>%
  select(c(species_name, hab_breadth))


save(habitat_breadth, file ="data/habitat_breadth.rda")  

