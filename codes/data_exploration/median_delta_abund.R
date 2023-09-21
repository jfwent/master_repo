# data exploration
# Author: Jon Went, jwent@ethz.ch
# Date: 21.09.2023

# ----

library(tidyverse)
library(ggplot2)

# ---- abundance data  -----
load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")
load("data/Lica/BBS_partition_abundance.rda")

birds <- unique(BBS_partition_abundance$animal_jetz); rm(BBS_partition_abundance)

median.d.abund.min6 <- d.abund.min6 %>%
  group_by(animal_jetz) %>%
  summarize(median.d.abund = median(delta.abund)); rm(d.abund.min6)

median.d.abund.min10 <- d.abund.min10 %>%
  group_by(animal_jetz) %>%
  summarize(median.d.abund = median(delta.abund)); rm(d.abund.min10)

median.d.abund.min40 <- d.abund.min40 %>%
  group_by(animal_jetz) %>%
  summarize(median.d.abund = median(delta.abund)); rm(d.abund.min40)


# ----- species traits ----

load("data/AVONET/AVONET_BirdTree.rda")
load("data/Gen_length/Bird_full_df.rda")
load("data/fun_traits/FuncDat.rda")
load("data/Brain_size/brain_sub.rda")

FuncDat.sub <- FuncDat %>% select(animal_jetz, contains("Diet"), -Diet.5Cat)

FuncDat.sub$shannon <- vegan::diversity(FuncDat.sub[,-1], index = "shannon")

FuncDat.sub <- FuncDat.sub  %>%
  mutate(tot_diet_div = rowSums(select(., -c(animal_jetz, shannon)) != 0))

diet.breadth <- FuncDat.sub %>% select(animal_jetz, tot_diet_div, shannon) %>%
  mutate(diet.breadth = shannon +1); rm(FuncDat, FuncDat.sub)

# length(unique(diet.breadth$animal_jetz)) # 564 species

hab.breadth <- readxl::read_xlsx("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/doi_10.5061_dryad.sf7m0cg2k__v3/Dataset_Ducatez_et_al.xlsx") %>%
  select(Species, HabitatBreadth, TotalInnovations) %>%
  rename(animal_jetz = Species, 
         hab.breadth = HabitatBreadth,
         tot.innov = TotalInnovations)

# length(unique(hab.breadth$animal_jetz)) # 8645

gen.length <- Bird_full_df %>% select(animal_jetz, GenLength); rm(Bird_full_df)

# length(unique(gen.length$animal_jetz)) # 11126 species

brain.df <- brain_sub %>% mutate(rel_brain_size = (brain.mass/body.mass)*100) %>%
  select(animal_jetz, rel_brain_size); rm(brain_sub)

# length(unique(brain.df$animal_jetz)) # 285 species

avonet <- AVONET_BirdTree_sub %>% select(animal_jetz, Mass, `Hand-Wing.Index`) %>%
  rename(hand.wing.ind = `Hand-Wing.Index`) %>%
  mutate(log_body_mass = log(Mass)); rm(AVONET_BirdTree_sub)

# length(unique(avonet$animal_jetz)) # 564 species

species.traits <- gen.length %>%
  left_join(hab.breadth, by = "animal_jetz") %>%
  left_join(avonet, by = "animal_jetz") %>%
  left_join(diet.breadth, by = "animal_jetz") %>%
  left_join(brain.df, by = "animal_jetz") %>%
  filter(animal_jetz %in% birds); rm(hab.breadth, gen.length, avonet, diet.breadth, brain.df)
  # na.omit()

summary(species.traits)

# save(species.traits, file = "data/species_traits.rda")

# ---- full data sets -----

full.min10 <- median.d.abund.min10 %>%
  left_join(species.traits, by = "animal_jetz") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 10)

# summary(full.min10)

full.min40 <- median.d.abund.min40 %>%
  left_join(species.traits, by = "animal_jetz") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 10)

# summary(full.min40)

# ----- plots -----

ggplot(full.min40, aes(y = median.d.abund, x = GenLength)) +
  geom_point()


