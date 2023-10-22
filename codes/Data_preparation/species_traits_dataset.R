# Prepare the species traits data set
# Author: Jon Went, jwent@ethz.ch
# Date: 13.10.2023

# ---- library -----
library(tidyverse)

# ---- load data ----

load("data/AVONET/AVONET_BirdTree.rda")
load("data/Gen_length/Bird_full_df.rda")
load("data/fun_traits/FuncDat.rda")
load("data/Brain_size/brain_sub.rda")
load("data/Clutch_sizes/clutch_sizes.rda")

ACAD <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/ACAD Global 2021.02.05-filtered.csv")

sauer <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS_1966-2019_core_trend_best.csv")

hab.breadth <- readxl::read_xlsx("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/doi_10.5061_dryad.sf7m0cg2k__v3/Dataset_Ducatez_et_al.xlsx")

# ----- select and clean data ----

ACAD.sub <- ACAD %>% select(Scientific.Name, Common.Name, PT.c) %>%
  mutate(animal_jetz = gsub(" ", "_", Scientific.Name)) %>%
  relocate(animal_jetz) %>%
  rename(ACAD.ind = PT.c) %>%
  select(-Scientific.Name); rm(ACAD)

sauer.sub <- sauer %>% filter(Region %in%  "US1") %>%
  left_join(AOU_spec_name, by = "AOU") %>%
  select(animal_jetz, Trend) %>%
  rename(sauer.trend = Trend); rm(sauer)

FuncDat.sub <- FuncDat %>% select(animal_jetz, contains("Diet"), -Diet.5Cat)

FuncDat.sub$shannon <- vegan::diversity(FuncDat.sub[,-1], index = "shannon")

FuncDat.sub <- FuncDat.sub  %>%
  mutate(tot_diet_div = rowSums(select(., -c(animal_jetz, shannon)) != 0))

diet.breadth <- FuncDat.sub %>% select(animal_jetz, tot_diet_div, shannon) %>%
  mutate(diet.breadth = shannon +1); rm(FuncDat, FuncDat.sub)

# length(unique(diet.breadth$animal_jetz)) # 564 species

hab.breadth <- hab.breadth %>%
  select(Species, HabitatBreadth, TotalInnovations) %>%
  rename(animal_jetz = Species,
         hab.breadth = HabitatBreadth,
         tot.innov = TotalInnovations)

# length(unique(hab.breadth$animal_jetz)) # 8645

gen.length <- Bird_full_df %>% select(animal_jetz, GenLength, Mean_clutch_size, Migrant) %>%
  mutate(Clutch.Bird = as.numeric(Mean_clutch_size)) %>%
  select(-Mean_clutch_size); rm(Bird_full_df)

# length(unique(gen.length$animal_jetz)) # 11126 species

brain.df <- brain_sub %>% mutate(rel_brain_size = (brain.mass/body.mass)*100) %>%
  select(animal_jetz, rel_brain_size); rm(brain_sub)

# length(unique(brain.df$animal_jetz)) # 285 species

avonet <- AVONET_BirdTree_sub %>% select(animal_jetz, Mass, `Hand-Wing.Index`, Trophic.Level, Trophic.Niche) %>%
  rename(hand.wing.ind = `Hand-Wing.Index`,
         body.mass = Mass); rm(AVONET_BirdTree_sub)

# length(unique(avonet$animal_jetz)) # 564 species

clutch <- clutch_dat %>% select(animal_jetz, Clutch); rm(clutch_dat)

# length(unique(clutch$animal_jetz)) # 3205 species

species.traits <- gen.length %>%
  left_join(clutch, by = "animal_jetz") %>%
  left_join(hab.breadth, by = "animal_jetz") %>%
  left_join(avonet, by = "animal_jetz") %>%
  left_join(diet.breadth, by = "animal_jetz") %>%
  left_join(brain.df, by = "animal_jetz") %>%
  left_join(ACAD.sub, by = "animal_jetz") %>%
  left_join(sauer.sub, by = "animal_jetz") %>%
  filter(animal_jetz %in% birds)

rm(hab.breadth, gen.length,
   avonet, diet.breadth, brain.df,
   ACAD.sub, sauer.sub, AOU_spec_name, birds)

summary(species.traits)

save(species.traits, file = "data/species_traits.rda")

load("data/species_traits.rda")

load("data/BBS.full.stable.min40.rda")

my.init.common <- BBS.stable.full.min40 %>%
  filter(year == "2001") %>%
  group_by(animal_jetz) %>%
  summarize(abund.sum = sum(abund.geom.mean)) %>%
  ungroup() %>%
  mutate(abundance_groups = cut(abund.sum,
                                breaks = c(0, 100, 1000, 10000, 100000),
                                labels = c("Rare", "Less Common", "Common", "Superabundant")))

species.traits <- species.traits %>%
  left_join(my.init.common, by = "animal_jetz") %>%
  rename(initial.abundance = abund.sum)

save(species.traits, file = "data/species_traits.rda")