# data exploration
# Author: Jon Went, jwent@ethz.ch
# Date: 21.09.2023

# ---- libraries ----

library(tidyverse)
library(ggplot2)
library(patchwork)


# ---- abundance data  -----
load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")
load("data/Lica/BBS_partition_abundance.rda")

AOU_spec_name <- BBS_partition_abundance %>% select(AOU, animal_jetz) %>% distinct()
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

ACAD <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/ACAD Global 2021.02.05-filtered.csv")

ACAD.sub <- ACAD %>% select(Scientific.Name, Common.Name, PT.c) %>%
  mutate(animal_jetz = gsub(" ", "_", Scientific.Name)) %>%
  relocate(animal_jetz) %>%
  rename(ACAD.ind = PT.c) %>%
  select(-Scientific.Name); rm(ACAD)

sauer <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS_1966-2019_core_trend_best.csv")

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

avonet <- AVONET_BirdTree_sub %>% select(animal_jetz, Mass, `Hand-Wing.Index`, Trophic.Level, Trophic.Niche) %>%
  rename(hand.wing.ind = `Hand-Wing.Index`,
         body.mass = Mass); rm(AVONET_BirdTree_sub)

# length(unique(avonet$animal_jetz)) # 564 species

species.traits <- gen.length %>%
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

# summary(species.traits)

# save(species.traits, file = "data/species_traits.rda")

# ---- full data sets -----

load("data/species_traits.rda")

full.min40 <- median.d.abund.min40 %>%
  left_join(species.traits, by = "animal_jetz") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 14) %>%
  select(-na.num) %>%
  relocate(Common.Name, .after = animal_jetz)

# summary(full.min40)

# ----- continuous variables plots -----

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  ylab("Median delta Abund") +
  xlab("log(Generation length)")

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Diet breadth") +
  ylab("")

p3 <- ggplot(full.min40, aes(y = median.d.abund, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("")

p4 <- ggplot(full.min40, aes(y = median.d.abund, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Hand-wing index") +
  ylab("Median delta Abund")

p5 <- ggplot(full.min40, aes(y = median.d.abund, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  xlab("log(Body mass)") +
  ylab("")

p6 <- ggplot(full.min40, aes(y = median.d.abund, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  xlab("Habitat breadth") +
  ylab("")

final_plot <- p1 + p2 + p3 + p4 + p5 + p6

# final_plot

# ggsave(filename = "figures/species_traits_median_abund_min40.png", plot = final_plot, width = 8, height = 6, dpi = 300)

# ----- boxplots -----

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  ylab("Median delta abundance") +
  xlab("log(Innovativeness)")

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

p3 <- ggplot(full.min40, aes(y = median.d.abund, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  xlab("Trophic Level") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
  

boxplot <- p1 + p2 + p3

# boxplot

ggsave(filename = "figures/boxplot_species_traits_min40.png", plot = boxplot, width = 8, height = 6, dpi = 300)

# ---- validation plots ---- 

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = PT.c, group = PT.c)) +
  geom_boxplot() +
  xlab("ACAD pop. trend") +
  ylab("Median delta Abund")

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = sauer.trend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()  +
  xlab("Sauer's pop. trend") +
  ylab("")

p3 <- ggplot(full.min40, aes(x = PT.c, y = sauer.trend, group = PT.c)) +
  geom_boxplot() +
  ylab("Sauer's pop. trend") +
  xlab("ACAD pop. trend")

validation_plot <- p1 + p2 + p3

validation_plot

ggsave(filename = "figures/validation_plot.png", plot = validation_plot, width = 8, height = 6, dpi = 300)
