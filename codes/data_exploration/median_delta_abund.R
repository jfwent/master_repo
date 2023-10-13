# data exploration
# Author: Jon Went, jwent@ethz.ch
# Date: 21.09.2023

# ---- libraries ----

library(tidyverse)
library(ggplot2)
library(patchwork)

# ---- abundance data  -----

load("data/d.abund.min40.rda")

# load("data/d.abund.min6.rda")
# load("data/d.abund.min10.rda")
load("data/Lica/BBS_partition_abundance.rda")

AOU_spec_name <- BBS_partition_abundance %>% select(AOU, animal_jetz) %>% distinct()
birds <- unique(BBS_partition_abundance$animal_jetz); rm(BBS_partition_abundance)

# median.d.abund.min6 <- d.abund.min6 %>%
#   group_by(animal_jetz) %>%
#   summarize(median.d.abund = median(delta.abund)); rm(d.abund.min6)
# 
# median.d.abund.min10 <- d.abund.min10 %>%
#   group_by(animal_jetz) %>%
#   summarize(median.d.abund = median(delta.abund)); rm(d.abund.min10)

median.d.abund.min40 <- d.abund.min40 %>%
  group_by(animal_jetz) %>%
  summarize(median.d.abund = median(delta.abund))

# ---- land use data ----

load("data/Land_use/land_use_area_t1_t2.rda")

load("data/Landuse_PC1_PC2.rda")

lc.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2, pasture.area.m2)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"))

# lc.df <- land_use_area %>%
#   select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
#   mutate(PC1 = urban.high.area.m2 + urban.low.area.m2 + grass.area.m2 + pasture.area.m2,
#          PC2 = forest.area.m2 + crop.area.m2) %>%
#   select(-c(urban.high.area.m2, urban.low.area.m2,
#             grass.area.m2, pasture.area.m2, forest.area.m2, crop.area.m2)) %>%
#   mutate(across(
#     .cols = contains("area") | contains("PC"),
#     .fns = c(
#       log = \(x) log(x + 900))
#     ,
#     .names = "{.col}.{.fn}"
#   )) %>%
#   select(year, segment, contains("log"))

lc.t1 <- lc.df %>% filter(year == 2001) %>% select(-year)

dlc <- lc.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('area') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit()

lc.df <- lc.t1 %>% left_join(dlc, by = "segment")%>%
  left_join(lc.pcs, by = "segment") %>% select(-year)

rm(dlc, land_use_area, lc.t1)

# ---- climate data ----

load("data/Climate/climate_df.rda")

climate.df <- climate_df %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean"))

clim.t1 <- climate.df %>% filter(year == 2001) %>% select(-year)

dclim <- climate.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit()

clim.df <- clim.t1 %>% left_join(dclim, by = "segment")

rm(climate_df, climate.df, clim.t1, dclim)

# ----- species traits ----

# load("data/AVONET/AVONET_BirdTree.rda")
# load("data/Gen_length/Bird_full_df.rda")
# load("data/fun_traits/FuncDat.rda")
# load("data/Brain_size/brain_sub.rda")
# load("data/Clutch_sizes/clutch_sizes.rda")
# 
# ACAD <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/ACAD Global 2021.02.05-filtered.csv")
# 
# ACAD.sub <- ACAD %>% select(Scientific.Name, Common.Name, PT.c) %>%
#   mutate(animal_jetz = gsub(" ", "_", Scientific.Name)) %>%
#   relocate(animal_jetz) %>%
#   rename(ACAD.ind = PT.c) %>%
#   select(-Scientific.Name); rm(ACAD)
# 
# sauer <- read.csv("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/BBS_1966-2019_core_trend_best.csv")
# 
# sauer.sub <- sauer %>% filter(Region %in%  "US1") %>%
#   left_join(AOU_spec_name, by = "AOU") %>%
#   select(animal_jetz, Trend) %>%
#   rename(sauer.trend = Trend); rm(sauer)
# 
# FuncDat.sub <- FuncDat %>% select(animal_jetz, contains("Diet"), -Diet.5Cat)
# 
# FuncDat.sub$shannon <- vegan::diversity(FuncDat.sub[,-1], index = "shannon")
# 
# FuncDat.sub <- FuncDat.sub  %>%
#   mutate(tot_diet_div = rowSums(select(., -c(animal_jetz, shannon)) != 0))
# 
# diet.breadth <- FuncDat.sub %>% select(animal_jetz, tot_diet_div, shannon) %>%
#   mutate(diet.breadth = shannon +1); rm(FuncDat, FuncDat.sub)
# 
# # length(unique(diet.breadth$animal_jetz)) # 564 species
# 
# hab.breadth <- readxl::read_xlsx("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/doi_10.5061_dryad.sf7m0cg2k__v3/Dataset_Ducatez_et_al.xlsx") %>%
#   select(Species, HabitatBreadth, TotalInnovations) %>%
#   rename(animal_jetz = Species,
#          hab.breadth = HabitatBreadth,
#          tot.innov = TotalInnovations)
# 
# # length(unique(hab.breadth$animal_jetz)) # 8645
# 
# gen.length <- Bird_full_df %>% select(animal_jetz, GenLength, Mean_clutch_size, Migrant) %>%
#   mutate(Clutch.Bird = as.numeric(Mean_clutch_size)) %>%
#   select(-Mean_clutch_size); rm(Bird_full_df)
# 
# # length(unique(gen.length$animal_jetz)) # 11126 species
# 
# brain.df <- brain_sub %>% mutate(rel_brain_size = (brain.mass/body.mass)*100) %>%
#   select(animal_jetz, rel_brain_size); rm(brain_sub)
# 
# # length(unique(brain.df$animal_jetz)) # 285 species
# 
# avonet <- AVONET_BirdTree_sub %>% select(animal_jetz, Mass, `Hand-Wing.Index`, Trophic.Level, Trophic.Niche) %>%
#   rename(hand.wing.ind = `Hand-Wing.Index`,
#          body.mass = Mass); rm(AVONET_BirdTree_sub)
# 
# # length(unique(avonet$animal_jetz)) # 564 species
# 
# clutch <- clutch_dat %>% select(animal_jetz, Clutch); rm(clutch_dat)
# 
# # length(unique(clutch$animal_jetz)) # 3205 species
# 
# species.traits <- gen.length %>%
#   left_join(clutch, by = "animal_jetz") %>%
#   left_join(hab.breadth, by = "animal_jetz") %>%
#   left_join(avonet, by = "animal_jetz") %>%
#   left_join(diet.breadth, by = "animal_jetz") %>%
#   left_join(brain.df, by = "animal_jetz") %>%
#   left_join(ACAD.sub, by = "animal_jetz") %>%
#   left_join(sauer.sub, by = "animal_jetz") %>%
#   filter(animal_jetz %in% birds)
# 
# rm(hab.breadth, gen.length,
#    avonet, diet.breadth, brain.df,
#    ACAD.sub, sauer.sub, AOU_spec_name, birds)
# 
# summary(species.traits)
# 
# save(species.traits, file = "data/species_traits.rda")
# 
# load("data/species_traits.rda")
# 
# load("data/BBS.full.stable.min40.rda")
# 
# my.init.common <- BBS.stable.full.min40 %>%
#   filter(year == "2001") %>%
#   group_by(animal_jetz) %>%
#   summarize(abund.sum = sum(abund.geom.mean)) %>%
#   ungroup() %>%
#   mutate(abundance_groups = cut(abund.sum,
#                                 breaks = c(0, 100, 1000, 10000, 100000),
#                                 labels = c("Rare", "Less Common", "Common", "Superabundant")))
# 
# species.traits <- species.traits %>%
#   left_join(my.init.common, by = "animal_jetz") %>%
#   rename(initial.abundance = abund.sum)

save(species.traits, file = "data/species_traits.rda")

# ---- full data sets -----

full.min40 <- median.d.abund.min40 %>%
  left_join(species.traits, by = "animal_jetz") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 17) %>%
  select(-na.num) %>%
  relocate(Common.Name, .after = animal_jetz)

summary(full.min40)

median.abund.lc <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(lc.df, by = "segment") %>%
  group_by(animal_jetz) %>%
  mutate(median.abund = median(delta.abund),
         abs_diff = abs(delta.abund - median.abund)) %>%
  filter(abs_diff == min(abs_diff)) %>%
  select(-abs_diff) %>%
  relocate(median.abund, .after = delta.abund) %>%
  mutate(n.obs = n())

multiples <- median.abund.lc %>% filter(n.obs > 1) %>%
  group_by(animal_jetz, median.abund) %>%
  reframe(across(
    .cols = contains("log"),
    .fns = c(
      mean = \(x) mean(x)),
    .names = "{.col}"
  ))

median.abund.lc <- median.abund.lc %>% filter(n.obs == 1) %>%
  select(-segment, -delta.abund, -n.obs) %>%
  bind_rows(multiples)

rm(multiples)

median.abund.clim <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(clim.df, by = "segment") %>%
  group_by(animal_jetz) %>%
  mutate(median.abund = median(delta.abund),
         abs_diff = abs(delta.abund - median.abund)) %>%
  filter(abs_diff == min(abs_diff)) %>%
  select(-abs_diff) %>%
  relocate(median.abund, .after = delta.abund) %>%
  mutate(n.obs = n())

multiples <- median.abund.clim %>% filter(n.obs > 1) %>%
  group_by(animal_jetz, median.abund) %>%
  reframe(across(
    .cols = contains("log"),
    .fns = c(
      mean = \(x) mean(x)),
    .names = "{.col}"
  ))

median.abund.clim <- median.abund.clim %>% filter(n.obs == 1) %>%
  select(-segment, -delta.abund, -n.obs) %>%
  bind_rows(multiples)

rm(multiples)

# ----- continuous species traits plots -----

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Median delta Abund") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(full.min40, aes(y = median.d.abund, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(full.min40, aes(y = median.d.abund, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("Median delta Abund")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p5 <- ggplot(full.min40, aes(y = median.d.abund, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p6 <- ggplot(full.min40, aes(y = median.d.abund, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p7 <- ggplot(full.min40, aes(y = median.d.abund, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size (Jiménez-Ortega)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p8 <- ggplot(full.min40, aes(y = median.d.abund, x = Clutch)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size (Jetz)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8

final_plot

# ggsave(filename = "figures/species_traits_median_abund_min40.png", plot = final_plot, width = 8, height = 6, dpi = 300)

# ----- species traits boxplots -----

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  ylab("Median delta abundance") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylim(-5,3)

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylim(-5,3)

p3 <- ggplot(full.min40, aes(y = median.d.abund, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  xlab("Trophic Level") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(full.min40, aes(y = median.d.abund, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

boxplot <- p1 + p2 + p3 + p4

boxplot

# ggsave(filename = "figures/boxplot_species_traits_min40.png", plot = boxplot, width = 8, height = 6, dpi = 300)

# ---- land cover plots ----

p1 <- ggplot(median.abund.lc, aes(y = median.abund, x = forest.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Median delta Abund") +
  xlab("Forest area")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p2 <- ggplot(median.abund.lc, aes(y = median.abund, x = crop.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Crop area") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(median.abund.lc, aes(y = median.abund, x = wet.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Wet area") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(median.abund.lc, aes(y = median.abund, x = urban.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Urban area") +
  ylab("Median delta Abund")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p5 <- ggplot(median.abund.lc, aes(y = median.abund, x = all.grass.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Grass area") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p6 <- ggplot(median.abund.lc, aes(y = median.abund, x = PC1)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("PC1") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p7 <- ggplot(median.abund.lc, aes(y = median.abund, x = PC2)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("PC2") +
  ylab("Median delta Abund") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

land_cover_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7
land_cover_plot

ggsave(filename = "figures/MedianAbund_Landcover.png", plot = land_cover_plot, width = 8, height = 6, dpi = 300)

p1 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.forest.area.m2.log)) +
  # geom_point(size = 2, alpha = 0.5) +
  # geom_smooth() +
  # geom_abline() +
  # geom_hex(bins = 50) +
  ylab("Median delta Abund") +
  xlab("log(abs(delta Forest area))")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p1

p2 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.crop.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("delta Crop area") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.wet.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("delta Wet area") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.urban.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("delta Urban area") +
  ylab("Median delta Abund")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p5 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.all.grass.area.m2.log)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("delta Grass area") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-5,3)

p6 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.PC1)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("delta PC1") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p7 <- ggplot(median.abund.lc, aes(y = median.abund, x = delta.PC2)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("delta PC2") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

delta_land_cover_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

delta_land_cover_plot

# ---- climate plots ----
p1 <- ggplot(median.abund.clim, aes(y = median.abund, x = tmax.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Median delta Abund") +
  xlab("Max. temp.")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p2 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.tmax.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Delta max. temp.") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(median.abund.clim, aes(y = median.abund, x = tmin.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Min. temp.") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.tmin.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Delta min. temp.") +
  ylab("Median delta Abund")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p5 <- ggplot(median.abund.clim, aes(y = median.abund, x = swb.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("SWB") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p6 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.swb.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Delta SWB") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

climate_plot1 <- p1 + p2 + p3 + p4 + p5 + p6
climate_plot1

ggsave(filename = "figures/MedianAbund_Clim1.png", plot = climate_plot1, width = 8, height = 6, dpi = 300)

p1 <- ggplot(median.abund.clim, aes(y = median.abund, x = cmi.diff.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Median delta Abund") +
  xlab("CMI diff.")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p2 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.cmi.diff.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Delta CMI diff.") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(median.abund.clim, aes(y = median.abund, x = cmi.annual.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("CMI annual") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p4 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.cmi.annual.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Delta CMI annual") +
  ylab("Median delta Abund")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p5 <- ggplot(median.abund.clim, aes(y = median.abund, x = pr.diff.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Pr diff.") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p6 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.pr.diff.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Delta pr. diff.") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p7 <- ggplot(median.abund.clim, aes(y = median.abund, x = pr.sum.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Pr. sum") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p8 <- ggplot(median.abund.clim, aes(y = median.abund, x = delta.pr.sum.mean)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Delta pr. sum") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

climate_plot2 <- p1 + p2 + p3 + p4
climate_plot2

ggsave(filename = "figures/MedianAbund_Clim2.png", plot = climate_plot2, width = 8, height = 6, dpi = 300)

climate_plot3 <- p5 + p6 + p7 + p8
climate_plot3

ggsave(filename = "figures/MedianAbund_Clim3.png", plot = climate_plot3, width = 8, height = 6, dpi = 300)


# ---- validation plots ---- 

p1 <- ggplot(full.min40, aes(y = median.d.abund, x = ACAD.ind, group = ACAD.ind)) +
  geom_boxplot() +
  xlab("ACAD pop. trend") +
  ylab("Median delta Abund") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p2 <- ggplot(full.min40, aes(y = median.d.abund, x = sauer.trend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()  +
  geom_abline() +
  xlab("Sauer's pop. trend") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

p3 <- ggplot(full.min40, aes(x = ACAD.ind, y = sauer.trend, group = ACAD.ind)) +
  geom_boxplot() +
  ylab("Sauer's pop. trend") +
  xlab("ACAD pop. trend") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5,3)

validation_plot <- p1 + p2 + p3

validation_plot

ggsave(filename = "figures/validation_plot.png", plot = validation_plot, width = 8, height = 6, dpi = 300)
