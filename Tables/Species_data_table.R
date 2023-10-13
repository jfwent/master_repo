# make tables
# date: 13.10.2023

# -----
# install.packages("sjPlot")
# library(sjPlot)
# install.packages("stargazer")
# install.packages("xtable")
library(xtable)
library(stargazer)
library(tidyverse)

# ---- 
load("data/d.abund.min40.rda")
load("data/species_traits.rda")

# ----

final_birds <- unique(abund.min40.lc$animal_jetz)

species.traits.sub <- species.traits %>%
  select(-c(tot_diet_div, shannon, Clutch)) %>%
  rename(Species =  animal_jetz,
         Common.name = Common.Name,
         Gen.length = GenLength,
         Clutch = Clutch.Bird,
         Habitat.breadth = hab.breadth,
         Tot.innov = tot.innov,
         Hand.wing.ind = hand.wing.ind,
         Diet.breadth = diet.breadth,
         Rel.brain.size = rel_brain_size,
         Pop.trend.Sauer = sauer.trend,
         Pop.trend.ACAD = ACAD.ind) %>%
  relocate(Common.name, .after = Species) %>%
  relocate(Hand.wing.ind, .after = Migrant) %>%
  relocate(body.mass, .after = Hand.wing.ind) %>%
  relocate(Habitat.breadth, .after = body.mass) %>%
  relocate(Diet.breadth, .after = Habitat.breadth) %>%
  relocate(Trophic.Level, .after = Diet.breadth) %>%
  relocate(Trophic.Niche, .after = Trophic.Level) %>%
  relocate(Gen.length, .after = Trophic.Niche) %>%
  relocate(Clutch, .after = Gen.length) %>%
  filter(Species %in% final_birds) %>%
  arrange(Species)

xtable(species.traits.sub, caption = "Bird species traits")

# ----- land use change ---- 

load("data/Land_use/land_use_area_t1_t2.rda")







