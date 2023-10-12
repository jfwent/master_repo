# Model ready data sets
# Author: Jon Went
# Date: 29.09.2023

#---- library ----
library(tidyverse)

# === Linear model data set with delta data set ----
# ---- load data ----

load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")

load("data/Climate/climate_df.rda")
load("data/Land_use/land_use_area_t1_t2.rda")
load("data/Landuse_PC1_PC2.rda")

# ---- bioclim data ----

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median")),
         contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, pr.diff.mean))

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

clim.df <- clim.t1 %>% left_join(dclim, by = "segment"); rm(climate_df, climate.df, clim.t1, dclim)

lc.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2,
            pasture.area.m2)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"))

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

lc.df <- lc.t1 %>% left_join(dlc, by = "segment")

dlc.pcs <- lc.pcs %>% na.omit() %>% select(-year)

rm(dlc, land_use_area, lc.t1)

# ---- full data set ----

abund.min40.lc <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(clim.df, by = "segment") %>%
  left_join(lc.df, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries >= 40) %>%
  select(-n_entries)

abund.min40.pc <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(clim.df, by = "segment") %>%
  left_join(dlc.pcs, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries >= 40) %>%
  select(-n_entries)

rm(clim.df, lc.df, lc.pcs, d.abund.min10, d.abund.min40, d.abund.min6, dlc.pcs)

rm(abund.min40.pc)

# === SDM model data set T1 + T2 ----
# ---- load data ----

load("data/BBS.full.stable.min40.rda")

load("data/Climate/climate_df.rda")
load("data/Land_use/land_use_area_t1_t2.rda")
load("data/Landuse_PC1_PC2.rda")

# ---- bioclim data ----

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median")),
         contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, pr.diff.mean)); rm(climate_df)

land.use.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2, pasture.area.m2, wet.area.m2)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log")); rm(land_use_area)

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")) %>%
  na.omit()

bioclim.pcs <- climate.df %>%
  left_join(lc.pcs, by = c("year", "segment")) %>%
  select(-delta.PC1, -delta.PC2) %>%
  na.omit()

rm(climate.df, land.use.df, lc.pcs)

# ---- full data set

BBS_bioclim <- BBS.stable.full.min40 %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  left_join(bioclim.df, by = c("year", "segment")) %>%
  arrange(animal_jetz) %>%
  # mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

BBS_bioclim.pc <- BBS.stable.full.min40 %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  left_join(bioclim.pcs, by = c("year", "segment")) %>%
  arrange(animal_jetz) %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

rm(bioclim.df, bioclim.pcs, BBS.stable.full.min40)
