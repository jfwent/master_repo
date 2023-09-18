# Build Delta-Abundance Model
# Author: Jon Went
# Date: 14.09.2023

# ---- library ----
library(tidyverse)

# ---- load data ----

load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")

# ---- LM function -----




# ========== Old ----
# ---- prepare the bioclim data ---- 

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median"),
            pr.sum.mean, cmi.annual.mean, cmi.diff.mean),
         contains("mean")); rm(climate_df)

hfp.df <- hfp.full %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- hfp.df %>%
  left_join(land_use_area, by = c("segment", "year")) %>%
  select(-c(ecoregion, tot.area.m2, route)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"), contains("hfp")); rm(land_use_area, hfp.df)

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")); rm(climate.df, land.use.df)

dbioclim <- bioclim.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  reframe(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), lag(.) - ., NA),
      .names = "delta.{col}"
    )
  ); rm(bioclim.df)

dclim <- dbioclim %>% select(segment, contains("mean"), -contains("hfp")) %>% na.omit()

dland.use <- dbioclim %>% select(segment, contains("log"), contains("hfp")) %>% na.omit()

# ---- dabund + climate datasets -----

d.abundmin6.clim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin10.clim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin40.clim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

rm(dclim)

# ---- d.abund + land use datasets ----

d.abundmin6.land.use <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin10.land.use <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin40.land.use <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

rm(dland.use)

# ---- d.abund full datasets ----

d.abundmin6.bioclim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin10.bioclim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin40.bioclim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

rm(dbioclim)
rm(d.abund.min10, d.abund.min40, d.abund.min6)
