# Build Delta-Abundance Model
# Author: Jon Went
# Date: 14.09.2023

# ---- library ----
library(tidyverse)

# ---- load data ----

# load("data/Climate/climate_df.rda")
# load("data/hfp_t1_t2.rda")
# load("data/Land_use/land_use_area_t1_t2.rda")

load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")

# ---- look at delta abundance ----

hist(d.abund.min40$delta.abund, breaks = 100)

library(ggplot2)
library(ggridges)

d.abund.min40 %>%
  # group_by(animal_jetz) %>%
  # mutate(
  #   n_obs = n(),
  #   n_nulls = sum(between(delta.abund, -1, 1)),
  #   null_pct = (n_nulls/n_obs)*100
  # ) %>%
  # arrange(null_pct) %>%
  ggplot(aes(y = animal_jetz, x = delta.abund)) +
  # geom_boxplot()
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  theme(axis.text=element_text(size=5)) +
  scale_x_continuous(expand = expansion(mult = 0.01))

d.abund.min40 %>% 
  group_by(animal_jetz) %>%
  summarize(
    n_obs = n(),
    n_nulls = sum(between(delta.abund, -1, 1)),
    null_pct = (n_nulls/n_obs)*100
  ) %>%
  arrange(null_pct) %>%
  filter(n_obs >= 40) %>%
  print(n=80)

d.abund.min10 %>% 
  group_by(animal_jetz) %>%
  summarize(
    n_obs = n(),
    n_nulls = sum(between(delta.abund, -1, 1)),
    null_pct = (n_nulls/n_obs)*100
  ) %>%
  arrange(null_pct) %>%
  filter(n_obs >= 10) %>%
  print(n=140)

d.abund.min6 %>% 
  group_by(animal_jetz) %>%
  summarize(
    n_obs = n(),
    n_nulls = sum(between(delta.abund, -1, 1)),
    null_pct = (n_nulls/n_obs)*100
  ) %>%
  arrange(null_pct) %>%
  filter(n_obs >= 6) %>%
  print(n=180)

ttt %>%
  filter(animal_jetz == "Agelaius_phoeniceus") %>%
  .$d.abund %>%
  hist(breaks = 100, main = "Agelaius_phoeniceus")

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

# ---- LM function -----