# Select climate variables
# Author: Jon Went, jwent@ethz.ch
# Date: 1.9.2023

# --- libraries ----

library(tidyverse)
library(glmmTMB)

# ---- load data ----

load("data/Climate/climate_df.rda")
load("data/Lica/BBS_partition_abundance.rda")
load("data/Bird_full_df.rda")
load("data/AVONET/AVONET_BirdTree.rda")
load("data/Jimenez_Ortega_et_al/brain_sub.rda")
load("data/fun_traits/FuncDat.rda")


# ---- prepare data set -----

BBS.t1 <- BBS_partition_abundance %>%
  filter(year %in% c(2000:2002)) %>%
  rename(segment = partition,
         seg_abund = seg_abundance) %>%
  select(year, segment, animal_jetz, seg_abund) %>%
  group_by(segment, animal_jetz) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund), 2)) %>%
  mutate(year = 2001) %>%
  relocate(year)

BBS.t2 <- BBS_partition_abundance %>%
  filter(year %in% c(2017:2019)) %>%
  rename(segment = partition,
         seg_abund = seg_abundance) %>%
  select(year, segment, animal_jetz, seg_abund) %>%
  group_by(segment, animal_jetz) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund), 2)) %>%
  mutate(year = 2019) %>%
  relocate(year)

stable_birds <- BBS_partition_abundance %>%
  filter(year %in% c(2000:2002)) %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(avg_abundance = mean(seg_abund)) %>%
  mutate(avg_abund = round(avg_abundance,1)) %>%
  select(-avg_abundance) %>%
  na.omit()


  