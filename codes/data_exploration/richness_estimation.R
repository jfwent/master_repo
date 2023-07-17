# Richness estimation/rarification using vegan and Chao
# Author: Jon Went, jwent@ethz.ch
# Date: 12.06.2023

# ----- load libraries ----

library(tidyverse)
library(vegan)
library(ggplot2)

# ---- load data ---- 

rm(list=ls())
load("data/stable_species_mat.rda")
load("data/Lica/BBS_partition_abundance.rda")
load("data/land_use_clustered_complete_df.rda")

# --- simple data transformations ----

stable_df <- stable_species_mat_filtered %>%
  reshape2::melt(varnames = c("segment", "animal_jetz"),
                 value.name = "avg_abundance") %>%
  filter(avg_abundance > 0) %>%
  left_join(land_complete[land_complete$year == 2001, ], by = c("segment")) %>%
  select(year, segment, animal_jetz, avg_abundance, cluster, ecoregion) %>%
  rename(seg_abundance = avg_abundance)

BBS_sub <- BBS_partition_abundance %>%
  filter(year %in% land_complete$year,
         year > 2001) %>%
  rename(segment = partition) %>%
  left_join(land_complete, by = c("segment", "year")) %>%
  select(year, segment, animal_jetz, seg_abundance, cluster, ecoregion)

birds_all <- bind_rows(stable_df, BBS_sub)

birds_all  %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  mutate(seg_abundance = round(seg_abundance, 0)) %>%
  summarize(tot_abund = sum(seg_abundance)) %>%
  summarize(min = min(tot_abund)) %>%
  arrange(min)

# minimal total abundance per cluster for years is lowest in 2008 and highest in 2019, from 28-66

for(i in unique(birds_all$year)){

p <-   birds_all  %>%
    group_by(cluster) %>%
    filter(year %in% i) %>%
    filter(!is.na(cluster)) %>%
    summarize(tot_abund = sum(seg_abundance)) %>%
    ggplot(aes(x = tot_abund)) +
    geom_histogram(binwidth = 500) +
    coord_cartesian(xlim=c(0,4000), ylim = c(0,50)) +
    ggtitle(paste0("Data in ", i))

print(p)

}


# we can see that a lot of data is between 0-1000 for the total abundance per cluster for all years
# especially so for 2001, 2013 and 2019

for(i in unique(birds_all$year)){
  
  p2 <- birds_all  %>%
    group_by(cluster) %>%
    filter(year %in% i) %>%
    filter(!is.na(cluster)) %>%
    summarize(tot_abund = sum(seg_abundance)) %>%
    ggplot(aes(x =1, y = tot_abund)) +
    geom_jitter() +
    scale_y_log10() +
    ggtitle(paste0("Data in ", i))
  
  print(p2)
  
  #we can see that most clusters have points with tot_abund > 100 in all years
  
}




birds_all  %>%
  group_by(cluster) %>%
  filter(year == 2001) %>%
  filter(!is.na(cluster)) %>%
  summarize(tot_abund = sum(seg_abundance)) %>%
  ggplot(aes(x =1, y = tot_abund)) +
  geom_violin()


birds_all %>%
  group_by(cluster) %>%
  filter(year %in% 2019) %>%
  filter(!is.na(cluster)) %>%
  summarize(tot_abund = sum(seg_abundance)) %>%
  arrange(tot_abund) %>%
  ggplot(aes(x = 1:nrow(.), y = tot_abund)) +
  geom_line() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,1000))

birds_all %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  summarize(tot_abund = sum(seg_abundance)) %>%
  arrange(tot_abund) %>%
  ggplot(aes(x = 1:nrow(.), y = tot_abund)) +
  geom_line() +
  coord_cartesian(xlim = c(0,50), ylim = c(0,100))

# we see sort of an inflection around tot_abund = 56

birds_all %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  summarize(tot_abund = sum(seg_abundance)) %>%
  arrange(tot_abund) %>%
  print(n=20)

# also here a nice little jump from 48 to 56

get_chao <- function(x){
  
  sp_obs <- sum(x$n)
  sing <- x[x$seg_abund == 1, "n"] %>% pull(n)
  doub <- x[x$seg_abund == 2, "n"] %>% pull(n)
  
  chao <- sp_obs + sing^2 / (2*doub)
  
  return(chao)
  
}

get_chao1 <- function(x){
  
  sp_obs <- sum(x$n)
  
  doub <- x[x$seg_abund == 2, "n"] %>% pull(n)
  
  chao1 <- sp_obs + (sp_obs^2 / (2*(doub+1)))
  
  return(chao1)
}


tmp <- birds_all %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  mutate(seg_abundance = round(seg_abundance, 0)) %>%
  count(cluster, seg_abundance) %>%
  nest(data = - c("year", "cluster")) %>%
  mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance * .x$n)),
       sp_obs = map_dbl(data, ~sum(.x$n)),
       rare = map_dbl(data, ~rarefy(rep(.x$seg_abundance, .x$n), sample = 56)),
       # chao = map_dbl(data, ~get_chao(.x)),
       # chao1 = map_dbl(data, ~get_chao1(.x))
       ) %>%
  select(-data)


b_analysis <- birds_all %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  mutate(seg_abundance = round(seg_abundance, 0)) %>%
  count(cluster, seg_abundance) %>%
  nest(data = - c("year", "cluster")) %>%
  mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance * .x$n)),
         sp_obs = map_dbl(data, ~sum(.x$n)),
         rare = map_dbl(data, ~rarefy(rep(.x$seg_abundance, .x$n), sample = 56)),
         # chao = map_dbl(data, ~get_chao(.x)),
         # chao1 = map_dbl(data, ~get_chao1(.x))
         ) %>%
  select(-data)
  
b_analysis %>%
  select(year, cluster, tot_abund, sp_obs, rare) %>%
  pivot_longer(-c(year, cluster, tot_abund)) %>%
  ggplot(aes(x = tot_abund, y = value, color=name)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(ylim=c(0,300))

# not sure if I can use the same principle from here on, because we're not working with sequencing data...

# calculating the good coverage

coverage_stats <- birds_all %>%
  group_by(year, cluster) %>%
  filter(!is.na(cluster)) %>%
  mutate(seg_abundance = round(seg_abundance, 0)) %>%
  summarize(tot_abund = sum(seg_abundance),
            n_sings = sum(seg_abundance == 1),
            goods = 100*(1 - n_sings / tot_abund)) %>%
  filter(tot_abund > 56)

# plotting the goods coverage
coverage_stats %>%
  ggplot(aes(x=tot_abund, y=goods)) +
  geom_point()

# with out cut off value of 56, all coverage is above 74.2 percent
coverage_stats %>%
  arrange(goods) %>%
  print(n = 20)
