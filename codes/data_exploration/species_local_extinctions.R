# Species level approach
# Author: Jon Went, jwent@ethz.ch
# Date: 18.07.2023

#the idea is to investigate the effect of the initial abundance and the sensitivity of a species to random extinctions

# ---- load libraries ----
library(tidyverse)
library(zoo)
library(readxl)

# --- load data ----
rm(list = ls())
load("data/BBS_Bird_df.rda")
load("data/land_use_clustered_complete_df.rda")
eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition)

# --- simple data transformations ----

BBS_df <- BBS_bird %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(
    three_year_window = paste0(year - 1, "_", year + 1),
    occurred = ifelse(
      year == min(year) | year == max(year),
      (seg_abund != 0),
      (seg_abund != 0 | lead(seg_abund != 0) | lag(seg_abund != 0))
    )
  ) %>%
  ungroup() %>%
  mutate(occurred = ifelse(three_year_window %in% c("1999_2001", "2018_2020"), NA, occurred))

rm(BBS_bird)

# filter to keep only terrestrial species
BBS_sub <- BBS_df %>%
  select(-c(2:8)) %>%
  filter(Marine != 1, 
         Freshwater != 1,
         Nocturnal != 1)

# filter for birds present at a segment in 2000-2002
stable_birds <- BBS_sub %>%
  filter(year %in% c(2000:2002)) %>%
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

#get stable birds
birds <- unique(stable_birds$animal_jetz)
segments <- unique(stable_birds$segment)

# only get combinations of birds + segments, that are also present in the first stable data frame
BBS_stable <- BBS_df %>%
  filter(year > 2002) %>%
  semi_join(stable_birds,by = c("animal_jetz", "segment")) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) # 410 846 entries

# ---- get all birds that don't appear from 2017-2019 ----

# filter initial birds to those not present anymore at a segment throughout 2017-2019
disappeared <- BBS_sub %>%
  semi_join(stable_birds, by = c("animal_jetz", "segment")) %>%
  filter(year %in% c(2017:2019)) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund == 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  na.omit() %>%
  group_by(segment, animal_jetz) %>%
  summarize(total_combinations = n()) # there are 669 combinations of species + segment

length(unique(disappeared$animal_jetz)) # 112 species don't appear anymore at a segment in 2017-2019
length(unique(disappeared$segment)) # at 552 segments disappearances happen

# add the disappearance to the stable birds

BBS_stable_extinct_in_2019 <- BBS_stable %>%
  # inner_join(disappeared, by = c("animal_jetz", "segment"))
  left_join(disappeared, by = c("segment", "animal_jetz")) %>%
  mutate(extinct = ifelse(is.na(total_combinations), 0, 1)) %>%
  select(-total_combinations)

# --- find all segments and periods where bird species disappear ----

# get the periods a bird is recorded at a segment
recording_periods <- BBS_stable %>%
  group_by(segment, animal_jetz) %>%
  summarize(
    years_recorded = n_distinct(year),
    first_year = min(year),
    last_year = max(year)
  )

# 908 bird+segment combinations are recorded less than 10 years
min_recording_period <- recording_periods %>%
  filter(years_recorded < 10)

# filter out those bird+segment combinations which are not recorded for the min_recording_period
BBS_stable_sub <- BBS_stable %>%
  anti_join(min_recording_period, by = c("animal_jetz", "segment"))

# check for birds with absences at a segment for >= 3 years
BBS_stable_sub %>%
  group_by(segment, animal_jetz) %>%
  filter(seg_abund == 0 &
           lag(seg_abund) == 0 &
           lead(seg_abund) == 0) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  filter(last_year - first_year >= 3) %>%
  group_by(segment, animal_jetz, .add = T) %>%
  group_split() %>%
  bind_rows()

# birds that are not present anymore in all years post-2002
locally_absent <- BBS_stable_sub %>%
  group_by(segment, animal_jetz) %>%
  filter(any(year > 2002) & 
           all(seg_abund == 0)) %>%
  ungroup()

locally_absent %>%
  group_by(segment, animal_jetz) %>%
  summarize(total_combinations = n()) # 28 combinations of species + segment

length(unique(locally_absent$animal_jetz)) # 24 species
length(unique(locally_absent$segment)) # 27 segments

absence_periods <- BBS_stable_sub %>%
  group_by(segment, animal_jetz) %>%
  filter(seg_abund == 0 &
           lag(seg_abund) == 0 &
           lead(seg_abund) == 0) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  filter(last_year - first_year >= 3) %>%
  group_by(segment, animal_jetz, .add = T) %>%
  group_split() %>%
  bind_rows()

locally_unobserved %>%
  filter(animal_jetz == "Dendroica_discolor")

absent_birds <- unique(locally_extinct$animal_jetz)

BBS_stable %>%
  filter(animal_jetz %in% absent_birds) %>%
  group_by(segment, animal_jetz)


# create data frame with each three year period with a FALSE if the bird is not observed and TRUE if the bird is observed
BBS_periods <- BBS_stable %>%
  group_by(segment, animal_jetz) %>%
  mutate(
    three_year_window = paste0(year - 1, "_", year + 1),
    occurred = ifelse(
      year == min(year) | year == max(year),
      (seg_abund != 0),
      (seg_abund != 0 | lead(seg_abund != 0) | lag(seg_abund != 0))
    )
  ) %>%
  ungroup()


  # pivot_wider(
  #     id_cols = c(segment, animal_jetz),
  #     names_from = three_year_window,
  #     values_from = occurred,
  #     values_fill = NA
  #   ) %>%
  #   rename_with(~ paste0(.), starts_with("2")) %>%
  # #   ungroup() %>%
  # filter(animal_jetz == "Archilochus_colubris",
  #        segment == "02_001_5")

# ---- moving averages for simple abundance trends ----

# Calculate moving averages for the segments abundances

BBS_stable_full <- BBS_stable %>%
  left_join(eco_txt, by = c("segment"))

test <- BBS_stable_full %>%
  group_by(segment, animal_jetz) %>%
  mutate(moving_average = rollmean(seg_abund, k = 3, align = "right", fill = NA)) %>%
  na.omit()

test2 <- BBS_stable_full %>%
  group_by(Ecoregion, animal_jetz) %>%
  mutate(moving_average = rollmean(seg_abund, k = 3, align = "right", fill = NA)) %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  summarize(avg_moving_average = mean(moving_average, na.rm = TRUE))

test2 %>%
  ggplot(aes(x = year, y = avg_moving_average, color = animal_jetz)) +
  geom_smooth() +
  # geom_line() +
  # geom_point() +
  labs(x = "Year", y = "Average Moving Average",
       title = "Abundance trends by bird species for whole US") +
  theme(legend.position = "none")

test %>%
  group_by(year, animal_jetz) %>%
  summarize(avg_moving_average = mean(moving_average, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_moving_average, color = animal_jetz)) +
  geom_smooth() +
  # geom_line() +
  # geom_point() +
  labs(x = "Year", y = "Average Moving Average",
       title = "Abundance trends by bird species for ecoregions") +
  theme(legend.position = "none")


# ---- baustelle -----

# get abundance data for 2017-2019

abund_2017 <- res_abund[[18]]
abund_2018 <- res_abund[[19]]
abund_2019 <- res_abund[[20]]

abund_2017_df <- abund_2017 %>%
  reshape2::melt(varnames = c("segment", "animal_jetz"),
                 value.name = "avg_abundance") %>%
  mutate(year = 2017)

abund_2018_df <- abund_2018 %>%
  reshape2::melt(varnames = c("segment", "animal_jetz"),
                 value.name = "avg_abundance") %>%
  mutate(year = 2018)

abund_2019_df <- abund_2019 %>%
  reshape2::melt(varnames = c("segment", "animal_jetz"),
                 value.name = "avg_abundance") %>%
  mutate(year = 2019)

tmp <- bind_rows(abund_2017_df, abund_2018_df, abund_2019_df)

tmp <- tmp %>%
  filter(!is.na(avg_abundance),
         animal_jetz %in% stable_birds,
         segment %in% initial_segments)

test <- BBS_partition_abundance %>%
  filter(year %in% c(2017:2019),
         animal_jetz %in% stable_birds,
         partition %in% initial_segments) %>%
  group_by(partition, animal_jetz) %>%
  filter(all(seg_abundance))

BBS_stable %>%
  subset(seg_abundance != seg_abund_sum) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

local_extinctions <- BBS_stable %>%
  group_by(segment, animal_jetz) %>%
  filter(seg_abund_sum == 0) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  ungroup() %>%
  filter(last_year - first_year >= 3)


# get birds with multiple entries
mult_entries <- BBS_partition_abundance %>%
  filter(animal_jetz %in% stable_birds,
         year > 2002) %>%
  group_by(year, partition, animal_jetz, seg_abundance) %>%
  summarize(total_entries = n())

# 
first_nonzero <- mult_entries %>%
  group_by(animal_jetz) %>%
  arrange(year, partition) %>%
  filter(seg_abundance != 0) %>%
  slice(1) %>%
  ungroup()

# 
disappeared_species <- mult_entries %>%
  group_by(animal_jetz) %>%
  arrange(year, partition) %>%
  right_join(first_nonzero, by = "animal_jetz") %>%
  filter(is.na(year.x) | (year.x > year.y & partition.x == partition.y)) %>%
  select(animal_jetz, segment = partition.x, year = year.x) %>%
  distinct()
