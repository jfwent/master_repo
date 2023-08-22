# Segment based species level models 
# Author: Jon Went
# Date: 27.07.2023

# ---- start with the 0_start file

# ---- filter stable initial community
initial_birds <- BBS_final_area %>%
  filter(year %in% c(2000:2002)) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries >= 3) %>%
  summarize(avg_abundance = mean(seg_abund)) %>%
  mutate(avg_abund = round(avg_abundance,1)) %>%
  select(-avg_abundance) %>%
  na.omit()
  
# --- filter whole BBS for the stable community
BBS_stable <- BBS_final_area %>%
  inner_join(initial_birds, by = c("segment", "animal_jetz")) %>%
  select(-avg_abund)

BBS_end_present <- BBS_stable %>%
  filter(year %in% c(2017:2019)) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries >= 3) %>%
  summarize(avg_abundance = mean(seg_abund)) %>%
  mutate(avg_abund = round(avg_abundance,1)) %>%
  select(-avg_abundance) %>%
  na.omit()

BBS_end_absent <- BBS_stable %>%
  filter(year %in% c(2017:2019)) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund == 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  summarize(total_entries = n()) %>%
  filter(total_entries >= 3) %>%
  na.omit()

# ---- get three year windows with occurrence = T/F ----
BBS_df <- BBS_stable %>%
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
  mutate(occurred = ifelse(three_year_window %in% c("1999_2001", "2018_2020", "2020_2022"), NA, occurred))



