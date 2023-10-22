# Explore initial commonness 
# Date: 13.10.2023

# --- library
library(tidyverse)
library(readxl)
library(ggplot2)

load("data/BBS.full.stable.min40.rda")

# ---- 

init.commonness <- read_xlsx("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/gcb16700-sup-0002-supinfo2.xlsx",
                             ) %>%
  mutate(animal_jetz = gsub(" ", "_", scientific.name)) %>%
  relocate(animal_jetz) %>%
  select(-c(english, scientific.name))

my.init.common <- BBS.stable.full.min40 %>%
  filter(year == "2001") %>%
  group_by(animal_jetz) %>%
  summarize(abund.sum = sum(abund.geom.mean)) %>%
  mutate(abund.sum.log = log(abund.sum)) %>%
  ungroup() %>%
  mutate(abundance_groups = cut(abund.sum,
                                 breaks = c(0, 100, 1000, 10000, 100000),
                                 labels = c("Rare", "Less Common", "Common", "Superabundant"))) %>%
  mutate(abund.group.log = cut(abund.sum.log,
                               breaks = c(0,5, 7, 9, 10),
                               labels = c("Rare", "Less Common", "Common", "Superabundant")))

init.commonness %>%
  left_join(my.init.common, by = "animal_jetz") %>%
  ggplot(aes(x = log(abund.sum), y = log(total.abund.2001.allrt))) +
  geom_point() +
  geom_smooth(method = "lm")


