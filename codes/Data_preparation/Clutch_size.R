# Clutch sizes, Jetz et al., 2008
# Author: Jon Went
# Date: 27.09.2023

# ---- 

# install.packages("pdftools")
# library(pdftools)
library(tidyverse)
# library(readr)
library(readxl)

# ----

clutch_dat <- read_xlsx("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/pbio.0060303.st004.xlsx",
                        skip = 1) %>%
  select(Order, Family, Species, Clutch, Range, Traits) %>%
  filter(Order != "Order",
         Family != "Family",
         Species != "Species",
         Clutch != "Clutch",
         Range != "Range",
         Traits != "Traits") %>%
  mutate(Clutch = as.numeric(Clutch),
         Range = as.numeric(Range)) %>%
  mutate(animal_jetz = gsub(" ", "_", Species)) %>%
  relocate(animal_jetz, .after = Species)

save(clutch_dat, file = "data/Clutch_sizes/clutch_sizes.rda")


