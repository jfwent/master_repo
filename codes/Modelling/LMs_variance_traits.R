# LMs for variance ~ traits 
# Author: Jon Went
# Date: 12.10.2023

# ---- libraries ----

library(tidyverse)
library(caret)
library(progress)

# ---- load data ---- 

load("results/LMs_swb/LMs_no_crop/adj_R2_trained_LMs.rda")
# load("results/LMs_swb/LMs_no_crop/beta_coefficients_full_LMs.rda")
load("data/species_traits.rda")

# ---- prepare data ----

species.traits <- species.traits %>%
  rename(bird =  animal_jetz) %>%
  dplyr::select(-c(Common.Name, tot_diet_div, shannon, Clutch))

adj_r2_lc_traits <- adj_r2_lc %>%
  left_join(species.traits, by = "bird") %>%
  distinct()

# ---- LMs ----

traits <- colnames(species.traits[2:14])
model_types <- 

traits_models <- list()


pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

set.seed(123)

for()
for(trait.ind in traits){
  
}