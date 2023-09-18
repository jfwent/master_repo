# GAM Model
# Author: Jon Went
# Date: 15.09.2023

# ---- libraries -----

library(tidyverse)
library(mgcv)
library(modelr)

# ---- load data -----

load("data/BBS.full.stable.min40.rda")
load("data/BBS.full.stable.min10.rda")

# ---- build models -----

# ---- test area ----

df.tmp <- BBS.stable.full.min10 %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  filter(animal_jetz %in% "Agelaius_phoeniceus", year %in% 2001)

folds <- crossv_kfold(df.tmp, k = 10)

cv_df <-
  folds %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

cv_df <-
  cv_df %>%
  mutate(
    smooth_mod  = map(train, ~mgcv::gam(abund.geom.mean ~ s(pr.diff.mean, tmax.mean,
                                                            tmin.mean, swb.mean,
                                                            bs = "tp") +
                                          s(hfp.mean, bs = "tp", k = 3) +
                                          s(urban.low.area.m2.log, urban.high.area.m2.log,
                                            forest.area.m2.log, pasture.area.m2.log,
                                            grass.area.m2.log, crop.area.m2.log,
                                            wet.area.m2.log, barren.area.m2.log,
                                            bs = "tp"),
                                        family = poisson, method = "REML", data = .x)),
  ) %>%
  mutate(
    rmse_smooth = map2_dbl(smooth_mod, test, ~rmse(model = .x, data = .y)),
  )

cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()

# min 10----

BBS.min10 <- BBS.stable.full.min10 %>% mutate(abund.geom.mean = as.integer(abund.geom.mean))

birds <- sort(unique(BBS.stable.full.min10$animal_jetz))

gam_models <- list()

for(year.ind in years){
  
  for(bird in birds){
    
    df.tmp <- BBS.min10 %>% filter(animal_jetz %in% bird, year %in% year.ind)
    
    gam_model_k3_reml <- gam(abund.geom.mean ~ s(pr.diff.mean, tmax.mean,
                                                 tmin.mean, swb.mean,
                                                 bs = "tp", k = 3) +
                               # s(urban.low.area.m2.log, urban.high.area.m2.log,
                               #   forest.area.m2.log, pasture.area.m2.log,
                               #   grass.area.m2.log, crop.area.m2.log,
                               #   wet.area.m2.log, barren.area.m2.log,
                               #   bs = "tp", k = 3) +
                               s(hfp.mean, bs = "tp", k = 3),
                             data = df.tmp, family = poisson, method = "REML")
    
    # cv_results <- cv.gam(gam_model_k3_reml, K = 10)
    
    gam_model_kest_reml <- gam(abund.geom.mean ~ s(pr.diff.mean, tmax.mean,
                                                   tmin.mean, swb.mean,
                                                   bs = "tp", k = 9) +
                                 s(urban.low.area.m2.log, urban.high.area.m2.log,
                                   forest.area.m2.log, pasture.area.m2.log,
                                   grass.area.m2.log, crop.area.m2.log,
                                   wet.area.m2.log, barren.area.m2.log,
                                   bs = "tp", k = 9) +
                                 s(hfp.mean, bs = "tp", k = 9),
                               data = your_data, family = poisson, method = "REML")
  }
}




# min 40----

