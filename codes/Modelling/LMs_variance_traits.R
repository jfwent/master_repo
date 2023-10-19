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
load("results/beta_coefs_univar_models.rda")

# ---- prepare data ----

species.traits <- species.traits %>%
  rename(bird =  animal_jetz) %>%
  dplyr::select(-c(Common.Name, tot_diet_div, shannon, Clutch, abundance_groups))

adj_r2_lc_traits <- adj_r2_lc %>%
  left_join(species.traits, by = "bird") %>%
  distinct() %>%
  rowwise() %>%
  mutate(v.clim = adj.r2 - adj.r2_lc,
         v.lc = adj.r2 - adj.r2_clim,
         v.joint = adj.r2 - (adj.r2_lc + adj.r2_clim))

coefs_tib <- univar_coefs %>%
  na.omit() %>%
  filter(!(variable %in% "(Intercept)")) %>%
  pivot_wider(id_cols = bird, names_from = variable, values_from = beta.coefs) %>%
  select(bird, contains("delta")) %>%
  left_join(species.traits, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num < 13) %>%
  select(-c(contains("crop"), na.num))

# ---- LMs ----

traits <- colnames(species.traits[2:15])
model_types <- colnames(adj_r2_lc_traits[19:21])

variance_models <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(model_types),
  clear = FALSE
)

train_control <- caret::trainControl(method = "LOOCV", savePredictions = "final")

set.seed(123)

for(mod.types in model_types){
  
  pb$tick()
  
  trait_mods <- list()
  
  for(trait.ind in traits){
    
    trait.tmp <- adj_r2_lc_traits %>%
      select(bird, all_of(mod.types), all_of(trait.ind)) %>%
      na.omit()
    
    form.tmp <- as.formula(paste(mod.types, "~", trait.ind))
    
    model.tmp <- caret::train(
      form.tmp,
      data = trait.tmp,
      method = "lm",
      trControl = train_control,
      preProcess = c("center", "scale", "zv")
    )
    
    trait_mods[[trait.ind]] <- model.tmp
  }
  
  variance_models[[mod.types]] <- trait_mods
}

rm(pb, train_control, trait_mods, trait.tmp, form.tmp, mod.types, model_types, trait.ind, traits, model.tmp)

# ---- find p-values ----

traits <- colnames(species.traits[2:15])
model_types <- colnames(adj_r2_lc_traits[19:21])

trait_LM_res <- tibble(trait = character(),
                       model_type = character(),
                       beta.coefs = numeric(),
                       p.val = numeric())

for(model.type.ind in model_types){
  
  for(trait.ind in traits){
    
    mod.tmp <- variance_models[[model.type.ind]][[trait.ind]]
    
    beta.coefs <- coefficients(mod.tmp$finalModel)
    
    p.val <- summary(mod.tmp)$coefficients[,4]
    
    tib_tmp <- tibble(trait = trait.ind,
                      model_type = model.type.ind,
                      beta.coefs = beta.coefs[2],
                      p.val = p.val[2])
    
    trait_LM_res <- bind_rows(trait_LM_res, tib_tmp)
  }
}

trait_LM_res %>%
  filter(p.val < 0.1) %>%
  arrange(p.val)

save(trait_LM_res, file = "results/traits_variance_LMs.rda")

rm(mod.tmp, tib_tmp, beta.coefs, model_types, model.type.ind, p.val, trait.ind, traits)

# ---- beta coefs vs traits models ---- 
# ----- LMs ----

traits <- colnames(species.traits[2:15])
model_types <- colnames(coefs_tib[2:9])

beta_models <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(model_types),
  clear = FALSE
)

train_control <- caret::trainControl(method = "LOOCV", savePredictions = "final")

set.seed(123)

for(mod.types in model_types){
  
  pb$tick()
  
  trait_mods <- list()
  
  for(trait.ind in traits){
    
    trait.tmp <- coefs_tib %>%
      select(bird, all_of(mod.types), all_of(trait.ind)) %>%
      na.omit()
    
    form.tmp <- as.formula(paste(mod.types, "~", trait.ind))
    
    model.tmp <- caret::train(
      form.tmp,
      data = trait.tmp,
      method = "lm",
      trControl = train_control,
      preProcess = c("center", "scale", "zv")
    )
    
    trait_mods[[trait.ind]] <- model.tmp
  }
  
  beta_models[[mod.types]] <- trait_mods
}

rm(pb, train_control, trait_mods, trait.tmp, form.tmp, mod.types, model_types, trait.ind, traits, model.tmp)


# ---- find p values ----

traits <- colnames(species.traits[2:15])
model_types <- colnames(coefs_tib[2:9])

beta_LM_res <- tibble(trait = character(),
                       model_type = character(),
                       beta.coefs = numeric(),
                       p.val = numeric())

for(model.type.ind in model_types){
  
  for(trait.ind in traits){
    
    mod.tmp <- beta_models[[model.type.ind]][[trait.ind]]
    
    beta.coefs <- coefficients(mod.tmp$finalModel)
    
    p.val <- summary(mod.tmp)$coefficients[,4]
    
    tib_tmp <- tibble(trait = trait.ind,
                      model_type = model.type.ind,
                      beta.coefs = beta.coefs[2],
                      p.val = p.val[2])
    
    beta_LM_res <- bind_rows(beta_LM_res, tib_tmp)
  }
}

beta_LM_res %>%
  filter(p.val < 0.1) %>%
  arrange(p.val)

save(beta_LM_res, file = "results/traits_beta_coefs_LMs.rda")

rm(mod.tmp, tib_tmp, beta.coefs, model_types, model.type.ind, p.val, trait.ind, traits)

