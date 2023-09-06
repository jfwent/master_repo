# Select climate variables
# Author: Jon Went, jwent@ethz.ch
# Date: 1.9.2023

# --- libraries ----

library(tidyverse)
# library(lme4)
# library(ecospat)
# library(gam)

# ---- load data ----

load("data/Climate/climate_df.rda")
load("data/BBS_stable_t1_t2.rda")
load("data/hfp_t1_t2.rda")
load("data/Land_use/land_use_area_t1_t2.rda")
# load("data/elevation/elevation.rda")

# ---- build data sets ----

# elevation_data <- elevation_data %>%
#   select(partition, elev.mean) %>%
#   rename(segment = partition)

climate.df <- climate_df %>%
  filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean"))

hfp.df <- hfp.full %>%
  filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean))

land.use.df <- land_use_area %>%
  filter(year == 2001) %>%
  select(-c(ecoregion, tot.area.m2, route)) %>%
  left_join(hfp.df, by = c("segment", "year"))

ecospat::ecospat.cor.plot(climate.df[3:9])
# cmi.annual.mean + pr.sum.mean = 0.9
# cmi.annual.mean + swb.mean = 0.89
# cmi.diff.mean + pr.diff.mean = 0.98
# pr.sum.mean + swb.mean = 0.75
# --> choose one: cmi.annual, pr.sum, swb.mean
# --> swb.mean has the best predictive power and similar spread to cmi.annual

# ecospat::ecospat.cor.plot(land.use.df[3:11])
# no correlations

BBS_climate_means <- BBS.stable.full %>%
  filter(year == 2001) %>%
  left_join(climate.df, by = c("year", "segment")) %>%
  select(-abund.median, -abund.var)

BBS_land_use_means <- BBS.stable.full %>%
  filter(year == 2001) %>%
  select(-abund.median, -abund.var) %>%
  left_join(land.use.df, by = c("year", "segment")) %>%
  arrange(animal_jetz)

# summary(BBS_climate_means)
# 15446 NA's in in every climate variable in 2001

# summary(BBS_land_use_means)
# 19674 NA's in hfp
# 16800 NA's in every land use variable

BBS.climate <- BBS_climate_means %>%
  na.omit()

BBS.land.use <- BBS_land_use_means %>%
  na.omit()

rm(BBS_climate_means, BBS_land_use_means)
rm(climate_df, climate.df, land_use_area, land.use.df, hfp.df, hfp.full, BBS.stable.full)

# ---- build glmm function  ----

# for each species we build a glm according to species~var+var^2
# then we extract the d2
# we calculate the mean and median d2 for each variable across all species
# then we correlate the variables and select all parameters that have an index < 0.7

fit_mean_glm <- function(df, var) {
  formula <- as.formula(paste("abund.mean ~", var, "+ I(", var, "^2)"))
  model <- glm(formula, data = df, family = poisson)
  return(model)
}

#---- glmm models climate variables ----

climate_models <- list()
birds <- sort(unique(BBS.climate$animal_jetz))
climate_vars <- colnames(BBS.climate[5:11])

for(variable.ind in climate_vars){
  
  print(paste0("working on ", which(climate_vars == variable.ind), "/", length(climate_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- BBS.climate %>%
      filter(animal_jetz == bird)
    model <- fit_mean_glm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  climate_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, climate_vars, fit_mean_glm)

# ---- adjusted d2 climate variables ----

climate_vars <- colnames(BBS.climate[5:11])

adj_d2_climate_models <- list()

for(variable.ind in climate_vars){
  
  bird_models <- climate_models[[variable.ind]]
  
  adj.d2 <- rep(NA, length(birds))
  
  for (i in seq_along(bird_models)) {
    
    go <- bird_models[[i]]
    D2 <- (go$null.deviance - go$deviance)/go$null.deviance
    p <- length(go$coefficients)
    n <- length(go$fitted)
    adj.d2[i] <- (1 - ((n - 1)/(n - p)) * (1 - D2))
    
    if (adj.d2[i] < 0 | is.na(adj.d2[i]) | is.infinite(adj.d2[i]) == T) {
      adj.d2[i] <- 0
    }
  }
  
  # names(adj.d2) <- birds
  entry <- as.data.frame(matrix(NA, nrow = length(birds), ncol = 2)) %>%
    mutate(birds = birds,
           adj.d2 = adj.d2) %>%
    select(-V1, -V2)
  
  adj_d2_climate_models[[variable.ind]] <- entry
}

rm(adj.d2, entry, go, D2, n, p)

d2s_climate <- list()

for(variable.ind in climate_vars){
  
  var.list <- adj_d2_climate_models[[variable.ind]]
  
  var.df <- var.list %>%
    reshape2::melt() %>%
    rename(d2 = value) %>%
    summarize(d2.mean = mean(d2),
              d2.median = median(d2))
  
  d2s_climate[[variable.ind]] <- var.df
  
}

d2_climate_df <- do.call(rbind,d2s_climate)
d2_climate_df$variable <- rownames(d2_climate_df)
rownames(d2_climate_df) <- NULL 
d2_climate_df <- d2_climate_df %>%
  relocate(variable)

rm(climate_vars)

# --- build climate boxplots  ----

adj_d2_climate_models %>%
  reshape2::melt(id.vars = c("birds")) %>%
  select(-variable) %>%
  rename(d2 = value,
         variable = L1) %>%
  # pivot_wider(id_cols = "birds", names_from = "variable", values_from = "d2")
  ggplot2::ggplot(aes(y = variable, x = d2, fill = variable)) +
  geom_boxplot(width = 0.3,
               outlier.size = 1,
               alpha = 0.5) +
  # ggdist::geom_dots(side = "bottom",
  #           position = position_nudge(y = -0.075),
  #           height = 0.55) +
  geom_point(
    aes (color = variable),
    shape = "|",
    size = 1.8,
    alpha = 0.7,
    position = position_nudge(y = -0.275)
  ) +
  ggdist::stat_slab(
    position = position_nudge(y = 0.200),
    height = 0.55
  )
  # facet_wrap(~ birds, scales = "free_y", ncol = 2)
  
rm(d2s_mean, var.df, i, variable.ind, vars_mean, bird_models, var.list)

# --- glmm models land use variables ----

land_use_models <- list()
birds <- sort(unique(BBS.land.use$animal_jetz))
land_use_vars <- colnames(BBS.land.use[5:13])

for(variable.ind in land_use_vars){
  
  print(paste0("working on ", which(land_use_vars == variable.ind), "/", length(land_use_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- BBS.land.use %>%
      filter(animal_jetz == bird)
    model <- fit_mean_glm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  land_use_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, land_use_vars, fit_mean_glm)

# --- d2 for land use variables ----

land_use_vars <- colnames(BBS.land.use[5:13])

adj_d2_land_use_models <- list()

for(variable.ind in land_use_vars){
  
  bird_models <- land_use_models[[variable.ind]]
  
  adj.d2 <- rep(NA, length(birds))
  
  for (i in seq_along(bird_models)) {
    
    go <- bird_models[[i]]
    D2 <- (go$null.deviance - go$deviance)/go$null.deviance
    p <- length(go$coefficients)
    n <- length(go$fitted)
    adj.d2[i] <- (1 - ((n - 1)/(n - p)) * (1 - D2))
    
    if (adj.d2[i] < 0 | is.na(adj.d2[i]) | is.infinite(adj.d2[i]) == T | adj.d2[i] == 2) {
      adj.d2[i] <- 0
    }
  }
  
  # names(adj.d2) <- birds
  entry <- as.data.frame(matrix(NA, nrow = length(birds), ncol = 2)) %>%
    mutate(birds = birds,
           adj.d2 = adj.d2) %>%
    select(-V1, -V2)
  
  adj_d2_land_use_models[[variable.ind]] <- entry
}

rm(adj.d2, entry, go, D2, n, p)

d2s_land_use <- list()

for(variable.ind in land_use_vars){
  
  var.list <- adj_d2_land_use_models[[variable.ind]]
  
  var.df <- var.list %>%
    reshape2::melt() %>%
    rename(d2 = value) %>%
    summarize(d2.mean = mean(d2),
              d2.median = median(d2))
  
  d2s_land_use[[variable.ind]] <- var.df
  
}

d2_land_use_df <- do.call(rbind, d2s_land_use)
d2_land_use_df$variable <- rownames(d2_land_use_df)
rownames(d2_land_use_df) <- NULL 
d2_land_use_df <- d2_land_use_df %>%
  relocate(variable)

rm(climate_vars)

#---- build land use boxplots ----

adj_d2_land_use_models %>%
  reshape2::melt(id.vars = c("birds")) %>%
  select(-variable) %>%
  rename(d2 = value,
         variable = L1) %>%
  # pivot_wider(id_cols = "birds", names_from = "variable", values_from = "d2")
  ggplot2::ggplot(aes(y = variable, x = d2, fill = variable)) +
  geom_boxplot(width = 0.3,
               outlier.size = 1,
               alpha = 0.5) +
  # ggdist::geom_dots(side = "bottom",
  #           position = position_nudge(y = -0.075),
  #           height = 0.55) +
  geom_point(
    aes (color = variable),
    shape = "|",
    size = 1.8,
    alpha = 0.7,
    position = position_nudge(y = -0.275)
  ) +
  ggdist::stat_slab(
    position = position_nudge(y = 0.200),
    height = 0.55
  )
# facet_wrap(~ birds, scales = "free_y", ncol = 2)

rm(d2s_mean, var.df, i, variable.ind, vars_mean, bird_models, var.list)
