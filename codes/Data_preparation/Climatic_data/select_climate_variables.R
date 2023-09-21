# Select climate variables
# Author: Jon Went, jwent@ethz.ch
# Date: 1.9.2023

# ---- libraries ----

library(tidyverse)
# library(lme4)
# library(ecospat)
# library(gam)

# ---- load data ----

load("data/BBS.full.stable.min6.rda")
load("data/BBS.full.stable.min10.rda")
load("data/BBS.full.stable.min40.rda")

load("data/Climate/climate_df.rda")
load("data/hfp_t1_t2.rda")
load("data/Land_use/land_use_area_t1_t2.rda")

# load("data/elevation/elevation.rda")
# load("data/BBS_stable_t1_t2.rda")

# ---- correlations of the the bioclim data ---- 

climate.df <- climate_df %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean")); rm(climate_df)

# length(unique(climate.df$segment)) # 3326 segments, but contains the segments 2 and 4 for each route, which we discard

# ecospat::ecospat.cor.plot(climate.df[3:9])
# cmi.annual.mean + pr.sum.mean = 0.9
# cmi.annual.mean + swb.mean = 0.89
# cmi.diff.mean + pr.diff.mean = 0.98
# pr.sum.mean + swb.mean = 0.75
# --> choose one: cmi.annual, pr.sum, swb.mean
# --> swb.mean has the best predictive power and similar spread to cmi.annual
# pr.diff.mean better predictive power (higher median, lower mean) compared to cmi.diff.mean
# cmi.diff.mean potentially the better ecologically meaningful variable

hfp.df <- hfp.full %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

# length(unique(hfp.df$segment)) # 3326 segments, but contains the segments 2 and 4 for each route, which we discard

# length(unique(land_use_area$segment)) # 1923 segments

land.use.df <- hfp.df %>%
  # filter(year == 2001) %>%
  left_join(land_use_area, by = c("segment", "year")) %>%
  select(-c(ecoregion, tot.area.m2, route)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"), contains("hfp")); rm(land_use_area, hfp.df)

# hist(land.use.df$`urban.low.area.m2-----log`)
# hist(land.use.df$`urban.high.area.m2-----log`) # still left skewed
# hist(land.use.df$`forest.area.m2-----log`)
# hist(land.use.df$`grass.area.m2-----log`) # still left skewed
# hist(land.use.df$`pasture.area.m2-----log`) # still left skewed
# hist(land.use.df$`crop.area.m2-----log`) # still left skewed
# hist(land.use.df$`wet.area.m2-----log`) # still left skewed
# hist(land.use.df$`barren.area.m2-----log`) # still left skewed

ecospat::ecospat.cor.plot(land.use.df[,3:11])
# no correlations

# bioclim.df <- climate.df %>%
#   left_join(land.use.df, by = c("year", "segment"))

# ---- data sets with all climate and land use variables ----

BBS.climate <- BBS.stable.full.min40 %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  left_join(climate.df, by = c("segment", "year")) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean))

BBS.land.use <- BBS.stable.full.min40 %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  left_join(land.use.df, by = c("segment", "year"))  %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean))

rm(BBS.stable.full.min10, BBS.stable.full.min6, BBS.stable.full.min40)
rm(climate.df, land.use.df)

# ---- build glmm function  ----

# for each species we build a glm according to species~var+var^2
# then we extract the d2
# we calculate the mean and median d2 for each variable across all species
# then we correlate the variables and select all parameters that have an index < 0.7

fit_glm <- function(df, var) {
  formula <- as.formula(paste("abund.geom.mean ~", var, "+ I(", var, "^2)"))
  model <- glm(formula, data = df, family = poisson)
  return(model)
}

# ---- glmm models climate variables ----

climate_models <- list()
birds <- sort(unique(BBS.climate$animal_jetz))
climate_vars <- colnames(BBS.climate[5:11])

for(variable.ind in climate_vars){
  
  print(paste0("working on ", which(climate_vars == variable.ind), "/", length(climate_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- BBS.climate %>%
      filter(animal_jetz == bird)
    model <- fit_glm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  climate_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, climate_vars, birds)

# ---- glmm models land use variables ----

land_use_models <- list()
birds <- sort(unique(BBS.land.use$animal_jetz))
land_use_vars <- colnames(BBS.land.use[5:13])

for(variable.ind in land_use_vars){
  
  print(paste0("working on ", which(land_use_vars == variable.ind), "/", length(land_use_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- BBS.land.use %>%
      filter(animal_jetz == bird)
    model <- fit_glm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  land_use_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, land_use_vars, birds)


# ---- d2 climate variables ----

climate_vars <- colnames(BBS.climate[5:11])
birds <- unique(BBS.climate$animal_jetz)

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

rm(climate_vars, i, birds, variable.ind, d2s_climate,
   bird_models, climate_models, var.df, var.list)

# ---- d2 for land use variables ----

land_use_vars <- colnames(BBS.land.use[5:13])

birds <- unique(BBS.land.use$animal_jetz)

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

rm(land_use_vars, i, variable.ind, var.df, var.list, birds, land_use_models, d2s_land_use)

# ---- build climate boxplots  ----

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
  )+
  stat_summary(
    fun.data = mean_se, # Use mean and standard error
    geom = "point",
    color = "black",
    size = 1.5
  ) +
  theme(
    # legend.key.size = unit(5, "mm"),
    legend.text = element_text(size = 7, color = "black"),
    legend.margin = margin(t=-0.6, unit = "cm"),
    legend.position = "right",
    legend.key.width = unit(5, "mm"),
  )

bp

# ggplot2::ggsave(filename = "figures/ClimVar_bp_d2_min40_t1.png", plot = bp, width = 8, height = 6, dpi = 300)

rm(var.df, i, variable.ind, bird_models, var.list)

# ---- build land use boxplots ----

bp2 <- adj_d2_land_use_models %>%
  reshape2::melt(id.vars = c("birds")) %>%
  select(-variable) %>%
  rename(d2 = value,
         variable = L1) %>%
  ggplot2::ggplot(aes(y = variable, x = d2, fill = variable)) +
  geom_boxplot(width = 0.3,
               outlier.size = 1,
               alpha = 0.5) +
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
  ) +
  stat_summary(
    fun.data = mean_se, # Use mean and standard error
    geom = "point",
    color = "black",
    size = 1.5
  ) +
  coord_cartesian(xlim = c(0, 0.25))

bp2

# ggplot2::ggsave(filename = "figures/LCVar_bp_d2_min40_t1_t2.png", plot = bp2, width = 8, height = 6, dpi = 300)


rm(d2s_mean, var.df, i, variable.ind, vars_mean, bird_models, var.list)

# ============ old -----

dbioclim <- bioclim.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  reframe(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), lag(.) - ., NA),
      .names = "delta.{col}"
    )
  )

BBS_climate <- BBS.stable.full.min40 %>%
  filter(year == 2001) %>%
  left_join(climate.df, by = c("year", "segment")) %>%
  select(-c(abund.median, abund.var, abund.mean)) %>%
  arrange(animal_jetz) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean))

# summary(BBS_climate)
# 14693 NA's in in every climate variable in 2001

BBS_land_use <- BBS.stable.full.min40 %>%
  # filter(year == 2001) %>%
  select(-c(abund.median, abund.var, abund.mean)) %>%
  left_join(land.use.df, by = c("year", "segment")) %>%
  arrange(animal_jetz) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean))

# summary(BBS_land_use)
# 18347 NA's in hfp
# 16042 NA's in every land use variable

BBS.climate <- BBS_climate %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  select(-tot_entries, -tot_occur)

# length(unique(BBS.climate$segment))
# length(unique(BBS.climate$animal_jetz))

BBS.land.use <- BBS_land_use %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  select(-tot_entries, -tot_occur)

rm(BBS_climate, BBS_land_use)
