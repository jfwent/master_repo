# Select the climate variables for the delta abundance models
# Author: Jon Went
# Date: 21.09.2023

# ---- libraries -----

library(tidyverse)

# ---- load data ----

# load("data/d.abund.min6.rda")
# load("data/d.abund.min10.rda")
# load("data/d.abund.min40.rda")

load("data/BBS.full.stable.min40.rda")
load("data/BBS.full.stable.min10.rda")
load("data/BBS.full.stable.min6.rda")

load("data/Climate/climate_df.rda")
load("data/Land_use/land_use_area_t1_t2.rda")

# ---- abundance data ----

d.abund.min6 <- BBS.stable.full.min6 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund =  abund.geom.mean - lag(abund.geom.mean)) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  na.omit()

d.abund.min10 <- BBS.stable.full.min10 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = abund.geom.mean - lag(abund.geom.mean)) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  na.omit()

d.abund.min40 <- BBS.stable.full.min40 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = abund.geom.mean - lag(abund.geom.mean)) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  na.omit()


# ---- climate data ----

climate.df <- climate_df %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean"))

clim.t1 <- climate.df %>% filter(year == 2001) %>% select(-year)

dclim <- climate.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit()

clim.df <- clim.t1 %>% left_join(dclim, by = "segment")

# ecospat::ecospat.cor.plot(clim.df[2:15])

# correlated >0.7:
# cmi.diff.mean + pr.diff.mean
# cmi.annual.mean + pr.sum.mean + swb.mean
# delta.cmi.diff.mean + delta.pr.diff.mean
# delta.cmi.annual.mean + delta.pr.sum.mean + delta.swb.mean

rm(climate_df, climate.df, clim.t1, dclim)

# ---- land use data ----

lc.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2, pasture.area.m2)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"))

lc.t1 <- lc.df %>% filter(year == 2001) %>% select(-year)

dlc <- lc.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('area') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit()

lc.df <- lc.t1 %>% left_join(dlc, by = "segment")

# ecospat::ecospat.cor.plot(lc.df[2:11])
# no correlations

rm(dlc, land_use_area, lc.t1)

# ---- full data sets----

d.abund.climate <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(clim.df, by = c("segment")) #%>%
  # mutate(delta.abund = as.integer(delta.abund))

d.abund.lc <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(lc.df, by = c("segment")) #%>%
  # mutate(delta.abund = as.integer(delta.abund))

# ---- LM function ----

fit_lm <- function(df, var) {
  formula <- as.formula(paste("delta.abund ~", var, "+ I(", var, "^2)"))
  model <- lm(formula, data = df)
  return(model)
}

# ---- glmm models climate variables ----

climate_models <- list()
birds <- sort(unique(d.abund.climate$animal_jetz))
climate_vars <- colnames(d.abund.climate[4:17])

for(variable.ind in climate_vars){
  
  print(paste0("working on ", which(climate_vars == variable.ind), "/", length(climate_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- d.abund.climate %>%
      filter(animal_jetz == bird)
    model <- fit_lm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  climate_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, climate_vars, birds)

# ---- glmm models land use variables ----

land_use_models <- list()
birds <- sort(unique(d.abund.lc$animal_jetz))
land_use_vars <- colnames(d.abund.lc[4:13])

for(variable.ind in land_use_vars){
  
  print(paste0("working on ", which(land_use_vars == variable.ind), "/", length(land_use_vars), " ..."))
  
  bird_models <- list()
  
  bird_models <- lapply(birds, function(bird) {
    bird.tmp <- d.abund.lc %>%
      filter(animal_jetz == bird)
    model <- fit_lm(bird.tmp, variable.ind)
    return(model)
  })
  
  names(bird_models) <- birds
  
  land_use_models[[variable.ind]] <- bird_models
}

rm(bird_models, variable.ind, land_use_vars, birds)


# ---- d2 climate variables ----

climate_vars <- colnames(d.abund.climate[5:11])
birds <- unique(d.abund.climate$animal_jetz)

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

