# Build Delta-Abundance Model
# Author: Jon Went
# Date: 14.09.2023

# ---- library ----
library(tidyverse)

# ---- load data ----

load("data/d.abund.min6.rda")
load("data/d.abund.min10.rda")
load("data/d.abund.min40.rda")

load("data/Climate/climate_df.rda")
load("data/Land_use/land_use_area_t1_t2.rda")

# ---- bioclim data ----

climate.df <- climate_df %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, pr.diff.mean))

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

clim.df <- clim.t1 %>% left_join(dclim, by = "segment"); rm(climate_df, climate.df, clim.t1, dclim)

lc.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2,
            pasture.area.m2)) %>%
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

rm(dlc, land_use_area, lc.t1)

# ---- full data set ----

abund.min40 <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(clim.df, by = "segment") %>%
  left_join(lc.df, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries >= 40) %>%
  select(-n_entries)

# ---- LM function -----

fit_lm <- function(df, vars) {
  formula_string <- paste("delta.abund ~", paste(vars, collapse = " + "))
  formula <- as.formula(formula_string)
  # formula <- as.formula(paste(formula_string, "+", paste("I(", vars, "^2)", collapse = " + ")))
  model <- lm(formula, data = df)
  return(model)
}

birds <- unique(abund.min40$animal_jetz)
vars.all <- colnames(abund.min40[4:19])

clim_vars <- colnames(abund.min40[4:11])
lc_vars <- colnames(abund.min40[12:19])

# formula_string <- paste("delta.abund ~", paste(vars, collapse = " + "))
# formula_string <- as.formula(paste(formula_string, "+", paste("I(", vars, "^2)", collapse = " + ")))
# lm_formula <- formula(formula_string)

num_folds <- 10

LM_list <- list()

rmse_tib <- tibble(bird = character(),
                    mean_rmse = numeric(),
                    n.folds = numeric())

adj_r2_tib <- tibble(bird = character(),
                  adj.r2 = numeric())
  
for(bird.ind in birds){
  
  print(paste0("working on ", which(birds == bird.ind), "/", length(birds), " ..."))
  
  bird.tmp <- abund.min40 %>%
    filter(animal_jetz == bird.ind) %>%
    tibble::rowid_to_column(., "ID")
  
  full.mod <- fit_lm(bird.tmp, vars = vars.all)
  adj.r2.now <- summary(full.mod)$adj.r.squared
  
  lc.mod <- fit_lm(bird.tmp, vars = lc_vars)
  adj.r2.lc <- summary(lc.mod)$adj.r.squared
  
  clim.mod <- fit_lm(bird.tmp, vars = clim_vars)
  adj.r2.clim <- summary(clim.mod)$adj.r.squared
  
  residuals.now <- adj.r2.now - (adj.r2.clim + adj.r2.lc)
  
  LM_list[[bird.ind]] <- full.mod
  
  folds <- caret::groupKFold(bird.tmp$segment, k = 10)
  
  rmse_values <- numeric(num_folds)
  
  for(n_fold in seq_along(folds)){
    
    bird.train <- bird.tmp %>%
      filter(ID %in% folds[[n_fold]])
    
    bird.test <- bird.tmp %>%
      filter(!(ID %in% folds[[n_fold]]))
    
    bird_model <- fit_lm(bird.train, vars = vars.all)
    
    predicted_values <- predict(bird_model, newdata = bird.test)
    
    rmse <- sqrt(mean((bird.train$delta.abund - predicted_values)^2))
    
    rmse_values[n_fold] <- rmse
  }
  
  mean_rmse <- mean(rmse_values)
  
  new_row1 <- tibble(bird = bird.ind,
                     adj.r2 = adj.r2.now,
                     adj.r2_clim = adj.r2.clim,
                     adj.r2_lc = adj.r2.lc,
                     residuals = residuals.now)
  
  new_row2 <- tibble(bird = bird.ind, mean_rmse = mean_rmse,
                     n.folds = length(folds))
  
  adj_r2_tib <- bind_rows(adj_r2_tib, new_row1)
  rmse_tib <- bind_rows(rmse_tib, new_row2)
}

summary(LM_list[[1]])

rm(bird_model, bird.test, bird.train, bird.tmp, clim.df,
   clim.mod, folds, full.mod, lc.df, lc.mod, new_row1, new_row2,
   adj.r2.clim, adj.r2.lc, adj.r2.now, bird.ind, clim_vars, lc_vars, lm_formula,
   mean_rmse, n_fold, num_folds, predicted_values, residuals.now, rmse, rmse_values, vars, vars.all)

# ---- plot residuals with species traits ----

load("data/species_traits.rda")

species.traits <- species.traits %>% rename(bird =  animal_jetz)

adj_r2_tib_plot <- adj_r2_tib %>% left_join(species.traits, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 14) %>%
  select(-na.num)


# ---- continuous traits plots ----

p1 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Residuals") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p2 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p3 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p4 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("Residuals")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p5 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p6 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6

final_plot

# ---- categorical traits ----

p1 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  ylab("Residuals") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p2 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p3 <- ggplot(adj_r2_tib_plot, aes(y = residuals, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  xlab("Trophic Level") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

boxplot <- p1 + p2 + p3

boxplot

# ========== Old ----
# ---- prepare the bioclim data ---- 

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median"),
            pr.sum.mean, cmi.annual.mean, cmi.diff.mean),
         contains("mean")); rm(climate_df)

hfp.df <- hfp.full %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- hfp.df %>%
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

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")); rm(climate.df, land.use.df)

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
  ); rm(bioclim.df)

dclim <- dbioclim %>% select(segment, contains("mean"), -contains("hfp")) %>% na.omit()

dland.use <- dbioclim %>% select(segment, contains("log"), contains("hfp")) %>% na.omit()

# ---- dabund + climate datasets -----

d.abundmin6.clim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin10.clim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin40.clim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

rm(dclim)

# ---- d.abund + land use datasets ----

d.abundmin6.land.use <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin10.land.use <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin40.land.use <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

rm(dland.use)

# ---- d.abund full datasets ----

d.abundmin6.bioclim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin10.bioclim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin40.bioclim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

rm(dbioclim)
rm(d.abund.min10, d.abund.min40, d.abund.min6)
