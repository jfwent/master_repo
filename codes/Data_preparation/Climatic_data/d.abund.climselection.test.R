# Select the climate variables for the delta abundance models
# Author: Jon Went
# Date: 21.09.2023

# ---- libraries -----

library(tidyverse)
library(caret)
library(progress)
library(patchwork)

# ---- load data ----

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
  left_join(clim.df, by = c("segment")) %>%
  group_by(animal_jetz) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries >= 10) %>%
  select(-n_entries)

#%>%
# mutate(delta.abund = as.integer(delta.abund))

d.abund.lc <- d.abund.min40 %>%
  select(segment, animal_jetz, delta.abund) %>%
  left_join(lc.df, by = c("segment")) %>%
  group_by(animal_jetz) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries >= 10) %>%
  select(-n_entries)
#%>%
# mutate(delta.abund = as.integer(delta.abund))

rm(BBS.stable.full.min10, BBS.stable.full.min40, BBS.stable.full.min6, clim.df, d.abund.min10,
   d.abund.min40, d.abund.min6, lc.df)

# ---- LM function ----

fit_lm <- function(df, var) {
  
  model <- lm(formula, data = df)
  return(model)
}

# ---- LM models climate variables ----
# num_folds <- 10

# clim_rmse <- tibble(bird = character(),
#                     variable = character(),
#                     mean_rmse = numeric(),
#                     n.folds = numeric())
# significant_terms <- tibble(bird = character(),
#                             variable = character(),
#                             p.values = numeric()
# )

birds <- sort(unique(d.abund.climate$animal_jetz))
# birds <- sort(unique(abund.min40.lc$animal_jetz))
climate_vars <- colnames(d.abund.climate[4:17])

trControl <- trainControl(method = "cv",
                          number = 10)

r2.clim <- tibble(bird = character(),
                  variable = character(),
                  adj.r2 = numeric())

clim_mods <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(climate_vars),
  clear = FALSE
)

set.seed(123)
for(variable.ind in climate_vars){
  
  # print(paste0("working on ", which(climate_vars == variable.ind), "/", length(climate_vars), " ..."))
  pb$tick()
  
  bird_mods <- list()
  
  for(bird.ind in birds){
    bird.tmp <- d.abund.climate %>%
      filter(animal_jetz == bird.ind)
    
    full.mod <- train(as.formula(paste("delta.abund ~", variable.ind, "+ I(", variable.ind, "^2)")),
                      data = bird.tmp[3:17],
                      method = "lm",
                      trControl = trControl)
    
    bird_mods[[bird.ind]] <- full.mod
    
    adj.r2.now <- summary(full.mod$finalModel)$r.squared
    
    # browser()
    
    new_row1 <- tibble(bird = bird.ind, variable = variable.ind,
                       adj.r2 = adj.r2.now)
    
    r2.clim <- bind_rows(r2.clim, new_row1)
    
    # p.val <- summary(full.mod)$coefficients[,4]
    
    # new_row <- tibble(bird = bird.ind,
    #                   variable = names(p.val),
    #                   p.values = p.val)
    # 
    # significant_terms <- bind_rows(significant_terms, new_row)
    
    # mean_rmse <- mean(rmse_values)
    # new_row2 <- tibble(bird = bird.ind, mean_rmse = mean_rmse,
    #                    variable = variable.ind, n.folds = length(folds))
    # 
    # clim_rmse <- bind_rows(clim_rmse, new_row2)
  }
  
  clim_mods[[variable.ind]] <- bird_mods
}

rm(bird_model, variable.ind, climate_vars, birds,
   bird.ind, mean_rmse, n_fold, num_folds, predicted_values, rmse, rmse_values,
   new_row1, new_row2, folds,
   bird.test, bird.tmp, bird.train, full.mod, adj.r2.now,
   p.val, p.val.ind, p.val.quad, new_row, trControl, bird_mods, pb)

# sig.terms <- significant_terms %>% filter(p.values <= 0.05,
#                                           variable != "(Intercept)",
#                                           variable != grepl("^", variable))
# length(unique(sig.terms$bird))
# length(unique(sig.terms$variable))

# ---- LM models land use variables ----

birds <- sort(unique(d.abund.lc$animal_jetz))
# land_use_vars <- colnames(d.abund.lc[4:13])
land_use_vars <- colnames(d.abund.lc[4:13])

num_folds <- 10

lc_rmse <- tibble(bird = character(),
                  variable = character(),
                  mean_rmse = numeric(),
                  n.folds = numeric())

r2.lc<- tibble(bird = character(),
               variable = character(),
               adj.r2 = numeric())

set.seed(123)

for(variable.ind in land_use_vars){
  
  print(paste0("working on ", which(land_use_vars == variable.ind), "/", length(land_use_vars), " ..."))
  
  for(bird.ind in birds){
    
    bird.tmp <- d.abund.lc %>%
      filter(animal_jetz == bird.ind) %>%
      tibble::rowid_to_column(., "ID")
    
    full.mod <- fit_lm(bird.tmp, var = land_use_vars)
    
    adj.r2.now <- summary(full.mod)$adj.r.squared
    
    folds <- caret::groupKFold(bird.tmp$segment, k = 10)
    
    rmse_values <- numeric(num_folds)
    
    for(n_fold in seq_along(folds)){
      
      bird.train <- bird.tmp %>%
        filter(ID %in% folds[[n_fold]])
      
      bird.test <- bird.tmp %>%
        filter(!(ID %in% folds[[n_fold]]))
      
      bird_model <- fit_lm(bird.train, variable.ind)
      
      predicted_values <- predict(bird_model, newdata = bird.test)
      
      rmse <- sqrt(mean((bird.train$delta.abund - predicted_values)^2))
      
      rmse_values[n_fold] <- rmse
    }
    
    mean_rmse <- mean(rmse_values)
    
    new_row1 <- tibble(bird = bird.ind, variable = variable.ind,
                       adj.r2 = adj.r2.now)
    
    new_row2 <- tibble(bird = bird.ind, mean_rmse = mean_rmse,
                       variable = variable.ind, n.folds = length(folds))
    
    r2.lc <- bind_rows(r2.lc, new_row1)
    lc_rmse <- bind_rows(lc_rmse, new_row2)
    
  }
}

rm(bird_model, variable.ind, land_use_vars, birds,
   bird.ind, mean_rmse, n_fold, num_folds, predicted_values, rmse, rmse_values,
   new_row1, new_row2, folds,
   bird.test, bird.tmp, bird.train)


# ---- build climate boxplots  ----

bp_rmse_clim <- clim_rmse %>%
  ggplot2::ggplot(aes(y = variable, x = mean_rmse, fill = variable)) +
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

bp_r2_clim <- r2.clim %>%
  ggplot2::ggplot(aes(y = reorder(variable, adj.r2), x = adj.r2,
                      fill = reorder(variable, adj.r2)
                      )) +
  scale_fill_viridis_d(
    aesthetics = c("fill"),
    alpha = 0.6,
    # labels = c("Climate", "Full", "Land cover"),
    # guide = guide_legend(title = "Model"),
    option = "H",
  ) +
  geom_boxplot(width = 0.3,
               outlier.size = 1,
               alpha = 0.5) +
  # geom_point(
  #   aes (color = variable),
  #   shape = "|",
  #   size = 1.8,
  #   alpha = 0.7,
  #   position = position_nudge(y = -0.275)
  # ) +
  ggdist::stat_slab(
    position = position_nudge(y = 0.300),
    height = 0.5
  )+
  stat_summary(
    fun.data = mean_se, # Use mean and standard error
    geom = "point",
    color = "black",
    size = 1.5
  ) +
  theme_bw() +
  theme(
    # legend.position = "none",
    panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 7, color = "black"),
    legend.margin = margin(t=-0.6, unit = "cm"),
    legend.position = "none",
    legend.key.width = unit(5, "mm"),
    # legend.key.size = unit(5, "mm"),
  )+
  ylab("") +
  xlab(expression(paste("adj. R"^2)))

bp_r2_clim

bp_r2_clim_83birds <- ttt %>%
  ggplot2::ggplot(aes(y = reorder(variable, adj.r2), x = adj.r2,
                      fill = reorder(variable, adj.r2)
  )) +
  scale_y_discrete(breaks = c("delta.cmi.diff.mean", "delta.pr.sum.mean", "delta.tmax.mean",
                              "delta.pr.diff.mean", "delta.cmi.annual.mean", "tmax.mean",
                              "delta.swb.mean", 
                              "tmin.mean", "cmi.diff.mean", "pr.diff.mean",
                              "swb.mean", "delta.tmin.mean", "cmi.annual.mean", "pr.sum.mean"),
                   labels = c("+ delta.cmi.diff", "* delta.pr", "delta.tmax",
                              "+ delta.pr.diff", "* delta.cmi", "tmax", "* delta.swb", 
                              "tmin", "— cmi.diff", "— pr.diff", "º swb", "delta.tmin", "º cmi", "º pr")) +
  scale_fill_viridis_d(
    aesthetics = c("fill"),
    alpha = 0.6,
    # guide = guide_legend(title = "Model"),
    option = "H",
  ) +
  geom_boxplot(width = 0.3,
               outlier.size = 1,
               alpha = 0.5) +
  # geom_point(
  #   aes (color = variable),
  #   shape = "|",
  #   size = 1.8,
  #   alpha = 0.7,
  #   position = position_nudge(y = -0.275)
  # ) +
  ggdist::stat_slab(
    position = position_nudge(y = 0.300),
    height = 0.63
  )+
  stat_summary(
    fun.data = mean_se, # Use mean and standard error
    geom = "point",
    color = "black",
    size = 1.5
  ) +
  theme_bw() +
  theme(
    # legend.position = "none",
    panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    # legend.text = element_text(size = 7, color = "black"),
    # legend.margin = margin(t=-0.6, unit = "cm"),
    legend.position = "none",
    # legend.key.width = unit(5, "mm"),
    # legend.key.size = unit(5, "mm"),
  ) +
  ylab("") +
  xlab(expression(paste("adj. R"^2)))

bp_r2_clim_83birds

bp_vergleich <- bp_r2_clim + bp_r2_clim_83birds

bp_vergleich

ggplot2::ggsave(filename = "figures/ClimVar_adj_r2_dAbund.png", plot = bp_r2_clim_83birds, width = 8, height = 6, dpi = 300)

# ---- build land use boxplots ----

bp_lc_rmse_outliers <- lc_rmse %>%
  ggplot2::ggplot(aes(y = variable, x = mean_rmse, fill = variable)) +
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
  )

bp_lc_outliers

ggplot2::ggsave(filename = "figures/LCVar_bp_RMSE_dAbund.png", plot = bp_lc_outliers, width = 8, height = 6, dpi = 300)


bp_lc_rmse_no_outliers <- lc_rmse %>%
  ggplot2::ggplot(aes(y = variable, x = mean_rmse, fill = variable)) +
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
  coord_cartesian(xlim = c(0, 60))

bp_lc_no_outliers

ggplot2::ggsave(filename = "figures/LCVar_bp_RMSE_dAbund_noOutliers.png", plot = bp_lc_no_outliers, width = 8, height = 6, dpi = 300)

bp_lc_r2 <- r2.lc %>%
  ggplot2::ggplot(aes(y = variable, x = adj.r2, fill = variable)) +
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
  )

bp_lc_r2

ggplot2::ggsave(filename = "figures/LCVar_bp_R2_dAbund.png", plot = bp_lc_r2, width = 8, height = 6, dpi = 300)

