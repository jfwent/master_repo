# RandomForest Model for relative variable importance
# based on DOI: 10.1111/geb.12377 (Howard et al., 2015)
# Author: Jon Went, jwent@ethz.ch
# Date: 14.09.2023

# ---- libraries ---- 
library(tidyverse)
library(randomForest)
library(progress)
library(caret)

# library(foreach)
# library(doParallel)

# library(party)
# library(e1071)
# library(varImp)

# ---- training and testing data ----

bbs.t1 <- BBS_bioclim %>% filter(year == 2001)

birds <- unique(bbs.t1$animal_jetz)

train_tib_occ <- tibble()
test_tib_occ <- tibble()

train_tib_abs <- tibble()
test_tib_abs <- tibble()

set.seed(123)

for(bird in birds){
  
  bbs.tmp <- bbs.t1 %>% filter(animal_jetz %in% bird)
  
  bbs.occ <- bbs.tmp %>% filter(abund.geom.mean > 0)
  
  inTraining.occ <- createDataPartition(bbs.occ$animal_jetz, p = 0.85, list = F)
  training.occ <- bbs.occ[inTraining.occ, ]
  testing.occ <- bbs.occ[-inTraining.occ, ]
  
  train_tib_occ <- bind_rows(train_tib_occ, training.occ)
  test_tib_occ <- bind_rows(test_tib_occ, testing.occ)
  
  bbs.abs <- bbs.tmp %>% filter(abund.geom.mean == 0)
  
  inTraining.abs <- createDataPartition(bbs.abs$animal_jetz, p = 0.85, list = F)
  training.abs <- bbs.abs[inTraining.abs, ]
  testing.abs <- bbs.abs[-inTraining.abs, ]
  
  train_tib_abs <- bind_rows(train_tib_abs, training.abs)
  test_tib_abs <- bind_rows(test_tib_abs, testing.abs)
  
}

train_tib <- bind_rows(train_tib_occ, train_tib_abs)

test_tib <- bind_rows(test_tib_occ, test_tib_abs)

test_tib <- test_tib %>% mutate(PA = ifelse(abund.geom.mean > 0, 1, 0)) %>%
  relocate(PA, .after = abund.geom.mean) %>%
  group_by(animal_jetz) %>%
  mutate(n_obs = n(),
         n_obs_occ = sum(abund.geom.mean > 0)) %>%
  arrange(animal_jetz)

train_tib <- train_tib %>% mutate(PA = ifelse(abund.geom.mean > 0, 1, 0)) %>%
  relocate(PA, .after = abund.geom.mean)  %>%
  group_by(animal_jetz) %>%
  mutate(n_obs = n(),
         n_obs_occ = sum(abund.geom.mean > 0)) %>%
  arrange(animal_jetz)


rm(training.abs, training.occ, testing.abs, testing.occ, bird,
   inTraining.abs, inTraining.occ, bbs.tmp, test_tib_abs, test_tib_occ,
   train_tib_abs, train_tib_occ, bbs.abs, bbs.occ)

# ---- abundance based RF ----

# # ---- parallel ----
# 
# num_cores <- detectCores() - 4  # Adjust as needed
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
# 
# # --- find best mtry for each bird
# 
# tuneGrid <- expand.grid(
#   mtry = c(1: 10)#,
#   # ntree = c(300:1500),
#   # nodesize = c(3:25)
# )
# 
# trControl <- trainControl(method = "cv",
#                           number = 10,
#                           search = "grid",
#                           allowParallel = T)
# 
# set.seed(123)
# 
# pb <- progress_bar$new(
#   format = "[:bar] :percent | ETA: :eta",
#   total = length(birds),
#   clear = FALSE
# )
# 
# rf.models <- foreach(bird = birds, .combine = "list") %dopar% {
#   
#   train.tmp <- train_tib |>
#     dplyr::filter(animal_jetz %in% bird) |>
#     dplyr::select(-c(PA, n_obs, n_obs_occ, year, segment))
#   
#   rf.tmp <- caret::train(
#     abund.geom.mean ~ .,
#     data = train.tmp[2:10],
#     method = "rf",
#     trControl = trControl,
#     tuneGrid = tuneGrid,
#     metric = "RMSE"
#   )
#   
#   pb$tick()
#   
#   return(list(bird = bird, model = rf.tmp))
# }
# 
# stopCluster(cl)
# 
# unregister_dopar <- function() {
#   env <- foreach:::.foreachGlobals
#   rm(list=ls(name=env), pos=env)
# }
# 
# 
# unregister_dopar()

# ---- not parallel ----

# --- find best mtry 

tuneGrid <- expand.grid(
  mtry = c(1: 8)
)

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

# rf.mtry <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

for(bird in birds){

  pb$tick()

  train.tmp <- train_tib %>% filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment))

  rf.tmp <- train(abund.geom.mean ~.,
                  data = train.tmp[2:10],
                  method = "rf",
                  trControl = trControl,
                  tuneGrid = tuneGrid,
                  metric = "RMSE",
                  ntree = 500,
                  nodesize = 14)

  rf.mtry[[bird]] <- rf.tmp

}

best_mtry <- lapply(rf.mtry, function(model) {
  model$bestTune$mtry
})

tuning_params <- tibble(bird = names(best_mtry),
                        mtry = unlist(best_mtry))

# ---- find best maxnodes


# tuneGrid <- expand.grid(
#   mtry = c(1: 10)
# )

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

maxnodes_mods <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

for(bird in birds){
  
  pb$tick()
  
  train.tmp <- train_tib %>% filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment))
  
  mtry.best <- tuning_params$mtry[which(tuning_params$bird == bird)]
  
  store_maxnode <- list()
  
  for(maxnodes.it in 10:20){
    
    set.seed(123)
    
    rf_maxnode <- train(abund.geom.mean ~.,
                    data = train.tmp[2:10],
                    method = "rf",
                    trControl = trControl,
                    tuneGrid = expand.grid(mtry = mtry.best),
                    metric = "RMSE",
                    ntree = 500,
                    nodesize = 14,
                    maxnodes = maxnodes.it)
    
    current_iteration <- toString(maxnodes.it)
    store_maxnode[[current_iteration]] <- rf_maxnode
    
  }
  
  maxnodes_mods[[bird]] <- store_maxnode
}

best_models <- list()

maxnodes_tib <- tibble()

# Iterate through the list of entries (each entry represents a bird species)
for (entry in maxnodes_mods) {
  best_maxnodes <- NULL
  best_r_squared <- Inf
  
  # Iterate through the models within each entry
  for (maxnodes in 10:20) {  # Adjust the range as needed
    model <- entry[[toString(maxnodes)]]
    r_squared <- 1 - model$results$RMSE^2
    
    # Check if this model has a lower R-squared value
    if (r_squared < best_r_squared) {
      best_maxnodes <- maxnodes
      best_r_squared <- r_squared
    }
  }
  
  new_entry <- tibble(maxnodes = best_maxnodes)
  maxnodes_tib <- bind_rows(maxnodes_tib, new_entry)
}

tuning_params <- tibble(bird = names(best_mtry),
                        mtry = unlist(best_mtry),
                        maxnodes = maxnodes_tib$maxnodes)

rm(best_maxnodes, best_r_squared, bird, maxnodes, maxnodes.it, mtry.best, r_squared, current_iteration,
   store_maxnode, rf.tmp, rf_maxnode, new_entry, pb, best_models, best_model, train.tmp, maxnodes_tib, model)

# ---- find best ntree

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

maxtrees_mods <- list()

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

for(bird in birds){
  
  pb$tick()
  
  train.tmp <- train_tib %>% filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment))
  
  mtry.best <- tuning_params$mtry[which(tuning_params$bird == bird)]
  
  maxnodes.best <- tuning_params$maxnodes[which(tuning_params$bird == bird)]
  
  store_maxtrees <- list()
  
  for(maxtrees.it in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)){
    
    set.seed(123)
    
    rf_maxtrees <- train(abund.geom.mean ~.,
                        data = train.tmp[2:10],
                        method = "rf",
                        trControl = trControl,
                        tuneGrid = expand.grid(mtry = mtry.best),
                        metric = "RMSE",
                        ntree = maxtrees.it,
                        nodesize = 14,
                        maxnodes = maxnodes.best)
    
    key <- toString(maxtrees.it)
    store_maxtrees[[key]] <- rf_maxtrees
    
  }
  
  maxtrees_mods[[bird]] <- store_maxtrees
}

maxtrees_tib <- tibble()

# Iterate through the list of entries (each entry represents a bird species)
for (entry in maxtrees_mods) {
  best_maxtrees <- NULL
  best_r_squared <- Inf
  
  # Iterate through the models within each entry
  for (maxtrees in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {  # Adjust the range as needed
    model <- entry[[toString(maxtrees)]]
    r_squared <- 1 - model$results$RMSE^2
    
    # Check if this model has a lower R-squared value
    if (r_squared < best_r_squared) {
      best_maxtrees <- maxtrees
      best_r_squared <- r_squared
    }
  }
  
  new_entry <- tibble(maxtrees = best_maxtrees)
  maxtrees_tib <- bind_rows(maxtrees_tib, new_entry)
}

tuning_params <- tibble(bird = names(best_mtry),
                        mtry = unlist(best_mtry),
                        maxnodes = maxnodes_tib$maxnodes,
                        maxtrees = maxtrees_tib$maxtrees)

save(tuning_params, file = "data/abund_rf_tuning_params.rda")

# --- final model 

load("data/abund_rf_tuning_params.rda")

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf_final_mods <- list()

for(bird in birds){
  
  pb$tick()

  train.tmp <- train_tib %>% filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment))
  
  mtry.best <- tuning_params$mtry[which(tuning_params$bird == bird)]
  maxnodes.best <- tuning_params$maxnodes[which(tuning_params$bird == bird)]
  maxtrees.best <- tuning_params$maxtrees[which(tuning_params$bird == bird)]
  
  rf.final <- train(abund.geom.mean ~.,
                  data = train.tmp[2:10],
                  method = "rf",
                  trControl = trControl,
                  tuneGrid = expand.grid(mtry = mtry.best),
                  metric = "RMSE",
                  ntree = maxtrees.best,
                  nodesize = 14,
                  maxnodes = maxnodes.best)
  
  rf_final_mods[[bird]] <- rf.final
  
}

rsquared <- lapply(rf_final_mods, function(model) {
  model$results$Rsquared
})

rsquared <- lapply(rf_final_mods, function(model) {
  model$results$Rsquared
})

rsquared <- unlist(rsquared)

mean(rsquared)
median(rsquared)
sd(rsquared)

# --- test models ---

predictions <- list()

birds <- names(rf_final_mods)

for(bird in birds){
  
  bird.ind <- which(birds == bird)
  
  model.tmp <- rf_final_mods[[bird.ind]]
  
  test.tmp <- test_tib %>% filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment))
  
  prediction <- predict(model.tmp, test.tmp[3:10])
  
  predictions[[bird]] <- prediction
}

rmse_results <- list()
rsquared_results <- list()

for (bird in birds) {
  # Extract the predictions and test data for the current bird
  prediction <- predictions[[bird]]
  test_data <- test_tib %>%
    filter(animal_jetz %in% bird) %>%
    select(-c(PA, n_obs, n_obs_occ, year, segment)) %>%
    pull(abund.geom.mean)  # Assuming the true values are in the 'abund.geom.mean' column
  
  # Calculate RMSE
  rmse <- sqrt(mean((test_data - prediction)^2))
  rmse_results[[bird]] <- rmse
  
  # Calculate R-squared
  rsquared <- 1 - sum((test_data - prediction)^2) / sum((test_data - mean(test_data))^2)
  rsquared_results[[bird]] <- rsquared
}


res_tib <- tibble(birds = names(rmse_results),
                  rmse = unlist(rmse_results),
                  r2 = unlist(rsquared_results))

# ---- PA based RF ----

# ==== OLD ----
# ---- load data ----

load("data/Climate/climate_df.rda")
# load("data/hfp_t1_t2.rda")
load("data/Land_use/land_use_area_t1_t2.rda")

# load("data/BBS.full.stable.min6.rda")
# load("data/BBS.full.stable.min10.rda")
load("data/BBS.full.stable.min40.rda")

# ---- prepare the bioclim data ---- 

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median")),
         contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, pr.diff.mean)); rm(climate_df)

# hfp.df <- hfp.full %>%
#   # filter(year == 2001) %>%
#   select(-c(contains("var"), contains("median")), contains("mean")) %>%
#   mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         all.grass.area.m2 = grass.area.m2 + pasture.area.m2) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2, grass.area.m2, pasture.area.m2, wet.area.m2)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"))

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")) %>%
  na.omit()

rm(land_use_area)
rm(climate.df, land.use.df)

# ---- build the abundance data sets with climate and land use ----

BBS_bioclim <- BBS.stable.full.min40 %>%
  # filter(year == 2001) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  left_join(bioclim.df, by = c("year", "segment")) %>%
  # select(-c(abund.median, abund.var, abund.mean)) %>%
  arrange(animal_jetz) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  na.omit() %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

BBS_bioclim_t1 <- BBS.stable.full.min40 %>%
  filter(year == 2001) %>%
  left_join(bioclim.df, by = c("year", "segment")) %>%
  # select(-c(abund.median, abund.var, abund.mean)) %>%
  # select(-c(abund.median, abund.var, abund.mean)) %>%
  arrange(animal_jetz) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  na.omit()

BBS_bioclim_t2 <- BBS.stable.full.min40 %>%
  filter(year == 2019) %>%
  left_join(bioclim.df, by = c("year", "segment")) %>%
  # select(-c(abund.median, abund.var, abund.mean)) %>%
  arrange(animal_jetz) %>%
  mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  na.omit(); rm(bioclim.df, BBS.stable.full.min40)

# ---- RF model fitting T1 ----

set.seed(123)

birds <- unique(BBS_bioclim$animal_jetz)
years <- unique(BBS_bioclim$year)

climate_vars <- colnames(BBS_bioclim[5:8])
lc_vars <- colnames(BBS_bioclim[9:12])

rf_mods_t1 <- list()
rf_mods_t2 <- list()

# rel_var_importance_t1 <- list()
# rel_var_importance_t2 <- list()
# 
# climate_importance_list_t1 <- list()
# land_use_importance_list_t1 <- list()
# climate_importance_list_t2 <- list()
# land_use_importance_list_t2 <- list()
# 
# importance_ratio_list_t1 <- list()
# importance_ratio_list_t2 <- list()

tibble_data_combined <- list()

for(year.ind in years){
  
  for(bird in birds){
    
    print(paste0("working on year ", which(years == year.ind), ", bird ", which(birds == bird), "/", length(birds), " ..."))
    
    # Filter data for the current year
    df.tmp <- BBS_bioclim %>% filter(year == year.ind,
                                     animal_jetz %in% bird)
    
    # Create unique model names for each year and bird
    model_name <- paste0("mod.t", year.ind, ".", bird)
    
    # Fit the random forest model for the current year and bird
    # assign(model_name, randomForest(abund.geom.mean ~ pr.diff.mean + tmax.mean + tmin.mean + swb.mean +
    #                                   urban.low.area.m2.log + urban.high.area.m2.log + forest.area.m2.log +
    #                                   grass.area.m2.log + pasture.area.m2.log + crop.area.m2.log + wet.area.m2.log +
    #                                   barren.area.m2.log + hfp.mean,
    #                                 data = df.tmp,
    #                                 ntree = 1000,
    #                                 nfold = 10,
    #                                 importance = TRUE))
    
    assign(model_name, cforest(abund.geom.mean ~ .,
                          data = df.tmp[4:12],
                          controls = cforest_unbiased(mtry = sqrt(ncol(df.tmp) - 1),
                                                      ntree = 1000)))
    
    # Store the model in the corresponding list
    if (year.ind == 2001) {
      rf_mods_t1[[bird]] <- get(model_name)
    } else if (year.ind == 2019) {
      rf_mods_t2[[bird]] <- get(model_name)
    }
    
    # Calculate variable importance
    # var_importance <- varImpPlot(get(model_name), type = 1, conditional = TRUE)
    var_importance <- varimp(get(model_name))
    
    rm(list = model_name)
    
    var_imp_adj <- var_importance/sum(var_importance)
    
    # Store the relative variable importance for the current year and bird
    # if (year.ind == 2001) {
    #   rel_var_importance_t1[[bird]] <- var_imp_adj
    # } else if (year.ind == 2019) {
    #   rel_var_importance_t2[[bird]] <- var_imp_adj
    # }
    
    # Separate variable importance scores into climate and land use categories
    climate_importance <- sum(var_imp_adj[1:4])
    land_use_importance <- sum(var_imp_adj[5:8])
    
    # # Store the importance scores for climate and land use variables
    # if (year.ind == 2001) {
    #   climate_importance_list_t1[[bird]] <- climate_importance
    #   land_use_importance_list_t1[[bird]] <- land_use_importance
    # } else if (year.ind == 2019) {
    #   climate_importance_list_t2[[bird]] <- climate_importance
    #   land_use_importance_list_t2[[bird]] <- land_use_importance
    # }
    
    # Calculate the ratio of climate to land use importance
    importance_ratio <- climate_importance / land_use_importance
    
    # # Store the importance ratio for the current year and bird
    # if (year.ind == 2001) {
    #   importance_ratio_list_t1[[bird]] <- importance_ratio
    # } else if (year.ind == 2019) {
    #   importance_ratio_list_t2[[bird]] <- importance_ratio
    # }
    
    # browse()
    
    tibble_entry <- list(
      Year = year.ind,
      Bird = bird,
      Land_Use_Importance = land_use_importance,
      Climate_Importance = climate_importance,
      Importance_Ratio = importance_ratio,
      var.imp = var_importance,
      var.imp.adj = var_imp_adj,
      vars = names(var_imp_adj)
    )
    
    tibble_data_combined[[length(tibble_data_combined) + 1]] <- tibble_entry
  }
}

final_tibble <- bind_rows(tibble_data_combined)

# # ratio of the relative importance of climate:land use variables and the change from 2001 - 2019
# 
# importance_ratio_t1 <- reshape2::melt(importance_ratio_list_t1) %>%
#   mutate(year = 2001) %>%
#   rename(imp.ratio = value, bird = L1)
# 
# importance_ratio_t2 <- reshape2::melt(importance_ratio_list_t2)  %>%
#   mutate(year = 2019) %>%
#   rename(imp.ratio = value, bird = L1)
# 
# imp_ratio <- rbind(importance_ratio_t1, importance_ratio_t2)
# 
# save(imp_ratio, file = "data/importance_ratio.rda")

# imp_ratio <- importance_ratio_t1 %>% left_join(importance_ratio_t2, by = "bird") %>%
#   relocate(bird) %>% rowwise() %>% mutate(d.imp.ratio = imp.ratio.t2 - imp.ratio.t1)


# ---- data transformation ----

ratios <- final_tibble %>%
  select(Year, Bird, var.imp, vars) %>%
  mutate(var.imp.trans = ifelse(var.imp < 0, 0, var.imp),
         var.desig = ifelse(grepl("log", vars), "land", "climate")) %>%
  group_by(Year, Bird) %>%
  mutate(cum_imp = sum(var.imp.trans),
         rel.var.imp = var.imp.trans/cum_imp) %>%
  group_by(Year, Bird, var.desig) %>%
  mutate(sum_imp_vars = sum(rel.var.imp)) %>%
  group_by(Year, Bird) %>%
  reframe(ratio = sum_imp_vars[var.desig == "climate"] / sum_imp_vars[var.desig == "land"]) %>%
  distinct() %>%
  filter(Bird != "Amphispiza_bilineata")

# ---- plot results in a paired barplot ----

# load("data/importance_ratio.rda")

library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)

# showtext_auto()
# showtext_opts(dpi = 300)

# Colors we will use later
color_palette <- c("royalblue4", "royalblue")  # thematic::okabe_ito(2)
names(color_palette) <- c(2001, 2019)

# colors()

segment_helper <- ratios %>%
  pivot_wider(names_from = Year, values_from = ratio, names_prefix = 'year_') |>
  mutate(
    change = year_2019 - year_2001,
    Bird = fct_reorder(Bird, year_2001 * if_else(change < 0, -1, 1))
  )

paired_barplot <- ratios %>%
  # filter(bird %in% sample(unique(bird), size = 25)) %>%
  mutate(year = factor(Year)) %>%
  mutate(bird = fct_reorder(Bird, ratio, max)) %>%
  ggplot2::ggplot(aes(x = ratio, y = Bird, col = year)) +
  geom_segment(
    data = segment_helper,
    aes(x = year_2001, xend = year_2019, y = Bird, yend = Bird),
    col = 'grey60',
    linewidth = 0.5
  ) +
  geom_point(size = 1) +
  scale_color_manual(values = color_palette) +
  labs(
    x = 'Ratio climate:land use',
    y = element_blank()
  ) +
  theme(axis.text=element_text(size=5)) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40")

paired_barplot

ggsave(filename = "figures/paired_var_imp_ratio_RF.png", plot = paired_barplot, width = 8, height = 6, dpi = 300)
  
  
# --- RF model approach similar to Howard et al., 2023 ----
# The authors build SDMs


# ---- library ----
library(progress)
library(tidyverse)
library(randomForest)

# ---- data ----

load("data/BBS.full.stable.min40.rda")
load("data/BBS.full.stable.min10.rda")

birds <- sort(unique(BBS.stable.full.min40$animal_jetz))

BBS.dat <- BBS.stable.full.min40 %>% mutate(abund.geom.mean = as.integer(abund.geom.mean)) %>%
  filter(year %in% 2001)

# ---- for loop ----

# Initialize vectors to store results
best_mtry <- numeric(length(birds))
best_nt <- numeric(length(birds))
best_rmse <- numeric(length(birds))

# Define the maximum number of trees to add in each iteration
max_trees_to_add <- 500

pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(birds),
  clear = FALSE
)

for (bird_index in 1:length(birds)) {
  
  bird <- birds[bird_index]
  
  # Filter data for the current bird
  df.tmp <- BBS.dat %>% filter(animal_jetz == bird)
  
  # Initialize values for best mtry and best nt
  best_mtry[bird_index] <- sqrt(ncol(df.tmp) - 1)
  best_nt[bird_index] <- 1000
  prev_rmse <- 1000000000000000000  # Set to a high value to ensure improvement in RMSE
  
  # for (mtry in 1:(sqrt(ncol(df.tmp) - 1))) {
  for(mtry in rev(seq(1, sqrt(ncol(df.tmp) - 1)))){
    for (trees_to_add in seq(0, max_trees_to_add, by = 500)) {
      
      pb$tick()
      
      # # Fit the RF model with current mtry and nt
      # rf_model <- randomForest(abund.geom.mean ~ .,
      #                          data = df.tmp,
      #                          mtry = mtry,
      #                          ntree = best_nt[bird_index] + trees_to_add,
      #                          nodesize = 30)
      
      
      n <- nrow(df.tmp)
      rmse_values <- numeric(n)
      
      for (i in seq_along(length(df.tmp))) {
        i <- as.integer(i)
        # Split data into training and test sets, leaving out the i-th block
        test_indices <- (i - 1) * (n / 10) + 1:(n / 10)
        train_data <- df.tmp[-test_indices, ]
        test_data <- df.tmp[test_indices, ]
        
        # Fit the RF model on the training data
        rf_model <- randomForest(formula = as.formula(paste("abund.geom.mean", "~ .")),
                                 data = train_data[4:17],
                                 mtry = mtry,
                                 ntree = best_nt[bird_index] + trees_to_add,
                                 nodesize = 10)
        
        # Predict on the test data
        predictions <- predict(rf_model, newdata = test_data[4:17])
        
        # Calculate RMSE for this fold
        rmse <- sqrt(mean((test_data$abund.geom.mean - predictions)^2))
        rmse_values[i] <- rmse
      }
      
      # Check if RMSE improvement is significant
      if ((prev_rmse - mean(rmse_values)) < 0.1) {  # You can adjust the threshold as needed
        # If improvement is not significant, stop and store best mtry and nt
        break
      } else {
        # If improvement is significant, update best mtry, best nt, and prev_rmse
        best_mtry[bird_index] <- mtry
        best_nt[bird_index] <- best_nt[bird_index] + trees_to_add
        prev_rmse <- mean(rmse_values)
      }
    }
  }
  
  # Store the best RMSE for this bird
  best_rmse[bird_index] <- prev_rmse
}

res_tib <- tibble(bird = birds,
                  best_mtry = best_mtry,
                  best_nt = best_nt,
                  best_rmse = best_rmse)


# ---- new try ----

library(caret)
library(randomForest)

bbs.dat <- BBS_bioclim %>% filter(year == 2001)
birds <- unique(bbs.dat$animal_jetz)

# Create 10 blocks for each species

train.index.list <- list()

for (bird in birds) {
  
  bbs.tmp <- filter(bbs.dat, animal_jetz == species_group)
  
  train.index <- groupKFold(bbs.tmp, k = 0.2*length(bbs.tmp))
  
  train.index.list[[bird]] <- train.index
}


# Define control parameters for Random Forest
rf_control <- trainControl(
  method = "none",  # Turn off resampling for this procedure
  verboseIter = TRUE
)

# Define a grid of mtry values to try
mtry_grid <- expand.grid(mtry = 1:3)

# Initialize the number of trees
nt <- 1000  # Initial number of trees

num_blocks <- 10  # Number of blocks in your data
best_mtry <- NULL  # Initialize the best mtry
best_nt <- NULL    # Initialize the best number of trees
best_adj_r2 <- -Inf  # Initialize the best adjusted R-squared

for(bird in birds){
  
  bbs.tmp <- bbs.dat %>% filter(animal_jetz == "bird")
  
  for (block_to_omit in 1:num_blocks) {
    # Create a subset of your data, omitting the block_to_omit
    subset_data <- bbs.tmp[bbs.tmp$block_column != block_to_omit, ]
    
    # Initialize a flag to check for improvement in adjusted R-squared
    improvement_threshold <- 0.01  # 1% improvement threshold
    improvement <- TRUE
    
    while (improvement) {
      # Define the formula
      # formula <- abund.geom.mean ~ predictor1 + predictor2 + ...
      
      # Train the Random Forest model for the current mtry and nt values
      rf_model <- train(
        abund.geom.mean ~.,
        data = subset_data,
        method = "rf",         # Specify Random Forest as the method
        trControl = rf_control,  # Use the control parameters defined earlier
        tuneGrid = data.frame(mtry = best_mtry, ntree = nt)
      )
      
      # Calculate adjusted R-squared
      adj_r2 <- summary(rf_model$finalModel)$adj.r.squared
      
      # Check for improvement in adjusted R-squared
      improvement <- adj_r2 - best_adj_r2 > improvement_threshold
      
      if (improvement) {
        # Update the best values
        best_mtry <- rf_model$bestTune$mtry
        best_nt <- nt
        best_adj_r2 <- adj_r2
        
        # Add 500 trees to the model
        nt <- nt + 500
      }
    }
    
    # Print or store any relevant information about the best model and iteration
    print(paste("Iteration", block_to_omit, "- mtry:", best_mtry, "- nt:", best_nt, "- Adj.R-squared:", best_adj_r2))
  }
}

final_rf_models <- list()

for (block_to_omit in 1:num_blocks) {
  # Create a subset of your data, omitting the block_to_omit
  subset_data <- your_data[your_data$block_column != block_to_omit, ]
  
  # Train the Random Forest model with the best mtry and nt values
  final_rf_model <- train(
    formula,
    data = subset_data,
    method = "rf",  # Specify Random Forest as the method
    trControl = rf_control,
    tuneGrid = data.frame(mtry = best_mtry, ntree = best_nt)
  )
  
  # Store the final RF model
  final_rf_models[[block_to_omit]] <- final_rf_model
}
