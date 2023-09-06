# set up glm models
# Author: jon went, jwent@ethz.ch
# 6.09.2023

# ---- 
library(tidyverse)
# library(lme4)
# install.packages("mgcv")
# library(mgcv)
# install.packages("MCMCglmm")
# library(MCMCglmm)
# install.packages("AUC")
library(AUC)

# --- load data ----
load("data/BBS_stable_t1_t2.rda")
load("data/hfp_t1_t2.rda")
load("data/Land_use/land_use_area_t1_t2.rda")
load("data/Climate/climate_df.rda")

# ---- build data set ----
climate.df <- climate_df %>%
  select(-c(contains(c("var","median")))) %>%
  select(-c("cmi.diff.mean", "cmi.annual.mean", "pr.sum.mean")); rm(climate_df)

hfp.df <- hfp.full %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route)); rm(land_use_area)

BBS.full <- BBS.stable.full %>%
  select(-c("abund.median", "abund.var")) %>%
  left_join(climate.df, by = c("year", "segment")) %>%
  left_join(hfp.df, by = c("year", "segment")) %>%
  left_join(land.use.df, by = c("year", "segment"))

BBS.full$complete_data <- complete.cases(BBS.full)

BBS.df <- BBS.full %>% filter(complete_data == T) %>%
  select(-complete_data)

BBS.t1 <- BBS.df %>% filter(year == 2001)
BBS.t2 <- BBS.df %>% filter(year == 2019)

rm(BBS.full, BBS.stable.full, climate.df, hfp.df, land.use.df)

#---- glm function -----

# all_vars <- colnames(BBS.t1[5:17])
# unique(BBS.t1$animal_jetz)

# form.tmp <- as.formula(paste("abund.mean ~", all_vars, "+ I(", all_vars, "^2)"))

# form.glm <- as.formula(paste(abund.mean ~ pr.diff.mean + I(pr.diff.mean^2) +
#                          tmax.mean + I(tmax.mean^2) +
#                          tmin.mean + I(tmin.mean^2) +
#                          swb.mean + I(swb.mean^2) +
#                          hfp.mean + I(hfp.mean^2) +
#                          urban.low.area.m2 + I(urban.low.area.m2^2) +
#                          urban.high.area.m2 + I(urban.high.area.m2^2) +
#                          forest.area.m2 + I(forest.area.m2^2) +
#                          grass.area.m2 + I(grass.area.m2^2) +
#                          pasture.area.m2 + I(pasture.area.m2^2) +
#                          crop.area.m2 + I(crop.area.m2^2) +
#                          wet.area.m2 + I(wet.area.m2^2) +
#                          barren.area.m2 + I(barren.area.m2^2)))

fit_glms <- function(df, birds.func, vars){
  
  model_results <- list()
  
  form.glm <- as.formula(paste0("abund.mean ~", vars, "+ I(", vars, "^2)"))
  
  for(bird in birds.func){
    
    bird.tmp <- df %>%
      filter(animal_jetz == bird)
    
    model <- glm(form.glm, data = bird.tmp, family = poisson)
    
    model_results[[bird]] <- model
  }
  
  return(model_results)
}

# ---- fit glms ----

glm_all_full <- fit_glms(BBS.t1, birds.func = sort(unique(BBS.t1$animal_jetz)), vars = colnames(BBS.t1[5:17]))

#cross validation
# for(i in seq_along(n_reps)){
# train_prop = train_proportion
# train_indices <- sample_frac(df, vars, train_prop)
# train_data <- df %>% filter(row_number() %in% row_number(train_indices))
# eval_data <- df %>% filter(!row_number() %in% row_number(train_indices))
# }

# --- cross validation function ----

# The problem is, that for some species I have less than 10 observations....

cv.model <- function(model, K, data = model$data){
  
  ks <- dismo::kfold(model$data, k = K, by = model$data[,as.character(formula(model)[2])])
  
  if(length(ks) >= K){
    
    cvpreds <- data.frame(row = row.names(data), cvpred = numeric(length = nrow(data)))
    
    for(i in 1:K){
      train <- data[ks!=i,]
      test <- data[ks==i,]
      modtmp <- update(model, data = train)
      cvpreds[which(ks==i),2] <- predict(modtmp, newdata = test, type = 'response')
    }
  
  }else if(length(ks) < K){
    
    cvpreds <- NA
  }
  return(cvpreds)
}

# ---- cross validate and predict ----
set.seed(123)

cv.models <- list()

for(bird in sort(unique(BBS.t1$animal_jetz))){
  
  model.tmp <- glm_all_full[[bird]]
  
  cv.model.tmp <- cv.model(model.tmp, 5)
  
  cv.models[[bird]] <- cv.model.tmp
  
}

for(bird in sort(unique(BBS.t1$animal_jetz))){
  
  model.tmp <- glm_all_full[[bird]]
  
  pred.tmp <- cv.models[[bird]]
  
  if(!is.na(all(pred.tmp))){
    plot(model.tmp$fitted.values, pred.tmp$cvpred, 
         xlab = "fitted values from glm model", 
         ylab = "predicted values from cross-validation",
         main="Cross validation GAM")
    abline(0, 1, lwd = 3, col = "red")
  }
}

rm(form.glm, form.tmp, all_vars, bird, cv.model.tmp, model.tmp, pred.tmp)

# ---- calculate ROC and AUC ----



##ROC-plots and AUC
str(AUC::roc(glm.eagle.step.test$predicted, as.factor(glm.eagle.step.test$observed)))

#storing raw data for plotting ROC-curves and calculating AUC
roc.glm.eagle.step <- roc(glm.eagle.step.test$predicted, as.factor(glm.eagle.step.test$observed))
roc.glm.eagle.xval <- roc(xval.glm.eagle.step.test$predicted, as.factor(xval.glm.eagle.step.test$observed))


#plotting the ROC curves
plot(roc.glm.eagle.step, col = "grey20", lwd = 5, lty = 1, main='Eagle')
plot(roc.glm.eagle.xval, col = "grey70", lwd = 3, lty = 2, add = TRUE)


#comparing AUC values
auc(roc.glm.eagle.step) #0.9872774

auc(roc.glm.eagle.xval) #0.9864871


