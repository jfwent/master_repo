##### Legacy models exploration
##########################################


### The goal of this is to explore different set-ups of legacy models, according to Bird diversity~Land cover
### I want to compare 2 models to each other:
#1. Bird diversity in 2019 explained by land cover in 2019. This model is according to no legacy effects or rapid change.
#2. Bird diversity in 2019 explained by past land cover (for example 2001). This model is according to legacy effects.
#3. If time permits I want to build models of each year's bird diversity data with the corresponding land cover data (2001-2019),
# and predict to 2021 to see how the prediction strength of the models changes.


###======= load data ============
# Load the data
rm(list=ls())
library(dplyr); library(tidyr); library(purrr)

load("data/Lica/BBS_land_years.rda")
years <- unique(BBS_land_years$year) # get all years we have land cover information for
seg_land <- unique(BBS_land_years$partition) # get all segments we have landcover information for: there are 1923 unique segments

load("data/Lica/biodiv_indices.rda")

ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")
seg_eco <- unique(ecoreg$partition) # get all segments in the ecoregion data set: there are 3326 segments

land <- BBS_land_years; rm(BBS_land_years)
biodiv <- biodiv.df; rm(biodiv.df)

land <- land %>%
  rowwise() %>%
  mutate(urban = sum(urban.low, urban.high)) %>%
  select(-urban.low, -urban.high, -route) %>%
  arrange(partition)

land_ID <- unite(land, ID, year, partition, sep = "-")
biodiv_ID <- unite(biodiv, ID, year, segment, sep = "-")
full_df <- inner_join(land_ID, biodiv_ID, by = "ID")
full_df <- separate(full_df, ID, sep = "-", into = c("year", "partition"))
full_df <- inner_join(full_df, ecoreg, by = "partition")
full_df <- full_df %>% select(-FID, -Kilometers)

land_nest <- full_df %>%
  group_nest(year)


#========build GLMM bayesian function===========

 # Build GLMM with brms: Bayesian models to investigate the shannon index: brm(Shannon_index ~ Land_cover_data1 +...+ Land_cover_dataN 
 #                                                       + (1|segment) + (1|Ecoregion))

library(brms)

fit_bayesian_shannonA <- function(data) {
  
  formula <- brmsformula(shannon ~ forest + urban  + grass + pasture + crop + wet + barren +
                           (1|Ecoregion) + (1|partition),
                         family = brmsfamily("gaussian", link = "logit"))
  
  brm_model <- brm(formula, data = data,
                   chains = 4, # number of independent Markov Chains to assess convergence
                   cores = 6, # number of CPU cores to use for parallel processing
                   warmup = 1000, # number of MC iterations that are discarded as burn-in
                   iter = 2000, # number of MC iterations after warm up
                   # control = list(adapt_delta = 0.9999, # target acceptance rate, higher acceptance = higher efficiency in sampling process
                   #                max_treedepth = 12 # determine max tree depth, higher depth = longer trajectory --> better for complex posteriors 
                   #                )
  )
  
  return(brm_model)
  
}

fit_bayesian_shannonB <- function(data) {
  
  formula <- brmsformula(shannon ~ forest + urban  + (1 | grass) + (1|pasture) + (1|crop) + (1|wet )+ (1|barren) +
                           (1 | Ecoregion) + (1 | partition),
                         family = brmsfamily("gaussian", link = "logit"))
  
  brm_model <- brm(formula, data = data,
                   chains = 4, # number of independent Markov Chains to assess convergence
                   cores = 6, # number of CPU cores to use for parallel processing
                   warmup = 1000, # number of MC iterations that are discarded as burn-in
                   iter = 2000, # number of MC iterations after warm up
                   # control = list(adapt_delta = 0.9999, # target acceptance rate, higher acceptance = higher efficiency in sampling process
                   #                max_treedepth = 12 # determine max tree depth, higher depth = longer trajectory --> better for complex posteriors 
                   #                )
                   )
  
  return(brm_model)
  
}

df2001 <- full_df %>% filter(year == 2001)

shannon2001_bayesianA <- fit_bayesian_shannonA(df2001)
shannon2001_bayesianB <- fit_bayesian_shannonB(df2001)

shannon2001_bayesianA <- df2001_bayesian
shannon2001_bayesianB <- df2001_bayesian2

save(shannon2001_bayesianA, file="/Users/jonwent/downloads/shannon2001_bayesianA.rda")
save(shannon2001_bayesianB, file="/Users/jonwent/downloads/shannon2001_bayesianB.rda")



summary(shannon2001_bayesianA)
pp_check(shannon2001_bayesianA)

summary(shannon2001_bayesianB)
pp_check(shannon2001_bayesianB)

# nested_df <- nested_df %>%
#   mutate(model = map(data, fit_bayesian_shannon))


#========transform predictors=========

df2001_scaled <- scale(df2001[3:9])

df2001_arcsine <- asin(sqrt(df2001[3:9]/100))

library(car); library(corrplot); library(ecospat)

ecospat.cor.plot(df2001_arcsine)
ecospat.cor.plot(df2001_scaled)
ecospat.cor.plot(df2001[3:9])

library(MASS)

preds <- df2001[, 3:9]
var <- cor(preds)
var_inv <- MASS::ginv(var)

colnames(var_inv) <- colnames(preds)
rownames(var_inv) <- colnames(preds)

corrplot(var_inv, method='number', is.corr = F)

#========glm models for 2001=========

source("codes/data_exploration/glmm_model_builder.R")
glmm2001_shannon <- function_glmm(df2001, "shannon")
glmm2001_simpson <- function_glmm(df2001, "simpson")
glmm2001_richness <- function_glmm(df2001, "richness")

library(DHARMa)

res_2001_shannon <- simulateResiduals(glmm2001_shannon)
plotResiduals(res_2001_shannon, type = "standardized")
plotQQunif(res_2001_shannon, main = "Q-Q Plot or standardized residuals")

res_2001_simpson <- simulateResiduals(glmm2001_simpson)
plotResiduals(res_2001_simpson, type = "standardized")
plotQQunif(res_2001_simpson)

res_2001_richness <- simulateResiduals(glmm2001_richness)
plotResiduals(res_2001_richness, type="standardized")
plotQQunif(res_2001_richness)

testing_data <- full_df %>% filter(year == 2019)
real_shannon_2019 <- testing_data$shannon
simpson_real2019 <- testing_data$simpson
richness_real2019 <- testing_data$richness

shannon_prediction <- predict(glmm2001_shannon, newdata = testing_data, type = "response")
simpson_pred <- predict(glmm2001_simpson, newdata = testing_data, type="response")
richness_pred <- predict(glmm2001_richness, newdata = testing_data, type = "response")

rmse_shannon <- sqrt(mean((shannon_prediction - real_shannon_2019)^2))
cor_shannon <- cor(shannon_prediction, real_shannon_2019)

rmse_simpson <- sqrt(mean((simpson_pred - simpson_real2019)^2))
cor_simpson <- cor(simpson_pred, simpson_real2019)

rmse_rich <- sqrt(mean((richness_pred - richness_real2019)^2))
cor_rich <- cor(richness_pred, richness_real2019)

print(paste0("The RMSE for Shannon index of 2001 and 2019 is: ", round(rmse_shannon, 3),
             ". The correlation is: ", 
             round(cor_shannon,3), ". The RMSE for Simspon index of 2001 and 2019 is: ",
             round(rmse_simpson, 3), " The correlation is: ", round(cor_simpson, 3), 
             ". The RMSE for sp richness of 2001 and 2019 is: ", round(rmse_rich, 3),
             ". The correlation is: ", round(cor_rich, 3), "."))

#===== GLMM models for 2016========

df2016 <- full_df %>% filter(year == 2016)
df2016 <- df2016 %>% filter(shannon > 0)

source("codes/data_exploration/glmm_model_builder.R")
glmm2016_shannon <- function_glmm(df2016, "shannon")
glmm2016_simpson <- function_glmm(df2016, "simpson")
glmm2016_richness <- function_glmm(df2016, "richness")


shannon_res2016 <- simulateResiduals(glmm2016_shannon)
plotResiduals(shannon_res2016, type = "standardized")
plotQQunif(shannon_res2016, main = "Q-Q Plot or standardized residuals")

simpson_res2016 <- simulateResiduals(glmm2016_simpson)
plotResiduals(simpson_res2016, type = "standardized")
plotQQunif(simpson_res2016)

rich_res2016 <- simulateResiduals(glmm2016_richness)
plotResiduals(rich_res2016, type="standardized")
plotQQunif(rich_res2016)

shannon_pred2016 <- predict(glmm2016_shannon, newdata = testing_data, type = "response")
simpson_pred2016 <- predict(glmm2016_simpson, newdata = testing_data, type="response")
richness_pred2016 <- predict(glmm2016_richness, newdata = testing_data, type = "response")

rmse_shannon2016 <- sqrt(mean((shannon_pred2016 - real_shannon_2019)^2))
cor_shannon2016 <- cor(shannon_pred2016, real_shannon_2019)

rmse_simpson2016 <- sqrt(mean((simpson_pred2016 - simpson_real2019)^2))
cor_simpson2016 <- cor(simpson_pred2016, simpson_real2019)

rmse_rich2016 <- sqrt(mean((richness_pred2016 - richness_real2019)^2))
cor_rich2016 <- cor(richness_pred2016, richness_real2019)

print(paste0("The RMSE for Shannon index of 2016 and 2019 is: ", round(rmse_shannon2016, 3),
             ". The correlation is: ", 
             round(cor_shannon2016,3), ". The RMSE for Simspon index of 2016 and 2019 is: ",
             round(rmse_simpson2016, 3), " The correlation is: ", round(cor_simpson2016, 3), 
             ". The RMSE for sp richness of 2016 and 2019 is: ", round(rmse_rich2016, 3),
             ". The correlation is: ", round(cor_rich2016, 3), "."))

#====Bayesian model for abundance
