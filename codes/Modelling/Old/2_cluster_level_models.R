# Modelling the cluster level bird data
# Author: Jon Went, jwent@ethz.ch
# Date: 17.07.2023

# Always start with the 0_star.R file!

cluster_full_df$year <- as.integer(cluster_full_df$year)
mod1_dat <- bind_rows(cluster_stable_df, cluster_full_df)

# ---- Cluster model 1_1: log(richness) ~ year + (1 | cluster:ecoregion) ----

mod1.1 <- brm(log(richness) ~ year + (1 | cluster),
              data = mod1_dat,
              family = gaussian,
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

summary(mod1.1)
# cluster: 1.08 (CI: 0.97-1.19), intercept: -30.13 (CI: -39.55- -20.87), year: 0.02 (CI: 0.01-0.02)

# ---- cluster model 1_2: 
mod1_2_dat <- mod1_dat %>%
  arrange(year, cluster) %>%
  mutate(d_richness = richness - lag(richness)) %>%
  filter(!is.na(d_richness), d_richness != 0)

mod1_2 <- brm(log(richness) ~ year * log(d_richness) + (1 | cluster),
              data = mod1_2_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 4000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

summary(mod1_2) # model does not converge... Rhats > 1.05

#----- Cluster Model 2: log(richness2019) ~ log(richness.yearX) + (1 | cluster:ecoregion) ----

# Create an empty list to store the model summaries
mod2_cluster_summaries <- list()

# Define the years you want to use as predictors
predictor_years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016)

# Loop through each predictor year
for (i in seq_along(predictor_years)) {
  # Subset the data for the current predictor year and 2019
  current_year <- predictor_years[i]
  
  current_dat <- mod1_dat %>%
    filter(year %in% c(current_year, 2019)) %>%
    mutate(year_label = ifelse(year == current_year, "1", "2")) %>%
    select(cluster, richness, year_label) %>%
    pivot_wider(names_from = "year_label",
                values_from = "richness",
                names_prefix = "richness.") %>%
    filter(richness.1 != 0, !is.na(richness.1),
           richness.2 != 0, !is.na(richness.2))
  #
  # current_dat <- current_dat[current_dat[, i + 3] != 0 & !is.na(current_dat[, i + 3]), ]
  # current_dat <- current_dat[current_dat[, 5] != 0 & !is.na(current_dat[, 5]), ]
  
  
  # Build the model for the current predictor year
  model <- brm(log(richness.2) ~ log(richness.1) + (1 | cluster),
               data = current_dat,
               family = gaussian(),
               warmup = 1000,
               iter = 2000,
               chains = 2,
               init = 0,
               save_pars = save_pars(all = TRUE),
               cores = 2,
               seed = 123)
  
  # Save the model summary in the list with the year as the name
  mod2_cluster_summaries[[as.character(current_year)]] <- summary(model)
}


# ---- access model summaries -----

model_summary_2001 <- mod2_cluster_summaries[["2001"]]
model_summary_2004 <- mod2_cluster_summaries[["2004"]]
model_summary_2006 <- mod2_cluster_summaries[["2006"]]
model_summary_2008 <- mod2_cluster_summaries[["2008"]]
model_summary_2011 <- mod2_cluster_summaries[["2011"]]
model_summary_2013 <- mod2_cluster_summaries[["2013"]]
model_summary_2016 <- mod2_cluster_summaries[["2016"]]

model_summary_2001
# cluster = 0.31 (CI: 0.02-0.60), Intercept: 1.56 (CI: 1.17-1.94), log(richness.2001) =  0.77 (CI: 0.69-0.85)
model_summary_2004
# cluster = 0.26 (CI: 0.01-0.55) Rhat: 1.07!!, Intercept: 1.15 (CI: 0.69-1.57), log(richness.2004) =  0.78 (CI: 0.70-0.86)
model_summary_2006
# cluster = 0.38 (CI: 0.04-0.67) Rhat: 1.21!!, Intercept: 1.32 (CI: 0.80-1.83), log(richness.2006) =  0.75 (CI: 0.65-0.86)
model_summary_2008
# cluster = 0.42 (CI: 0.02-0.71) Rhat: 1.08!!, Intercept: 1.45 (CI: 0.94-1.94), log(richness.2008) =  0.72 (CI: 0.63-0.82)
model_summary_2011
# cluster = 0.26 (CI: 0.01-0.53) Rhat: 1.07!!, Intercept: 1.02 (CI: 0.56-1.41), log(richness.2011) =  0.80 (CI: 0.72-0.89)
model_summary_2013
# cluster = 0.17 (CI: 0.01-0.37), Intercept: 0.85 (CI: 0.47-0.1.23), log(richness.2013) =  0.83 (CI: 0.76-0.90)
model_summary_2016
# cluster = 0.26 (CI: 0.01-0.50) Rhat: 1.15!!, Intercept: 0.79 (CI: 0.46-1.16), log(richness.2016) =  0.84 (CI: 0.78-0.91)
