# Start modelling the segment level data
# Author: Jon Went, jwent@ethz.ch
# Date: 17.07.2023

#Always start with the 0_star.R file!

segments_full_df$year <- as.integer(segments_full_df$year)
seg_dat <- bind_rows(segments_stable_df, segments_full_df)

# ----- prepare mod1 data -----
mod1_dat <- seg_dat %>%
  arrange(year, segment) %>%
  mutate(d_richness = richness - lag(richness)) %>%
  filter(richness != 0, !is.na(d_richness), d_richness != 0)

# ---- mod1_1: log(richness) ~ year + (1 | cluster:ecoregion) -----
# Write here the RQ to be answered!

mod1_1 <- brm(log(richness) ~ year + (1 | cluster:ecoregion),
              data = mod1_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

sum_mod1_1 <- summary(mod1_1)
# year = 0.01 (CI: 0.01-0.02), cluster:ecoregion --> intercept = 0.32 with CI: 0.29-0.35

# ------ mod1_2: log(richness) ~ year * log(d_richness) + (1 | cluster:ecoregion) ----
#
mod1_2 <- brm(log(richness) ~ year * log(d_richness) + (1 | cluster:ecoregion),
              data = mod1_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

sum_mod1_2 <- summary(mod1_2)

# Problem with year:log(d_richness) and also with log(d_richness) --> Rhat > 1 and Bulk_ESS and Tail_ESS small (5)
# However, intercept of cluster:ecoregion is now smaller 1.23 (CI: 0.87-1.87)
# pop-level effects: Intercept -22.26 is smaller, year is the same, 

# --> wrong distribution family used? try with pp_check and see if d_richness is normally distributed
# if it is normally distributed then use more iterations 

# ---- mod1_3: shannon ~ year + (1 | cluster:ecoregion) -----

mod1_3 <- brm(shannon ~ year + (1 | cluster:ecoregion),
              data = mod1_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

sum_mod1_3 <- summary(mod1_3)
# Intercept -18.45 (CI: -(20.68 + 16.23)), year: 0.01 (CI: 0.01-0.01), cluster in ecoregion: 0.33 (CI: 0.30 - 0.37)

# ----- mod1_4: simpson ~ year + (1 | cluster:ecoregion) ---- 

mod1_4 <- brm(simpson ~ year + (1 | cluster:ecoregion),
              data = mod1_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

sum_mod1_4 <- summary(mod1_4)
# year 0 effect, Intercept = -0.98 (CI: -(1.4 + 0.59)), cluster in ecoregion: 0.05 (CI: 0.04 - 0.05)


# ----- save model 1 summaries -----

summaries_mod1 <- list()

summaries_mod1[["mod1_1"]] <- sum_mod1_1
summaries_mod1[["mod1_2"]] <- sum_mod1_2
summaries_mod1[["mod1_3"]] <- sum_mod1_3
summaries_mod1[["mod1_4"]] <- sum_mod1_4

save(summaries_mod1, file = "data/model_summaries/summaries_mod1_list.rda")

load("data/model_summaries/summaries_mod1_list.rda")

summaries_mod1[[2]]
??pp_check
# ----- mod 2 data -----

mod2_dat <- seg_dat %>%
  select(cluster, segment, year, ecoregion, richness) %>%
  pivot_wider(names_from = "year",
              values_from = "richness",
              names_prefix = "richness.")

# ---- mod2_1: log(richness2019) ~ log(richness2001) + (1 | cluster:ecoregion) ----

# Test:

mod2_1_dat <- mod2_dat %>%
  select(segment, cluster, ecoregion, richness.2001, richness.2019) %>%
  filter(richness.2001 != 0, !is.na(richness.2001), richness.2019 != 0, !is.na(richness.2019))

mod2_1 <- brm(log(richness.2019) ~ log(richness.2001) + (1 | cluster:ecoregion),
              data = mod2_1_dat,
              family = gaussian(),
              warmup = 1000,
              iter = 2000,
              chains = 2,
              init = 0,
              save_pars = save_pars(all = T),
              cores = 2,
              seed = 123)

summary(mod2_1)
# cluster:ecoregion = 0.11 (CI: 0.08-0.14), Intercept: 1.69 (CI: 1.58-1.80), log(richness.2001) =  0.56 (CI: 0.52-0.60)

# ---- buil same model for all years: log(richness.2019) ~ log(richness.yearX) + (1 | cluster:ecoregion) -----

# Create an empty list to store the model summaries
mod2_summaries <- list()

# Define the years you want to use as predictors
predictor_years <- c(2004, 2006, 2008, 2011, 2013, 2016)

# Loop through each predictor year
for (i in seq_along(predictor_years)) {
  # Subset the data for the current predictor year and 2019
  current_year <- predictor_years[i]
  
  current_dat <- seg_dat %>%
    filter(year %in% c(current_year, 2019)) %>%
    mutate(year_label = ifelse(year == current_year, "1", "2")) %>%
    select(cluster, segment, ecoregion, richness, year_label) %>%
    pivot_wider(names_from = "year_label",
                values_from = "richness",
                names_prefix = "richness.") %>%
    filter(richness.1 != 0, !is.na(richness.1),
           richness.2 != 0, !is.na(richness.2))
  #
  # current_dat <- current_dat[current_dat[, i + 3] != 0 & !is.na(current_dat[, i + 3]), ]
  # current_dat <- current_dat[current_dat[, 5] != 0 & !is.na(current_dat[, 5]), ]
  
  
  # Build the model for the current predictor year
  model <- brm(log(richness.2) ~ log(richness.1) + (1 | cluster:ecoregion),
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
  mod2_summaries[[as.character(current_year)]] <- summary(model)
}

# Access the model summaries -----

model_summary_2001 <- mod2_summaries[["2001"]]
model_summary_2004 <- mod2_summaries[["2004"]]
model_summary_2006 <- mod2_summaries[["2006"]]
model_summary_2008 <- mod2_summaries[["2008"]]
model_summary_2011 <- mod2_summaries[["2011"]]
model_summary_2013 <- mod2_summaries[["2013"]]
model_summary_2016 <- mod2_summaries[["2016"]]

model_summary_2001
# cluster:ecoregion = 0.11 (CI: 0.08-0.14), Intercept: 1.69 (CI: 1.58-1.80), log(richness.2001) =  0.56 (CI: 0.52-0.60)
model_summary_2004
# cluster:ecoregion = 0.10 (CI: 0.08-0.13), Intercept: 0.83 (CI: 0.69-0.97), log(richness.2004) =  0.74 (CI: 0.69-0.78)
model_summary_2006
# cluster:ecoregion = 0.12 (CI: 0.09-0.15), Intercept: 0.85 (CI: 0.68-1.02), log(richness.2006) =  0.73 (CI: 0.68-0.78)
model_summary_2008
# cluster:ecoregion = 0.10 (CI: 0.08-0.13), Intercept: 0.78 (CI: 0.63-0.92), log(richness.2008) =  0.75 (CI: 0.71-0.80)
model_summary_2011
# cluster:ecoregion = 0.06 (CI: 0.04-0.09), Intercept: 0.64 (CI: 0.51-0.77), log(richness.2011) =  0.80 (CI: 0.75-0.84)
model_summary_2013
# cluster:ecoregion = 0.07 (CI: 0.05-0.10), Intercept: 0.84 (CI: 0.73-0.96), log(richness.2013) =  0.74 (CI: 0.70-0.77)
model_summary_2016
# cluster:ecoregion = 0.09 (CI: 0.07-0.11), Intercept: 0.59 (CI: 0.47-0.71), log(richness.2016) =  0.81 (CI: 0.78-0.85)


# ---- save model 2 summaries ----

save(mod2_summaries, file = "data/model_summaries/summaries_mod2_list.rda")
