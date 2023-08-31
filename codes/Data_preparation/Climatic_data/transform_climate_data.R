# Transform climate data
# Author: Jon Went
# Date: 31.08.2023

# -----

library(tidyverse)

# --- tmax ----

load("data/Climate/tmax_df.rda")
# summary(tmax_df)

tmax_celsius <- tmax_df %>%
  mutate(
    across(
      .cols = c("mean", "median", "min", "max"),
      .fns = c(
        celsius = \(x) ((x/10)-273.15)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(var = stdev^2) %>%
  relocate(var, .before = stdev) %>%
  select(-c(mean, median, stdev, max, min)) %>%
  rename(mean = mean_celsius,
         median = median_celsius,
         min = min_celsius,
         max = max_celsius) %>%
  relocate(c(mean, median), .before = var) %>%
  mutate(across(.cols = c(mean, median, var, max, min), .fns = \(x) round(x, 3)))

tmax.t1 <- tmax_celsius %>%
  filter(year %in% 1996:2001) %>%
  group_by(segment) %>%
  summarize(mean = mean(max),
            median = median(max),
            var = var(max)) %>%
  mutate(year = 2001) %>%
  relocate(year)

tmax.t2 <- tmax_celsius %>%
  filter(year %in% 2014:2019) %>%
  group_by(segment) %>%
  summarize(mean = mean(max),
            median = median(max),
            var = var(max)) %>%
  mutate(year = 2019) %>%
  relocate(year)

tmax.df <- rbind(tmax.t1, tmax.t2)

summary(tmax.t1)
summary(tmax.t2)

rm(tmax_celsius, tmax_df)

# ---- tmin ----

load("data/Climate/tmin_df.rda")

tmin_celsius <- tmin_df %>%
  mutate(
    across(
      .cols = c("mean", "median", "min", "max"),
      .fns = c(
        celsius = \(x) ((x/10)-273.15)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(var = stdev^2) %>%
  relocate(var, .before = stdev) %>%
  select(-c(mean, median, stdev, max, min)) %>%
  rename(mean = mean_celsius,
         median = median_celsius,
         min = min_celsius,
         max = max_celsius) %>%
  relocate(c(mean, median), .before = var) %>%
  mutate(across(.cols = c(mean, median, var, max, min), .fns = \(x) round(x, 3)))

tmin.t1 <- tmin_celsius %>%
  filter(year %in% 1996:2001) %>%
  group_by(segment) %>%
  summarize(mean = mean(min),
            median = median(min),
            var = var(min)) %>%
  mutate(year = 2001) %>%
  relocate(year)

tmin.t2 <- tmin_celsius %>%
  filter(year %in% 2014:2019) %>%
  group_by(segment) %>%
  summarize(mean = mean(min),
            median = median(min),
            var = var(min)) %>%
  mutate(year = 2019) %>%
  relocate(year)

summary(tmin.t1)
summary(tmin.t2)

tmin.df <- rbind(tmin.t1, tmin.t2)

rm(tmin_celsius, tmin_df)

# ---- transform cmi data ---- 

load("data/Climate/cmi_df.rda")

# ---- annual cmi ----
cmi_annual.t1 <- cmi_df %>%
  filter(year %in% 1996:2001) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var)) %>%
  mutate(year = 2001)

cmi_annual.t2 <- cmi_df %>%
  filter(year %in% 2014:2019) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var)) %>%
  mutate(year = 2019)

cmi_annual.df <- rbind(cmi_annual.t1, cmi_annual.t2)

summary(cmi_annual.t1)
summary(cmi_annual.t2)

# ----- summer-winter cmi difference ----

cmi_summer.t1 <- cmi_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% 4:9,
         year %in% 1996:2001) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var))

cmi_summer.t2 <- cmi_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% 4:9,
         year %in% 2014:2019) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var))

cmi_winter.t1 <- cmi_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% c(1:3,10:12),
         year %in% 1996:2001) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var))

cmi_winter.t2 <- cmi_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% c(1:3,10:12),
         year %in% 2014:2019) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = mean(var))

cmi_diff.t1 <- cmi_summer.t1 %>%
  summarize(segment = segment,
            mean = cmi_summer.t1$mean - cmi_winter.t1$mean,
            median = cmi_summer.t1$median - cmi_winter.t1$median) %>%
  mutate(year = 2001)

cmi_diff.t2 <- cmi_summer.t2 %>%
  summarize(segment = segment,
            mean = cmi_summer.t2$mean - cmi_winter.t2$mean,
            median = cmi_summer.t2$median - cmi_winter.t2$median) %>%
  mutate(year = 2019)

pr_diff <- rbind(cmi_diff.t1, cmi_diff.t2)

summary(cmi_diff.t1)
summary(cmi_diff.t2)

rm(cmi_df)

# --- pr ---- 

load("data/Climate/pr_df.rda")


# ---- annual precipitation sum ----
pr_sum.t1 <- pr_df %>%
  filter(year %in% 1996:2001) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var)) %>%
  mutate(year = 2001)

pr_sum.t2 <- pr_df %>%
  filter(year %in% 2014:2019) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var)) %>%
  mutate(year = 2019)

pr_sum.df <- rbind(pr_sum.t1, pr_sum.t2)

summary(pr_sum.t1)
summary(pr_sum.t2)

# ----- summer-winter precipitation difference ----

pr_summer.t1 <- pr_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% 4:9,
         year %in% 1996:2001) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var))

pr_summer.t2 <- pr_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% 4:9,
         year %in% 2014:2019) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var))

pr_winter.t1 <- pr_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% c(1:3, 10:12),
         year %in% 1996:2001) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var))

pr_winter.t2 <- pr_df %>%
  mutate_at(c("month"), as.numeric) %>%
  filter(month %in% c(1:3, 10:12),
         year %in% 2014:2019) %>%
  group_by(year, segment) %>%
  mutate(var = stdev^2) %>%
  summarize(sum_mean = sum(mean),
            sum_median = sum(median),
            var = var(var)) %>%
  ungroup() %>%
  group_by(segment) %>%
  summarize(sum_mean = mean(sum_mean),
            sum_median = mean(sum_median),
            var = mean(var))

pr_diff.t1 <- pr_summer.t1 %>%
  summarize(segment = segment,
    mean = pr_summer.t1$sum_mean-pr_winter.t1$sum_mean,
    median = pr_summer.t1$sum_median-pr_winter.t1$sum_median) %>%
  mutate(year = 2001)

pr_diff.t2 <- pr_summer.t2 %>%
  summarize(segment = segment,
            mean = pr_summer.t2$sum_mean-pr_winter.t2$sum_mean,
            median = pr_summer.t2$sum_median-pr_winter.t2$sum_median) %>%
  mutate(year = 2019)

pr_diff <- rbind(pr_diff.t1, pr_diff.t2)

summary(pr_diff.t1)
summary(pr_diff.t2)

rm(pr_df)

# ---- swb ----

load("data/Climate/swb_df.rda")

swb.t1 <- swb_df %>%
  filter(year %in% 1996:2001) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = var(var)) %>%
  mutate(year = 2001) %>%
  relocate(year)

swb.t2 <- swb_df %>%
  filter(year %in% 2014:2019) %>%
  mutate(var = stdev^2) %>%
  group_by(segment) %>%
  summarize(mean = mean(mean),
            median = mean(median),
            var = var(var)) %>%
  mutate(year = 2019) %>%
  relocate(year)

summary(swb.t1)
summary(swb.t2)

swb.df <- rbind(swb.t1, swb.t2)

rm(swb_df)
