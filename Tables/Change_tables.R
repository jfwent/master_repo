# make tables
# date: 13.10.2023

# ---- libraries ----
# install.packages("sjPlot")
# library(sjPlot)
# install.packages("stargazer")
# install.packages("xtable")
library(xtable)
library(stargazer)
library(tidyverse)

# ---- load bird data ----
load("data/d.abund.min40.rda")
load("data/species_traits.rda")

# ---- prepare traits data ----

final_birds <- unique(abund.min40.lc$animal_jetz)

species.traits.sub <- species.traits %>%
  select(-c(tot_diet_div, shannon, Clutch)) %>%
  rename(Species =  animal_jetz,
         Common.name = Common.Name,
         Gen.length = GenLength,
         Clutch = Clutch.Bird,
         Habitat.breadth = hab.breadth,
         Tot.innov = tot.innov,
         Hand.wing.ind = hand.wing.ind,
         Diet.breadth = diet.breadth,
         Rel.brain.size = rel_brain_size,
         Pop.trend.Sauer = sauer.trend,
         Pop.trend.ACAD = ACAD.ind) %>%
  relocate(Common.name, .after = Species) %>%
  relocate(Hand.wing.ind, .after = Migrant) %>%
  relocate(body.mass, .after = Hand.wing.ind) %>%
  relocate(Habitat.breadth, .after = body.mass) %>%
  relocate(Diet.breadth, .after = Habitat.breadth) %>%
  relocate(Trophic.Level, .after = Diet.breadth) %>%
  relocate(Trophic.Niche, .after = Trophic.Level) %>%
  relocate(Gen.length, .after = Trophic.Niche) %>%
  relocate(Clutch, .after = Gen.length) %>%
  filter(Species %in% final_birds) %>%
  arrange(Species)

xtable(species.traits.sub, caption = "Bird species traits")

# ---- mean and median abundance changes ----

abund.min40.lc %>%
  group_by(animal_jetz) %>%
  reframe(
    mean.abund = mean(delta.abund),
    median.abund = median(delta.abund),
    sd.abund = sd(delta.abund)
  ) %>%
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "values") %>%
  group_by(variable) %>%
  reframe(mean = mean(values),
          median = median(values),
          sd = sd(values)) %>%
  print(n = 90)

abund.min40.lc %>%
  group_by(animal_jetz) %>%
  reframe(
    # mean.abund = mean(delta.abund),
    median.abund = median(delta.abund),
    sd.abund = sd(delta.abund)
  ) %>%
  pivot_longer(cols = 2:3, names_to = "variable", values_to = "values") %>%
  group_by(variable) %>%
  reframe(
    n_decrease = sum(values < 0),
    n_increase = sum(values > 0),
    mean = mean(values),
    median = median(values),
    sd = sd(values)) %>%
  print(n = 90)


# ---- relative abundance changes ----
load("data/BBS.full.stable.min40.rda")
load("data/d.abund.min40.rda")



ttt <-
  BBS.stable.full.min40 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = abund.geom.mean - lag(abund.geom.mean)) %>%
  group_by(segment, animal_jetz) %>%
  mutate(n_years = n()) %>%
  filter(all(abund.geom.mean !=0),
         animal_jetz %in% abund.min40.lc$animal_jetz,
         segment %in% abund.min40.lc$segment,
         n_years == 2
         ) %>%
  group_by(year, animal_jetz) %>%
  reframe(
    # tot.abund = sum(abund.geom.mean),
    median.abund = median(abund.geom.mean),
    mean.abund = mean(abund.geom.mean)
  ) %>%
  arrange(animal_jetz) %>%
  group_by(animal_jetz) %>%
  mutate(delta.median = median.abund - lag(median.abund))

delta.median <- ttt %>% na.omit() %>%
  select(delta.median)

median.t1 <- ttt %>%
  filter(year == 2001) %>%
  select(median.abund)

tttt <- delta.median %>%
  left_join(median.t1, by = "animal_jetz") %>%
  group_by(animal_jetz) %>%
  mutate(rel_change = delta.median/median.abund*100) %>%
  ungroup()

tttt %>%
  reframe(
    n_pos = sum(rel_change > 5),
    n_neg = sum(rel_change < -5)
  )

tttt %>%
  reframe(median = median(rel_change),
          sd = sd(rel_change),
          mean = mean(rel_change))

  # pivot_wider(id_cols = "animal_jetz", names_from = "year", values_from = "tot.abund") %>%
  # group_by(animal_jetz) %>%
  # mutate(
  #   delta = 2019 - 2001,
  #   rel_change = 2019/2001
  # ) %>%
  # ungroup()

# ---- variance ---- 

adj_r2_lc %>%
  ungroup() %>%
  summarize(
    mean_v_join = mean(v.joint),
    median_vjoin = median(v.joint),
    sd_vjoin = sd(v.joint),
    mean_vclim = mean(v.clim),
    median_vclim = median(v.clim),
    sd_vclim = sd(v.clim),
    mean_vlc = mean(v.lc),
    median_vlc = median(v.lc),
    sd_vlc = sd(v.lc)
  )

adj_r2_lc %>%
  ungroup() %>%
  filter(v.joint < 0) %>%
  summarize(n_obs = n(),
            pct = n()/83*100)

adj_r2_lc %>%
  ungroup() %>%
  filter(v.joint > v.clim | v.joint > v.lc) %>%
  summarize(n_obs = n(),
            pct = n()/83*100)

adj_r2_lc %>%
  ungroup() %>%
  filter(v.joint < v.clim | v.joint < v.lc,
         v.joint > 0) %>%
  summarize(n_obs = n(),
            pct = n()/83*100)

adj_r2_lc %>%
  ungroup() %>%
  filter(v.joint == v.clim | v.joint == v.lc) %>%
  summarize(n_obs = n(),
            pct = n()/83*100)


# ---- land use change ----

land_t1 <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
  ) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2,
            crop.area.m2
  )) %>%
  filter(year == 2001) %>%
  ungroup() %>%
  pivot_longer(cols = 3:6, names_to = "lc", values_to = "val") %>%
  group_by(lc) %>%
  summarize(
    mean = mean(val),
    median = median(val),
    sd = sd(val),
    amount = sum(val),
  ) %>%
  ungroup() %>%
  mutate(
    tot.amount = sum(amount)
  ) %>%
  group_by(lc) %>%
  mutate(rel_amount = amount/tot.amount*100)  %>%
  # pivot_longer(cols = 2:7, names_to = "var", values_to = "val") %>%
  mutate(year = 2001)

land_t2 <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
  ) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2,
            crop.area.m2
  )) %>%
  filter(year == 2019) %>%
  ungroup() %>%
  pivot_longer(cols = 3:6, names_to = "lc", values_to = "val") %>%
  group_by(lc) %>%
  summarize(
    mean = mean(val),
    median = median(val),
    sd = sd(val),
    amount = sum(val),
  ) %>%
  ungroup() %>%
  mutate(
    tot.amount = sum(amount)
  ) %>%
  group_by(lc) %>%
  mutate(rel_amount = amount/tot.amount*100) %>%
  mutate(year = 2019)22

land_seg_t1 <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
  ) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2,
            crop.area.m2
  )) %>%
  ungroup() %>%
  group_by(segment) %>%
  rowwise() %>%
  mutate(
    tot.amount = sum(c_across(2:5))
  ) %>%
  mutate(across(
    .cols = matches("area"),
    .fns = list(
      rel.amount = \(x) x/tot.amount*100
    ),
    .names = "{.fn}.{.col}"
  )) %>%
  filter(year == 2001) %>%
  ungroup() %>%
  na.omit() %>%
  reframe(
    across(
      .cols = matches("area"),
      .fns = list(
        mean = \(x) mean(x),
        sd = \(x) sd(x)
      ),
      .names = "{.fn}.{.col}"
    )
  ) %>%
  pivot_longer(cols = 1:16, names_to = "var",values_to = "val")

land_seg_t2 <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
  ) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2,
            crop.area.m2
  )) %>%
  ungroup() %>%
  group_by(segment) %>%
  rowwise() %>%
  mutate(
    tot.amount = sum(c_across(2:5))
  ) %>%
  mutate(across(
    .cols = matches("area"),
    .fns = list(
      rel.amount = \(x) x/tot.amount*100
    ),
    .names = "{.fn}.{.col}"
  )) %>%
  filter(year == 2019) %>%
  ungroup() %>%
  na.omit() %>%
  reframe(
    across(
      .cols = matches("area"),
      .fns = list(
        mean = \(x) mean(x),
        sd = \(x) sd(x)
      ),
      .names = "{.fn}.{.col}"
    )
  ) %>%
  pivot_longer(cols = 1:16, names_to = "var",values_to = "val")


# lc.df %>%
#   select(segment, contains("delta")) %>%
#   ungroup() %>%
#   pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
#   ggplot(aes(y = val, x = var)) +
#   geom_violin()
# 
# lc.df %>%
#   select(segment, contains("delta")) %>%
#   ungroup() %>%
#   pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
#   ggplot(aes(x = val, y = var)) +
#   ggridges::geom_density_ridges_gradient(
#     scale = 1,
#     rel_min_height = 0.005
#   ) +
#   xlim(-2, 2)
# 
# lc.df %>%
#   select(segment, contains("delta")) %>%
#   ungroup() %>%
#   # pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
#   ggplot() +
#   # geom_histogram(aes(x = delta.urban.area.m2.log), bins = 80) +
#   geom_histogram(aes(x = delta.forest.area.m2.log), bins = 80)
# 
# dlc %>%
#   ungroup() %>%
#   summarize(
#     across(
#       .cols = matches("delta"),
#       .fns = list(
#         mean = \(.) mean(.),
#         # median = \(.) median(.),
#         sd = \(.) sd(.)
#       ),
#       .names = "{.fn}.{col}"
#     )
#   ) %>%
#   pivot_longer(cols = 1:8, names_to = "var", values_to = "values") #%>%
# # mutate(pct_change = values/400000)

# ---- climate change -----

# clim_seg_t1 <- 
climate.df %>%
  filter(year == 2001,
         segment %in% d.abund.min40$segment) %>%
  ungroup() %>%
  reframe(
    across(
      .cols = matches("mean"),
      .fns = list(
        mean = \(x) mean(x),
        sd = \(x) sd(x)
      ),
      .names = "{.fn}.{.col}"
    )
  ) %>%
  pivot_longer(cols = 1:8, names_to = "var",values_to = "val")

climate.df %>%
  filter(year == 2019,
         segment %in% d.abund.min40$segment) %>%
  ungroup() %>%
  reframe(
    across(
      .cols = matches("mean"),
      .fns = list(
        mean = \(x) mean(x),
        sd = \(x) sd(x)
      ),
      .names = "{.fn}.{.col}"
    )
  ) %>%
  pivot_longer(cols = 1:8, names_to = "var",values_to = "val")

clim.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarize(
    mean = mean(val),
    median = median(val),
    sd = sd(val)
  )

moist <- clim.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = c(delta.cmi.diff.mean, delta.swb.mean), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = var, y = val, group = var)) +
  geom_boxplot()

moist

temp <- clim.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = c(delta.tmax.mean, delta.tmin.mean), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = var, y = val, group = var)) +
  geom_boxplot()

temp

dclim %>%
  ungroup() %>%
  summarize(
    across(
      .cols = matches("delta"),
      .fns = list(
        mean = \(.) mean(.),
        sd = \(.) sd(.)
      ),
      .names = "{.fn}.{col}"
    )
  ) %>%
  pivot_longer(cols = 1:8, names_to = "var", values_to = "values")

# ---- model results ----

ttt <- adj_r2_lc %>%
  select(bird, adj.r2, adj.r2_clim, adj.r2_lc) %>%
  arrange(bird)

xtable(ttt)

# ---- full LM model results ----

load("results/LMs_swb/LMs_no_crop/beta_coefficients_full_LMs.rda")
load("results/LMs_swb/LMs_no_crop/beta_coefficients_lc_LMs.rda")
load("results/LMs_swb/LMs_no_crop/beta_coefficients_clim_LMs.rda")
load("results/LMs_swb/LMs_no_crop/LOOCV_model_results.rda")

adj_r2_lc <- adj_r2_lc %>%
  rowwise() %>%
  mutate(v.clim = adj.r2 - adj.r2_lc,
         v.lc = adj.r2 - adj.r2_clim,
         v.joint = adj.r2 - (adj.r2_lc + adj.r2_clim))

adj_r2_lc %>%
  select(bird, adj.r2, adj.r2_clim, adj.r2_lc) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  reframe(
    mean = mean(r2s),
    median = median(r2s),
    sd = sd(r2s)
  )

adj_r2_lc %>%
  select(bird, v.joint, v.clim, v.lc) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  reframe(
    mean = mean(r2s),
    median = median(r2s),
    sd = sd(r2s)
  )

group1 <- adj_r2_lc_traits %>%
  drop_na(Migrant) %>%
  filter(Migrant %in% "Full Migrant") %>%
  select(Migrant, v.lc)

group2 <- adj_r2_lc_traits %>%
  drop_na(Migrant) %>%
  filter(Migrant %in% "Not a Migrant") %>%
  select(Migrant, v.lc)

# ttt <- kruskal.test(list(group1, group2))
# 
# ?kruskal.test
# 
# ttt <- wilcox.test(group1, group2)
# 
# str(adj_r2_lc_traits)

LOOCV_model_res %>%
  pivot_longer(!bird, names_to = "metric", values_to = "values") %>%
  group_by(metric) %>%
  reframe(
    metric.mean = mean(values),
    metric.sd = sd(values)
  )

clim_model_coefs %>%
  group_by(variable) %>%
  reframe(
    kept_pct = (n()/83)*100,
    beta.mean = mean(beta.coefs),
    beta.median = median(beta.coefs),
    beta.sd = sd(beta.coefs)
  ) %>%
  arrange(desc(kept_pct))

lc_model_coefs %>%
  group_by(variable) %>%
  reframe(
    kept_pct = (n()/83)*100,
    beta.mean = mean(beta.coefs),
    beta.median = median(beta.coefs),
    beta.sd = sd(beta.coefs)
  ) %>%
  arrange(desc(kept_pct))

full_model_coefs %>%
  group_by(variable) %>%
  reframe(
    kept_pct = (n()/83)*100,
    beta.mean = mean(beta.coefs),
    beta.median = median(beta.coefs),
    beta.sd = sd(beta.coefs)
  ) %>%
  arrange(desc(kept_pct))
