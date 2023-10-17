# make tables
# date: 13.10.2023

# -----
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

lc.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarize(
    mean = mean(val),
    median = median(val),
    sd = sd(val)
  )

lc.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
  ggplot(aes(y = val, x = var)) +
  geom_violin()

lc.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val, y = var)) +
  ggridges::geom_density_ridges_gradient(
    scale = 1,
    rel_min_height = 0.005
  ) +
  xlim(-2, 2)

lc.df %>%
  select(segment, contains("delta")) %>%
  ungroup() %>%
  # pivot_longer(cols = 2:5, names_to = "var", values_to = "val") %>%
  ggplot() +
  # geom_histogram(aes(x = delta.urban.area.m2.log), bins = 80) +
  geom_histogram(aes(x = delta.forest.area.m2.log), bins = 80)

dlc %>%
  ungroup() %>%
  summarize(
    across(
      .cols = matches("delta"),
      .fns = list(
        mean = \(.) mean(.),
        # median = \(.) median(.),
        sd = \(.) sd(.)
      ),
      .names = "{.fn}.{col}"
    )
  ) %>%
  pivot_longer(cols = 1:8, names_to = "var", values_to = "values") #%>%
# mutate(pct_change = values/400000)

# ---- climate change -----

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

# ---- 

ttt <- adj_r2_lc %>%
  select(bird, adj.r2, adj.r2_clim, adj.r2_lc) %>%
  arrange(bird)

adj_r2_lc %>%
  select(bird, adj.r2, adj.r2_clim, adj.r2_lc) %>%
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "values") %>%
  group_by(variable) %>%
  summarize(
    mean = mean(values),
    sd = sd(values),
    median = median(values)
  )

xtable(ttt)
