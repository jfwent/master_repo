# Linear model plots

# ---- library

library(tidyverse)
library(patchwork)
library(ggplot2)

# ---- load data ----

load("results/adj_R2_trained_LMs.rda")
load("results/beta_coefficients_full_LMs.rda")
load("data/species_traits.rda")


# --- prepare the data ----

species.traits <- species.traits %>%
  rename(bird =  animal_jetz) %>%
  dplyr::select(-c(Common.Name, tot_diet_div, shannon, Clutch)) #%>%
  # mutate(na.num = rowSums(is.na(.)))

# traits <- colnames(species.traits[2:14])

adj_r2_lc_traits <- adj_r2_lc %>%
  left_join(species.traits, by = "bird") %>%
  distinct()


# ---- r2 plots ----

adj_r2_lc <- adj_r2_lc %>%
  rowwise() %>%
  mutate(v.clim = adj.r2 - adj.r2_lc,
         v.lc = adj.r2 - adj.r2_clim,
         v.joint = adj.r2 - (adj.r2_lc + adj.r2_clim))

adj_r2_lc_traits <- adj_r2_lc_traits %>%
  rowwise() %>%
  mutate(v.clim = adj.r2 - adj.r2_lc,
         v.lc = adj.r2 - adj.r2_clim,
         v.joint = adj.r2 - (adj.r2_lc + adj.r2_clim))

ttt <- adj_r2_lc %>%
  dplyr::select(-c(adj.r2, adj.r2_clim, adj.r2_lc)) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  summarize(r2s = mean(r2s)) %>%
  mutate(bird = "Mean") %>%
  relocate(bird)


# All species
all_species_stacked <-
  adj_r2_lc %>%
  dplyr::select(-c(adj.r2, adj.r2_clim, adj.r2_lc)) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  add_row(.data = ttt) %>%
  ggplot(aes(y = reorder(bird, r2s), x = r2s, fill = var.type)) +
  geom_bar(position = "stack", stat = "identity",
           color = "grey40", alpha = 0.9, linewidth = 0.2,
           width = 0.7) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  ylab("") +
  xlab(expression(paste("adj. R"^2))) +
  theme(axis.text.y = element_text(size = 6)) +
  labs(fill = "Model type")

all_species_stacked

ggsave("figures/LM_Results/all_species_stacked.png", plot = all_species_stacked,
       width = 8, height = 6, dpi = 300)


# adj_r2_lc %>%
#   dplyr::select(-c(adj.r2, adj.r2_clim, adj.r2_lc)) %>%
#   pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
#   group_by(var.type) %>%
#   summarize(mean = mean(r2s)) %>%
#   ggplot(aes(y = mean, x = var.type, fill = var.type)) +
#   geom_bar(position = "stack", stat = "identity")

adj_r2_lc %>%
  dplyr::select(-c(adj.r2, adj.r2_clim, adj.r2_lc)) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  summarize(mean = mean(r2s)) %>%
  mutate(y_axis.tmp = 1) %>%
  ggplot(aes(x = y_axis.tmp, y = mean, fill = var.type)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")

# adj_r2_lc %>%
#   pivot_longer(!bird, names_to = "model_type", values_to = "r2s") %>%
#   ggplot(aes(x = model_type, y = r2s, group = model_type, fill = model_type)) +
#   geom_violin(alpha = 0.5) +
#   geom_boxplot(alpha = 0.5, width = 0.2) +
#   viridis::scale_fill_viridis(discrete = T, option = "inferno") +
#   stat_summary(fun.y=mean, geom="point", shape=20, size=3,
#                # color="red3", fill="red3",
#                alpha = 0.8)

# ---- life history traits and variance ----

gen.plot <- adj_r2_lc_traits %>%
  drop_na(GenLength) %>%
  mutate(gen.length.bins = cut(GenLength,
                               breaks = c(0, 2.5, 4, 12),
                               labels = c("short (< 2.5y)", "medium (2.5 - 4y)", "long (> 4y)"))) %>%
  group_by(gen.length.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(gen.length.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = gen.length.bins, y = R2,
             # group = gen.length.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Generation Length") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  theme(legend.position = "none") +
  ylim(0, 0.25)

clutch.plot <- adj_r2_lc_traits %>%
  drop_na(Clutch.Bird) %>%
  mutate(clutch.size.bins = cut(Clutch.Bird,
                                breaks = c(0, 3, 5, 12),
                                labels = c("small (< 3)",
                                           "medium (3 - 5)",
                                           "large (> 5)"))) %>%
  group_by(clutch.size.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(clutch.size.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = clutch.size.bins, y = R2,
             # group = clutch.size.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Clutch size") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0, 0.25)

brain.plot <- adj_r2_lc_traits %>%
  drop_na(rel_brain_size) %>%
  mutate(rel.brain.size.bins = cut(rel_brain_size,
                                   breaks = c(0, 2.5, 4, 6),
                                   labels = c("small",
                                              "medium",
                                              "large"))) %>%
  group_by(rel.brain.size.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(rel.brain.size.bins, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = rel.brain.size.bins, y = variance,
             # group = rel.brain.size.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Relative brain size") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  theme(legend.position = "none") +
  ylim(0, 0.25)

innov.plot <- adj_r2_lc_traits %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (<= 4)", "many (> 4)"))) %>%
  group_by(tot.innov.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(tot.innov.bins, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = tot.innov.bins, y = variance,
             # group = tot.innov.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Number of innovations") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0, 0.25)

life_history_plot <- gen.plot + clutch.plot + brain.plot + innov.plot
life_history_plot

ggsave(plot = life_history_plot, filename = "figures/LM_results/life_history_traits_adj_r2_stacked.png",
       width = 8, height = 6, dpi = 300)

# ---- ecological traits and variance ----

diet.plot <-adj_r2_lc_traits %>%
  drop_na(diet.breadth) %>%
  mutate(diet.breadth.bins = cut(diet.breadth,
                                 breaks = c(0, 1.5, 2, 3),
                                 labels = c("Low", "Medium", "High"))) %>%
  group_by(diet.breadth.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n(),
  ) %>%
  pivot_longer(!c(diet.breadth.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = diet.breadth.bins, y = R2,
             # group = diet.breadth.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Diet breadth") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  theme(legend.position = "none") +
  ylim(0, 0.25)

habitat.plot <- adj_r2_lc_traits %>%
  drop_na(hab.breadth) %>%
  mutate(hab.breadth.bins = cut(hab.breadth,
                                breaks = c(0, 3, 5, 10),
                                labels = c("Low", "Medium", "High"))) %>%
  group_by(hab.breadth.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(hab.breadth.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = hab.breadth.bins, y = R2,
             # group = hab.breadth.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Habitat breadth") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0, 0.25) +
  theme(legend.position = "none")

trophic.niche.plot <- adj_r2_lc_traits %>%
  group_by(Trophic.Niche) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(Trophic.Niche, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = Trophic.Niche, y = variance,
             # group = Trophic.Niche,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Trophic Niche") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)  +
  theme(legend.position = "none") +
  ylim(0, 0.5)

trophic.level.plot <- adj_r2_lc_traits %>%
  group_by(Trophic.Level) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(Trophic.Level, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = factor(Trophic.Level), y = variance,
             # group = factor(Trophic.Level),
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Trophic Level") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")
                    ) +
  labs(fill = "Model type") +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0, 0.25)

ecological_plot <- diet.plot + habitat.plot + trophic.level.plot
ecological_plot

ggsave(plot = ecological_plot, filename = "figures/LM_results/eco_traits_adj_r2_stacked.png",
       width = 8, height = 6, dpi = 300)

# ---- morphological traits and variance ----

mass.plot <-adj_r2_lc_traits %>%
  drop_na(body.mass) %>%
  mutate(body.mass.bins = cut(body.mass,
                              breaks = c(0, 30, 50, 2000),
                              labels = c("small (< 30 g)", "medium (30 - 50 g)", "large (> 50 g)"))) %>%
  group_by(body.mass.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(body.mass.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = body.mass.bins, y = R2,
             # group = body.mass.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Body mass") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0,0.25) +
  theme(legend.position = "none")

wing.plot <- adj_r2_lc_traits %>%
  drop_na(hand.wing.ind) %>%
  mutate(hand.wing.ind.bin = cut(hand.wing.ind,
                                 breaks = c(0, 20, 35, 72),
                                 labels = c("small","medium", "large"))) %>%
  group_by(hand.wing.ind.bin) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(hand.wing.ind.bin, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = hand.wing.ind.bin, y = R2,
             # group = hand.wing.ind.bin,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Hand wing index") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0,0.25)  +
  theme(legend.position = "none")

mig.plot <- adj_r2_lc_traits %>%
  drop_na(Migrant) %>%
  group_by(Migrant) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()) %>%
  pivot_longer(!c(Migrant, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = Migrant, y = variance,
             # group = Migrant,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity",
           color = "grey30", alpha = 0.8, size = 0.3) +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0,0.25)

morph_plot <- mass.plot + wing.plot + mig.plot
morph_plot

ggsave(morph_plot, filename = "figures/LM_results/morph_traits_adj_r2_stacked.png",
       width = 8, height = 6, dpi = 300)

# ---- population trends and variance ----

# adj_r2_lc_traits %>%
#   group_by(ACAD.ind) %>%
#   summarize(mean.v.clim = mean(v.clim),
#             mean.v.lc = mean(v.lc),
#             mean.v.joint = mean(v.joint),
#             n_obs = n()) %>%
#   pivot_longer(!c(ACAD.ind, n_obs), names_to = "variable", values_to = "variance") %>%
#   ggplot(aes(x = ACAD.ind, y = variance, group = ACAD.ind, fill = variable)) +
#   geom_bar(position = "stack", stat = "identity", color = "grey30",
#            alpha = 0.8, size = 0.3) +
#   xlab("ACAD population trend") +
#   ylab("Mean R2") +
#   scale_fill_manual(values=c("grey40", "grey70","grey100"),
#                     labels = c("Climate", "Full", "Land cover")) +
#   labs(fill = "Model type")  +
#   geom_text(aes(label = n_obs),
#             stat = "count", vjust= -0.2, y = 0.01)

acad.plot <- adj_r2_lc_traits %>%
  drop_na(ACAD.ind) %>%
  mutate(ACAD.ind.bins = cut(ACAD.ind,
                             breaks = c(0, 2.5, 3.5, 6),
                             labels = c("Increasing","Stable" ,"Decreasing"))) %>%
  group_by(ACAD.ind.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()) %>%
  pivot_longer(!c(ACAD.ind.bins, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = ACAD.ind.bins, y = variance,
             # group = ACAD.ind.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("ACAD population trend") +
  ylab(expression(paste("adj. R"^2))) +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  theme(legend.position = "none") +
  ylim(0,0.25)

sauer.plot <- adj_r2_lc_traits %>%
  drop_na(sauer.trend) %>%
  mutate(sauer.trend.bins = cut(sauer.trend,
                                breaks = c(2, 0.2, -0.2, -4),
                                labels = c("Increasing", "Stable", "Decreasing"))) %>%
  group_by(sauer.trend.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()) %>%
  pivot_longer(!c(sauer.trend.bins, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = sauer.trend.bins, y = variance,
             # group = sauer.trend.bins,
             fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Sauer population trend") +
  ylab("") +
  scale_fill_manual(values=c("grey100", "grey70","grey40"),
                    labels = c("Climate", "Full", "Land cover")
  ) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01) +
  ylim(0,0.25)

pop_stacked <- acad.plot + sauer.plot
pop_stacked

ggsave(pop_stacked, filename = "figures/LM_results/pop_trends_adj_r2_stacked.png",
       width = 8, height = 6, dpi = 300)

# ----- all traits ----

stacked_final <- diet.plot + habitat.plot + trophic.niche.plot + trophic.level.plot +
  mass.plot + wing.plot + mig.plot + gen.plot + clutch.plot + brain.plot + innov.plot

stacked_final

# ---- beta coefs vs gen length plots ----

p2 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmax.mean) %>%
  ggplot(aes(y = `delta.tmax.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p1 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmax.mean) %>%
  ggplot(aes(y = `tmax.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p3 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmin.mean) %>%
  ggplot(aes(y = `tmin.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p4 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmin.mean) %>%
  ggplot(aes(y = `delta.tmin.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p6 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.pr.sum.mean) %>%
  ggplot(aes(y = `delta.pr.sum.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p5 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(pr.sum.mean) %>%
  ggplot(aes(y = `pr.sum.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p7 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(cmi.diff.mean) %>%
  ggplot(aes(y = `cmi.diff.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p8 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.cmi.diff.mean) %>%
  ggplot(aes(y = `delta.cmi.diff.mean`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p9 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(urban.area.m2.log) %>%
  ggplot(aes(y = `urban.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p10 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.urban.area.m2.log) %>%
  ggplot(aes(y = `delta.urban.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p11 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(crop.area.m2.log) %>%
  ggplot(aes(y = `crop.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p12 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.crop.area.m2.log) %>%
  ggplot(aes(y = `delta.crop.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p14 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.forest.area.m2.log) %>%
  ggplot(aes(y = `delta.forest.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p13 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(forest.area.m2.log) %>%
  ggplot(aes(y = `forest.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p15 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(all.grass.area.m2.log) %>%
  ggplot(aes(y = `all.grass.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

p16 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.all.grass.area.m2.log) %>%
  ggplot(aes(y = `delta.all.grass.area.m2.log`, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("log(Generation length)")

coef_plot_gen_length <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 

coef_plot_gen_length

# ---- beta coefs vs diet breadth plots ----

p2 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmax.mean) %>%
  ggplot(aes(y = `delta.tmax.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p1 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmax.mean) %>%
  ggplot(aes(y = `tmax.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p4 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmin.mean) %>%
  ggplot(aes(y = `delta.tmin.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p3 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmin.mean) %>%
  ggplot(aes(y = `tmin.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p6 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.pr.sum.mean) %>%
  ggplot(aes(y = `delta.pr.sum.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p5 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(pr.sum.mean) %>%
  ggplot(aes(y = `pr.sum.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p7 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(cmi.diff.mean) %>%
  ggplot(aes(y = `cmi.diff.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p8 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.cmi.diff.mean) %>%
  ggplot(aes(y = `delta.cmi.diff.mean`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p9 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(urban.area.m2.log) %>%
  ggplot(aes(y = `urban.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p10 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.urban.area.m2.log) %>%
  ggplot(aes(y = `delta.urban.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p11 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(crop.area.m2.log) %>%
  ggplot(aes(y = `crop.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p12 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.crop.area.m2.log) %>%
  ggplot(aes(y = `delta.crop.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p14 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.forest.area.m2.log) %>%
  ggplot(aes(y = `delta.forest.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p13 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(forest.area.m2.log) %>%
  ggplot(aes(y = `forest.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p15 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(all.grass.area.m2.log) %>%
  ggplot(aes(y = `all.grass.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

p16 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.all.grass.area.m2.log) %>%
  ggplot(aes(y = `delta.all.grass.area.m2.log`, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Diet breadth")

coef_plot_diet <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 

coef_plot_diet

# ---- beta coefs vs habitat breadth plots ----

p2 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmax.mean) %>%
  ggplot(aes(y = `delta.tmax.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p1 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmax.mean) %>%
  ggplot(aes(y = `tmax.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm")  +
  xlab("Habitat breadth")

p4 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmin.mean) %>%
  ggplot(aes(y = `delta.tmin.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p3 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmin.mean) %>%
  ggplot(aes(y = `tmin.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p6 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.pr.sum.mean) %>%
  ggplot(aes(y = `delta.pr.sum.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p5 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(pr.sum.mean) %>%
  ggplot(aes(y = `pr.sum.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p7 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(cmi.diff.mean) %>%
  ggplot(aes(y = `cmi.diff.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p8 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.cmi.diff.mean) %>%
  ggplot(aes(y = `delta.cmi.diff.mean`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p9 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(urban.area.m2.log) %>%
  ggplot(aes(y = `urban.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p10 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.urban.area.m2.log) %>%
  ggplot(aes(y = `delta.urban.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p11 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(crop.area.m2.log) %>%
  ggplot(aes(y = `crop.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p12 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.crop.area.m2.log) %>%
  ggplot(aes(y = `delta.crop.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p14 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.forest.area.m2.log) %>%
  ggplot(aes(y = `delta.forest.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p13 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(forest.area.m2.log) %>%
  ggplot(aes(y = `forest.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p15 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(all.grass.area.m2.log) %>%
  ggplot(aes(y = `all.grass.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

p16 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.all.grass.area.m2.log) %>%
  ggplot(aes(y = `delta.all.grass.area.m2.log`, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Habitat breadth")

coef_plot_hab <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 

coef_plot_hab

# ---- beta coefs vs migrant plots ----

p2 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmax.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.tmax.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p1 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmax.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `tmax.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant")  +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p4 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmin.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.tmin.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant")  +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p3 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmin.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `tmin.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p6 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.pr.sum.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.pr.sum.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p5 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(pr.sum.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `pr.sum.mean`, x = Migrant)) +
  geom_boxplot() +
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p7 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(cmi.diff.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `cmi.diff.mean`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p8 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.cmi.diff.mean) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.cmi.diff.mean`, x = Migrant))  +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p9 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(urban.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `urban.area.m2.log`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p10 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.urban.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.urban.area.m2.log`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p11 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(crop.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `crop.area.m2.log`, x = Migrant))  +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p12 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.crop.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.crop.area.m2.log`, x = Migrant))  +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p14 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.forest.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.forest.area.m2.log`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p13 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(forest.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `forest.area.m2.log`, x = Migrant))  +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p15 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(all.grass.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `all.grass.area.m2.log`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p16 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.all.grass.area.m2.log) %>%
  drop_na(Migrant) %>%
  ggplot(aes(y = `delta.all.grass.area.m2.log`, x = Migrant)) +
  geom_boxplot()+
  xlab("Migrant") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

coef_plot_migrant <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 

coef_plot_migrant

# ---- beta coefs vs innovativeness plots ----

p2 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  drop_na(delta.tmax.mean) %>%
  ggplot(aes(y = `delta.tmax.mean`, x = tot.innov.bins, group = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)
p2

p1 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmax.mean) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `tmax.mean`, x = tot.innov.bins, group = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness")  +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p4 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.tmin.mean) %>%
  drop_na(Migrant) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.tmin.mean`, x = tot.innov.bins, group = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness")  +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p3 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(tmin.mean) %>%
  drop_na(Migrant) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `tmin.mean`, x = tot.innov.bins, group = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p6 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.pr.sum.mean) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.pr.sum.mean`, x = tot.innov.bins, group = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p5 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(pr.sum.mean) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `pr.sum.mean`, x = tot.innov.bins)) +
  geom_boxplot() +
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p7 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(cmi.diff.mean) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `cmi.diff.mean`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p8 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.cmi.diff.mean) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.cmi.diff.mean`, x = tot.innov.bins))  +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p9 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(urban.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `urban.area.m2.log`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p10 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.urban.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.urban.area.m2.log`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p11 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(crop.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `crop.area.m2.log`, x = tot.innov.bins))  +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p12 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.crop.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.crop.area.m2.log`, x = tot.innov.bins))  +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p14 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.forest.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.forest.area.m2.log`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p13 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(forest.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `forest.area.m2.log`, x = tot.innov.bins))  +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p15 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(all.grass.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `all.grass.area.m2.log`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

p16 <- coefs_tib %>%
  pivot_wider(id_cols = bird, names_from = "variable", values_from = "beta.coefs") %>%
  left_join(species.traits, by = "bird") %>%
  drop_na(delta.all.grass.area.m2.log) %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                              breaks = c(-1, 0, 3, 45),
                              labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  ggplot(aes(y = `delta.all.grass.area.m2.log`, x = tot.innov.bins)) +
  geom_boxplot()+
  xlab("Innovativeness") +
  geom_text(aes(label = ..count..),
            stat = "count", vjust= -0.2, y = 0)

coef_plot_innov <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 

coef_plot_innov

# ---- continuous traits plots full ----

p1 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  # geom_abline() +
  # ylab("Residuals") +
  xlab("log(Generation length)")  +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  p2 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm")+
  # geom_abline() +
  xlab("Clutch size") +
  ylab("") +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  p3 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  p4 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm")+
  # geom_abline() +
  xlab("Habitat breadth") +
  # ylab("Residuals") +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  p5 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm")+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  p6 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("")
# geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1, 0.1)

p7 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Relative brain size") +
  # ylab("Residuals") +
  # geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)
  
  final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

final_plot

# ggsave(filename = "figures/contin_traits_residuals_full_LM.png", plot = final_plot,
#        width = 8, height = 6, dpi = 300)


# ---- categorical traits full ----

p1_bp <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  # geom_violin() +
  # ylab("") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p2_bp <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  # geom_violin() +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p3_bp <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Trophic Level") +
  # ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p4_bp <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed")

boxplot <- p1_bp + p2_bp + p3_bp + p4_bp

boxplot

# ggsave(filename = "figures/cat_traits_residuals_full_LM.png", plot = boxplot,
#        width = 8, height = 6, dpi = 300)


# ---- pop. trend plots full ---- 

p1 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = ACAD.ind, group = ACAD.ind)) +
  geom_boxplot() +
  xlab("ACAD pop. trend") +
  ylab("Mean R2")
# geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

p2 <- ggplot(adj_r2_lc_traits, aes(y = adj.r2, x = sauer.trend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm")  +
  # geom_abline() +
  xlab("Sauer's pop. trend") +
  ylab("")
# geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

pop_trend <- p1 + p2

pop_trend

# ggsave(filename = "figures/pop_trend_residuals_full_LM.png", plot = pop_trend,
# width = 8, height = 6, dpi = 300)

