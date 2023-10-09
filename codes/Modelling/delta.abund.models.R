# Build Delta-Abundance Model
# Author: Jon Went
# Date: 14.09.2023

# ---- library ----
library(tidyverse)
library(patchwork)
library(ggplot2)
# library(MASS)

# ==== with land cover vars -----
# ---- LM function -----

birds <- unique(abund.min40.lc$animal_jetz)
vars.all <- colnames(abund.min40.lc[4:19])

clim_vars <- colnames(abund.min40.lc[4:11])
lc_vars <- colnames(abund.min40.lc[12:19])

adj_r2_lc <- tibble(bird = character(),
                     adj.r2 = numeric())

coefs_tib <- tibble(bird = character(),
                    variable = character(),
                    beta.coefs = numeric())

full_mods.lc <- list()
lc.mods <- list()
clim.mods <- list()

fullMod.warning.tib <- tibble(bird = character())
clim.warning.tib <- tibble(bird = character())
lc.warning.tib <- tibble(bird = character())

set.seed(123)

for(bird.ind in birds){
  
  print(paste0("working on ", which(birds == bird.ind), "/", length(birds), " ..."))
  
  bird.tmp <- abund.min40.lc %>%
    filter(animal_jetz == bird.ind) %>%
    tibble::rowid_to_column(., "ID")
  
  all.predictors <- vars.all
  
  lc.predictors <- lc_vars
  
  clim.predictors <- clim_vars
  
  response <- "delta.abund"
  
  all.bird_data <- bird.tmp[, c(all.predictors, response)]
  
  lc.bird_data <- bird.tmp[, c(lc.predictors, response)]
  
  clim.bird_data <- bird.tmp[, c(clim.predictors, response)]
  
  # train_control <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10)
  train_control <- caret::trainControl(method = "LOOCV", savePredictions = "final")
  
  tryCatch({
    full.lm_model <- caret::train(
      delta.abund~.,
      data = all.bird_data,
      method = "lmStepAIC",
      trControl = train_control,
      preProcess = c("center", "scale", "zv"),
      trace = F
    )
    
    adj.r2.full <- summary(full.lm_model$finalModel)$r.squared
    coefs.full <- coefficients(full.lm_model$finalModel)
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during full model training:", conditionMessage(w), "\n")
    # fullMod.entry <- tibble(bird = bird.ind)
    # fullMod.warning.tib <- bind_rows(fullMod.warning.tib, fullMod.entry)
  }
  )
  
  full_mods.lc[[bird.ind]] <- full.lm_model
  
  tryCatch({
    lc.lm_model <- caret::train(
      delta.abund~.,
      data = lc.bird_data,
      method = "lmStepAIC",
      trControl = train_control,
      preProcess = c("center", "scale", "zv"),
      trace = F
    )
    
    adj.r2.lc <- summary(lc.lm_model$finalModel)$r.squared
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during land cover model training:", conditionMessage(w), "\n")
    # lc.entry <- tibble(bird = bird.ind)
    # lc.warning.tib <- bind_rows(lc.warning.tib, lc.entry)
  }
  )
  
  lc.mods[[bird.ind]] <- lc.lm_model
  
  tryCatch({
    clim.lm_model <- caret::train(
      delta.abund~.,
      data = clim.bird_data,
      method = "lmStepAIC",
      trControl = train_control,
      preProcess = c("center", "scale", "zv"),
      trace = F
    )
    
    adj.r2.clim <- summary(clim.lm_model$finalModel)$r.squared
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during climate model training:", conditionMessage(w), "\n")
    # clim.entry <- tibble(bird = bird.ind)
    # clim.warning.tib <- bind_rows(clim.warning.tib, clim.entry)
  }
  )
  
  clim.mods[[bird.ind]] <- clim.lm_model
  
  new_row1 <- tibble(bird = bird.ind,
                     adj.r2 = adj.r2.full,
                     adj.r2_clim = adj.r2.clim,
                     adj.r2_lc = adj.r2.lc
                     )
  
  adj_r2_lc <- bind_rows(adj_r2_lc, new_row1)
  
  new_row2 <- tibble(bird = bird.ind,
                     variable = names(coefs.full),
                     beta.coefs = coefs.full)
  
  coefs_tib <- bind_rows(coefs_tib, new_row2)
}

rm(all.bird_data, bird.tmp, clim.bird_data, clim.lm_model, coefs.full,
   full.lm_model, lc.bird_data,
   lc.lm_model,
   new_row1, new_row2, train_control,
   adj.r2.clim, adj.r2.full, adj.r2.lc, all.predictors, bird.ind, birds, clim_vars,
   clim.predictors,
   lc_vars,
   lc.predictors,
   response,
   vars.all)

save(coefs_tib, file = "results/beta_coefficienst_LMs.rda")
save(adj_r2_lc, file = "results/r2s_LMs.rda")

# ---- species traits data ----

# load("results/r2s_LMs.rda")
# load("results/beta_coefficienst_LMs.rda")

adj_r2_lc <- adj_r2_lc %>%
  distinct()

coefs_tib <- coefs_tib %>%
  distinct()

load("data/species_traits.rda")

species.traits <- species.traits %>%
  rename(bird =  animal_jetz) %>%
  dplyr::select(-c(Common.Name, tot_diet_div, shannon, Clutch))

# traits <- colnames(species.traits[2:14])

adj_r2_lc_traits <- adj_r2_lc %>%
  # left_join(coefs_tib, by = "bird") %>%
  left_join(species.traits, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 13) %>%
  dplyr::select(-na.num) %>%
  distinct()


# ---- summary stats ----

adj_r2_lc_traits %>%
  select(bird, v.clim, v.joint, v.lc) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  reframe(
    variance.mean = mean(r2s),
  )

adj_r2_lc %>%
  select(bird, v.clim, v.joint, v.lc) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  reframe(
    variance.mean = mean(r2s),
  )

adj_r2_lc %>%
  select(bird, adj.r2, adj.r2_clim, adj.r2_lc) %>%
  pivot_longer(!bird, names_to = "var.type", values_to = "r2s") %>%
  group_by(var.type) %>%
  reframe(
    r2.mean = mean(r2s),
    r2.median = median(r2s),
    r2.sd = sd(r2s)
  )


coefs_tib %>%
  group_by(variable) %>%
  reframe(
    kept_pct = (n()/83)*100,
    beta.mean = mean(beta.coefs),
    beta.median = median(beta.coefs),
    beta.sd = sd(beta.coefs)
  ) %>%
  arrange(desc(kept_pct))

# ---- r2 plots ----

ttt <-
  adj_r2_lc %>%
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
           color = "grey40", alpha = 0.9, size = 0.2,
           width = 0.7) +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  ylab("") +
  xlab("adjusted R2") +
  theme(axis.text.y = element_text(size = 6)) +
  labs(fill = "Model type")

all_species_stacked

ggsave("figures/LM_Results/all_species_stacked.png", plot = all_species_stacked,
       width = 8, height = 6, dpi = 300)

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

# ---- stacked barplots categorical traits ----

mig.plot <- adj_r2_lc_traits %>%
  group_by(Migrant) %>%
  summarize(mean.v.clim = mean(v.clim),
         mean.v.lc = mean(v.lc),
         mean.v.joint = mean(v.joint),
         n_obs = n()) %>%
  pivot_longer(!c(Migrant, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = Migrant, y = variance, group = Migrant, fill = variable)) +
  geom_bar(position = "stack", stat = "identity",
           color = "grey30", alpha = 0.9, size = 0.3) +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

gen.plot <- adj_r2_lc_traits %>%
  mutate(gen.length.bins = cut(GenLength,
                               breaks = c(0, 2.5, 4, 12),
                               labels = c("short (<2.5y)", "medium (2.5-4y)", "long (>4y)"))) %>%
  group_by(gen.length.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(gen.length.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = gen.length.bins, y = R2, group = gen.length.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Generation Length") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

clutch.plot <- adj_r2_lc_traits %>%
  drop_na(Clutch.Bird) %>%
  mutate(clutch.size.bins = cut(Clutch.Bird,
                                breaks = c(0, 3, 5, 12),
                                labels = c("small (<=3)",
                                           "medium (3-5)",
                                           "large (>5)"))) %>%
  group_by(clutch.size.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(clutch.size.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = clutch.size.bins, y = R2, group = clutch.size.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Clutch size") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

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
  ggplot(aes(x = rel.brain.size.bins, y = variance, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Relative brain size") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

innov.plot <- adj_r2_lc_traits %>%
  drop_na(tot.innov) %>%
  mutate(tot.innov.bins = cut(tot.innov,
                                breaks = c(-1, 0, 3, 45),
                                labels = c("none", "few (< 4)", "many (4-42)"))) %>%
  group_by(tot.innov.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
            ) %>%
  pivot_longer(!c(tot.innov.bins, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = tot.innov.bins, y = variance, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Number of innovations") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

trophic.level.plot <- adj_r2_lc_traits %>%
  group_by(Trophic.Level) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(Trophic.Level, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = factor(Trophic.Level), y = variance,
             group = factor(Trophic.Level), fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Trophic Level") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type") +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

trophic.niche.plot <- adj_r2_lc_traits %>%
  group_by(Trophic.Niche) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
            ) %>%
  pivot_longer(!c(Trophic.Niche, n_obs), names_to = "variable", values_to = "variance") %>%
  ggplot(aes(x = Trophic.Niche, y = variance, group = Trophic.Niche, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Trophic Niche") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
    geom_text(aes(label = n_obs),
              stat = "count", vjust= -0.2, y = 0.01)

# ---- continuous traits

mass.plot <-adj_r2_lc_traits %>%
  mutate(body.mass.bins = cut(body.mass,
                               breaks = c(0, 30, 50, 2000),
                               labels = c("small", "medium", "large"))) %>%
  group_by(body.mass.bins) %>%
  summarize(mean.v.clim = mean(v.clim),
            mean.v.lc = mean(v.lc),
            mean.v.joint = mean(v.joint),
            n_obs = n()
  ) %>%
  pivot_longer(!c(body.mass.bins, n_obs), names_to = "variable", values_to = "R2") %>%
  ggplot(aes(x = body.mass.bins, y = R2, group = body.mass.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Body mass") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

wing.plot <-adj_r2_lc_traits %>%
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
  ggplot(aes(x = hand.wing.ind.bin, y = R2, group = hand.wing.ind.bin, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Hand wing index") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

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
  ggplot(aes(x = hab.breadth.bins, y = R2, group = hab.breadth.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Habitat breadth") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

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
  ggplot(aes(x = diet.breadth.bins, y = R2, group = diet.breadth.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Diet breadth") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

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
  ggplot(aes(x = ACAD.ind.bins, y = variance, group = ACAD.ind.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("ACAD population trend") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

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
  ggplot(aes(x = sauer.trend.bins, y = variance, group = sauer.trend.bins, fill = variable)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30",
           alpha = 0.8, size = 0.3) +
  xlab("Sauer population trend") +
  ylab("Mean R2") +
  scale_fill_manual(values=c("grey40", "grey70","grey100"),
                    labels = c("Climate", "Full", "Land cover")
                    ) +
  labs(fill = "Model type")  +
  geom_text(aes(label = n_obs),
            stat = "count", vjust= -0.2, y = 0.01)

stacked_final <- diet.plot + habitat.plot + trophic.niche.plot + trophic.level.plot +
  mass.plot + wing.plot + mig.plot + gen.plot + clutch.plot + brain.plot + innov.plot

stacked_final

ecological_plot <- diet.plot + habitat.plot + trophic.niche.plot + trophic.level.plot
ecological_plot

life_history_plot <- gen.plot + clutch.plot + brain.plot + innov.plot
life_history_plot

pop_stacked <- acad.plot + sauer.plot
pop_stacked

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


# ================ OLD 2 ----
# ---- var.imp plot -----

var.imp.ratio.lc <- ggplot(var.imp.lc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio,
                                           color = var.imp.ratio)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient(low = "royalblue", high = "red") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  theme(axis.text=element_text(size=7)) +
  xlab("Variable importance climate:land use") +
  ylab("")

var.imp.ratio.lc

ggsave(filename = "figures/var_imp_ratio_full_LM.png", plot = var.imp.ratio.lc,
       width = 8, height = 6, dpi = 300)


# ---- continuous traits plots ~ var.imp ----

p1 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Var. imp. ratio") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p2 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p3 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p4 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("Var. imp. ratio") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p5 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p6 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1, 0.1)

p7 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("Var. imp. ratio") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

final_plot

ggsave(filename = "figures/cont_traits_var.imp.ratio_full_LM.png", plot = final_plot,
       width = 8, height = 6, dpi = 300)


# ---- categorical traits ~ var.imp ----

p1 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  # geom_violin() +
  ylab("Var. imp. ratio") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p2 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  # geom_violin() +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p3 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Trophic Level") +
  ylab("Var. imp. ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p4 <- ggplot(adj_r2_lc_traits, aes(y = var.imp.ratio, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed")

boxplot <- p1 + p2 + p3 + p4

boxplot

ggsave(filename = "figures/cat_traits_var.imp.ratio_full_LM.png", plot = boxplot,
       width = 8, height = 6, dpi = 300)


# ---- Anova ----

anova.data <- adj_r2_lc_traits %>%
  na.omit() %>%
  select(-c(bird, adj.r2, adj.r2_clim, adj.r2_lc,
            var.imp.ratio, ACAD.ind, Trophic.Niche))

anova.lc <- aov(residuals ~ .,
                data = anova.data)
summary(anova.lc)

anova.lc <- aov(residuals ~ diet.breadth*hab.breath + Migrant*hand.wing.ind + 
                  rel_brain_size*tot.innov + GenLength*Clutch.Bird +.,
                data = anova.data)
summary(anova.lc)

anova.lc <- aov(residuals ~ diet.breadth*Trophic.Level + ., data = anova.data)
summary(anova.lc)

anova.lc <- aov(residuals ~ GenLength * Clutch.Bird + body.mass, data = anova.data)
summary(anova.lc)

anova.lc <- aov(residuals ~ hand.wing.ind * Migrant + body.mass, data = anova.data)
summary(anova.lc)

# ==== with PC1 and PC2 ----
# ---- LM function -----
birds <- unique(abund.min40.pc$animal_jetz)
vars.all <- colnames(abund.min40.pc[4:15])

clim_vars <- colnames(abund.min40.pc[4:11])
lc_vars <- colnames(abund.min40.pc[12:15])

adj_r2_pc <- tibble(bird = character(),
                     adj.r2 = numeric())

var.imp.pc <- tibble(bird = character(),
                      var.imp.ratio = numeric(),
                      variables = character(),
                      var.imps = numeric())

for(bird.ind in birds){
  
  print(paste0("working on ", which(birds == bird.ind), "/", length(birds), " ..."))
  
  bird.tmp <- abund.min40.pc %>%
    filter(animal_jetz == bird.ind) %>%
    tibble::rowid_to_column(., "ID")
  
  all.predictors <- vars.all
  lc.predictors <- lc_vars
  clim.predictors <- clim_vars
  response <- "delta.abund"
  
  all.bird_data <- bird.tmp[, c(all.predictors, response)]
  
  lc.bird_data <- bird.tmp[, c(lc.predictors, response)]
  
  clim.bird_data <- bird.tmp[, c(clim.predictors, response)]
  
  train_control <- caret::trainControl(method = "cv", number = 10)
  
  tryCatch({
    full.lm_model <- caret::train(
      delta.abund~.,
      data = all.bird_data,
      method = "lm",
      trControl = train_control
    )
    
    adj.r2.full <- summary(full.lm_model$finalModel)$r.squared
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during full model training:", conditionMessage(w), "\n")
  }
  )
  
  tryCatch({
    lc.lm_model <- caret::train(
      delta.abund~.,
      data = lc.bird_data,
      method = "lm",
      trControl = train_control
    )
    
    adj.r2.lc <- summary(lc.lm_model$finalModel)$r.squared
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during land cover model training:", conditionMessage(w), "\n")
  }
  )
  
  tryCatch({
    clim.lm_model <- caret::train(
      delta.abund~.,
      data = clim.bird_data,
      method = "lm",
      trControl = train_control
    )
    
    adj.r2.clim <- summary(clim.lm_model$finalModel)$r.squared
  }, warning = function(w){
    cat("Warning encountered for bird", bird.ind, "during climate model training:", conditionMessage(w), "\n")
  }
  )
  
  residuals.now <- adj.r2.full - (adj.r2.clim + adj.r2.lc)
  
  new_row1 <- tibble(bird = bird.ind,
                     adj.r2 = adj.r2.full,
                     adj.r2_clim = adj.r2.clim,
                     adj.r2_lc = adj.r2.lc,
                     residuals = residuals.now)
  
  adj_r2_pc <- bind_rows(adj_r2_pc, new_row1)
  
  
  varImp.full <- caret::varImp(full.lm_model)$importance
  
  var.imp.adj <- varImp.full/sum(varImp.full)
  
  clim.imp <- sum(var.imp.adj[1:8,])
  
  lc.imp <- sum(var.imp.adj[9:12,])
  
  var.imp.ratio <- clim.imp/lc.imp
  
  new_entry_var.imp <- tibble(bird = bird.ind,
                              var.imp.ratio = var.imp.ratio,
                              variables = rownames(var.imp.adj),
                              var.imps = var.imp.adj[,1])
  
  var.imp.pc <- bind_rows(var.imp.pc, new_entry_var.imp)
}

rm(all.bird_data, bird.tmp, clim.bird_data, clim.lm_model, full.lm_model, lc.bird_data,
   lc.lm_model, new_entry_var.imp, new_row1, train_control, var.imp.adj, varImp.full,
   adj.r2.clim, adj.r2.full, adj.r2.lc, all.predictors, bird.ind, birds, clim_vars, clim.imp, clim.predictors,
   lc_vars, lc.imp, lc.predictors, residuals.now, response, var.imp.ratio, vars.all)




# ---- species traits ----
load("data/species_traits.rda")

species.traits <- species.traits %>%
  rename(bird =  animal_jetz)  %>%
  select(-c(Common.Name, tot_diet_div, shannon, Clutch))

adj_r2_pc_traits <- adj_r2_pc %>% left_join(species.traits, by = "bird") %>%
  left_join(var.imp.pc, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 13) %>%
  select(-na.num) %>%
  select(-variables, -var.imps) %>%
  distinct()

# ---- continuous traits plots ----

p1 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Residuals") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

p2 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

p3 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

p4 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

p5 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

p6 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("Residuals")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1, 0.1)

p7 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-0.1,0.1)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

final_plot

ggsave(filename = "figures/Full_LM/PC1_PC2/cont_traits_residuals_PCA.png", plot = final_plot,
       width = 8, height = 6, dpi = 300)


# ---- categorical traits ----

p1_bp <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  # geom_violin() +
  ylab("Residuals") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p2_bp <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  # geom_violin() +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p3_bp <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Trophic Level") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-3,5)

p4_bp <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed")

boxplot <- p1_bp + p2_bp + p3_bp + p4_bp

boxplot

ggsave(filename = "figures/Full_LM/PC1_PC2/cat_traits_residuals_PCA.png", plot = boxplot,
       width = 8, height = 6, dpi = 300)

# ---- pop. trend plots ---- 

p1 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = ACAD.ind, group = ACAD.ind)) +
  geom_boxplot() +
  xlab("ACAD pop. trend") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

p2 <- ggplot(adj_r2_pc_traits, aes(y = residuals, x = sauer.trend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()  +
  # geom_abline() +
  xlab("Sauer's pop. trend") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

pop_trend <- p1 + p2

pop_trend

ggsave(filename = "figures/Full_LM/PC1_PC2/pop_trend_residuals_PCA.png", plot = pop_trend,
       width = 8, height = 6, dpi = 300)


# ---- var.imp plot -----

var.imp.PCA <- ggplot(var.imp.pc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio,
                       color = var.imp.ratio)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient(low = "royalblue", high = "red2") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  theme(axis.text=element_text(size=7)) +
  xlab("Variable importance climate:land use") +
  ylab("")

var.imp.PCA

ggsave(filename = "figures/Full_LM/PC1_PC2/var.imp.plot_PCA.png", plot = var.imp.PCA,
       width = 8, height = 6, dpi = 300)

# ---- continuous traits plots ~ var.imp ----

p1 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Var. imp. ratio") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p2 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p3 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p4 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("Var. imp. ratio") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p5 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

p6 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1, 0.1)

p7 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("Var. imp. ratio") +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-0.1,0.1)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

final_plot

ggsave(filename = "figures/Full_LM/PC1_PC2/cont_traits_var.imp_PCA.png", plot = final_plot,
       width = 8, height = 6, dpi = 300)


# ---- categorical traits ~ var.imp ----

p1 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  # geom_violin() +
  ylab("Var. imp. ratio") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p2 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  # geom_violin() +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p3 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Trophic Level") +
  ylab("Var. imp. ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed") #+
# ylim(-3,5)

p4 <- ggplot(adj_r2_pc_traits, aes(y = var.imp.ratio, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 1, linetype = "dashed")

boxplot <- p1 + p2 + p3 + p4

boxplot

ggsave(filename = "figures/Full_LM/PC1_PC2/cat_traits_var.imp_PCA.png", plot = boxplot,
       width = 8, height = 6, dpi = 300)


# ---- Anova ----
# levene test:
# hand-wing index and sauer population trend differ variance between groups
# exclude from anova

# residuals are normal distributed

# data in traits is not normally distributed....

anova.data <- adj_r2_pc_traits %>%
  na.omit() %>%
  select(-c(bird, adj.r2, adj.r2_clim, adj.r2_lc,
            var.imp.ratio, ACAD.ind, Trophic.Niche,
            hand.wing.ind, sauer.trend))

anova.pc <- aov(residuals ~ ., data = anova.data)
summary(anova.pc)

anova.pc <- aov(residuals ~ Trophic.Level*diet.breadth, data = anova.data)
summary(anova.pc)

anova.pc <- aov(residuals ~ diet.breadth*hab.breadth + 
                  rel_brain_size*tot.innov + GenLength*Clutch.Bird +.,
                data = anova.data)

summary(anova.pc)

# ========== Old 1----
# ---- LM function ----
# formula_string <- paste("delta.abund ~", paste(vars, collapse = " + "))
# formula_string <- as.formula(paste(formula_string, "+", paste("I(", vars, "^2)", collapse = " + ")))
# lm_formula <- formula(formula_string)

fit_lm <- function(df, vars) {
  formula_string <- paste("delta.abund ~", paste(vars, collapse = " + "))
  formula <- as.formula(formula_string)
  # formula <- as.formula(paste(formula_string, "+", paste("I(", vars, "^2)", collapse = " + ")))
  model <- lm(formula, data = df)
  return(model)
}

num_folds <- 10

LM_list <- list()

rmse_tib <- tibble(bird = character(),
                   mean_rmse = numeric(),
                   n.folds = numeric())

adj_r2_tib <- tibble(bird = character(),
                     adj.r2 = numeric())

for(bird.ind in birds){
  
  print(paste0("working on ", which(birds == bird.ind), "/", length(birds), " ..."))
  
  bird.tmp <- abund.min40 %>%
    filter(animal_jetz == bird.ind) %>%
    tibble::rowid_to_column(., "ID")
  
  full.mod <- fit_lm(bird.tmp, vars = vars.all)
  adj.r2.now <- summary(full.mod)$adj.r.squared
  
  lc.mod <- fit_lm(bird.tmp, vars = lc_vars)
  adj.r2.lc <- summary(lc.mod)$adj.r.squared
  
  clim.mod <- fit_lm(bird.tmp, vars = clim_vars)
  adj.r2.clim <- summary(clim.mod)$adj.r.squared
  
  residuals.now <- adj.r2.now - (adj.r2.clim + adj.r2.lc)
  
  new_row1 <- tibble(bird = bird.ind,
                     adj.r2 = adj.r2.now,
                     adj.r2_clim = adj.r2.clim,
                     adj.r2_lc = adj.r2.lc,
                     residuals = residuals.now)
  adj_r2_tib <- bind_rows(adj_r2_tib, new_row1)
  
  LM_list[[bird.ind]] <- full.mod
  
  folds <- caret::groupKFold(bird.tmp$segment, k = 10)
  
  rmse_values <- numeric(num_folds)
  
  for(n_fold in seq_along(folds)){
    
    bird.train <- bird.tmp %>%
      filter(ID %in% folds[[n_fold]])
    
    bird.test <- bird.tmp %>%
      filter(!(ID %in% folds[[n_fold]]))
    
    bird_model <- fit_lm(bird.train, vars = vars.all)
    
    predicted_values <- predict(bird_model, newdata = bird.test)
    
    rmse <- sqrt(mean((bird.train$delta.abund - predicted_values)^2))
    
    rmse_values[n_fold] <- rmse
  }
  
  mean_rmse <- mean(rmse_values)
  
  
  
  new_row2 <- tibble(bird = bird.ind, mean_rmse = mean_rmse,
                     n.folds = length(folds))
  
  
  rmse_tib <- bind_rows(rmse_tib, new_row2)
}

summary(LM_list[[1]])
# ---- prepare the bioclim data ---- 

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median"),
            pr.sum.mean, cmi.annual.mean, cmi.diff.mean),
         contains("mean")); rm(climate_df)

hfp.df <- hfp.full %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- hfp.df %>%
  left_join(land_use_area, by = c("segment", "year")) %>%
  select(-c(ecoregion, tot.area.m2, route)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"), contains("hfp")); rm(land_use_area, hfp.df)

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")); rm(climate.df, land.use.df)

dbioclim <- bioclim.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  reframe(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), lag(.) - ., NA),
      .names = "delta.{col}"
    )
  ); rm(bioclim.df)

dclim <- dbioclim %>% select(segment, contains("mean"), -contains("hfp")) %>% na.omit()

dland.use <- dbioclim %>% select(segment, contains("log"), contains("hfp")) %>% na.omit()

# ---- dabund + climate datasets -----

d.abundmin6.clim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin10.clim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

d.abundmin40.clim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dclim, by = "segment") %>%
  na.omit()

rm(dclim)

# ---- d.abund + land use datasets ----

d.abundmin6.land.use <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin10.land.use <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

d.abundmin40.land.use <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dland.use, by = "segment") %>%
  na.omit()

rm(dland.use)

# ---- d.abund full datasets ----

d.abundmin6.bioclim <- d.abund.min6 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin10.bioclim <- d.abund.min10 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

d.abundmin40.bioclim <- d.abund.min40 %>%
  select(-c(abund.mean, abund.var, abund.median, abund.geom.mean, year)) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

rm(dbioclim)
rm(d.abund.min10, d.abund.min40, d.abund.min6)
