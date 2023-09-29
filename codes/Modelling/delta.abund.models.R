# Build Delta-Abundance Model
# Author: Jon Went
# Date: 14.09.2023

# ---- library ----
library(tidyverse)
library(patchwork)
library(ggplot2)
install.packages("aov")

# ==== with land cover vars -----
# ---- LM function -----

birds <- unique(abund.min40.lc$animal_jetz)
vars.all <- colnames(abund.min40.lc[4:19])

clim_vars <- colnames(abund.min40.lc[4:11])
lc_vars <- colnames(abund.min40.lc[12:19])

adj_r2_lc <- tibble(bird = character(),
                     adj.r2 = numeric())

var.imp.lc <- tibble(bird = character(),
                      var.imp.ratio = numeric(),
                      variables = character(),
                      var.imps = numeric())

full_mods.lc <- list()

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
  
  adj_r2_lc <- bind_rows(adj_r2_lc, new_row1)
  
  full_mods.lc[[bird.ind]] <- full.lm_model
  
  varImp.full <- caret::varImp(full.lm_model)$importance
  
  var.imp.adj <- varImp.full/sum(varImp.full)
  
  clim.imp <- sum(var.imp.adj[1:8,])
  
  lc.imp <- sum(var.imp.adj[9:12,])
  
  var.imp.ratio <- clim.imp/lc.imp
  
  new_entry_var.imp <- tibble(bird = bird.ind,
                              var.imp.ratio = var.imp.ratio,
                              variables = rownames(var.imp.adj),
                              var.imps = var.imp.adj[,1])
  
  var.imp.lc <- bind_rows(var.imp.lc, new_entry_var.imp)
}

rm(all.bird_data, bird.tmp, clim.bird_data, clim.lm_model, full.lm_model, lc.bird_data,
   lc.lm_model, new_entry_var.imp, new_row1, train_control, var.imp.adj, varImp.full,
   adj.r2.clim, adj.r2.full, adj.r2.lc, all.predictors, bird.ind, birds, clim_vars, clim.imp, clim.predictors,
   lc_vars, lc.imp, lc.predictors, residuals.now, response, var.imp.ratio, vars.all)

# wrning_birds <- c("Catharus_guttatus", "Catharus_ustulatus",
#                   "Dendroica_coronata", "Dendroica_virens", "Junco_hyemalis",
#                   "Myiarchus_cinerascens", "Piranga_ludoviciana", "Zonotrichia_albicollis")
# 
# t <- abund.min40.lc %>%
#   filter(animal_jetz %in% wrning_birds) %>%
#   group_by(animal_jetz) %>%
#   mutate(n_entries = n())
# 
# unique(t$n_entries)

# ---- species traits data ----

load("data/species_traits.rda")

species.traits <- species.traits %>%
  rename(bird =  animal_jetz) %>%
  select(-c(Common.Name, tot_diet_div, shannon, Clutch))

traits <- colnames(species.traits[2:14])

adj_r2_lc_traits <- adj_r2_lc %>% left_join(species.traits, by = "bird") %>%
  left_join(var.imp.lc, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 17) %>%
  select(-na.num) %>%
  select(-variables, -var.imps) %>%
  distinct()

# ---- continuous traits plots ----

p1 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = log(GenLength))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  ylab("Residuals") +
  xlab("log(Generation length)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

p2 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = Clutch.Bird)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Clutch size") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

p3 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = diet.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Diet breadth") +
  ylab("")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

p4 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = hab.breadth)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("Habitat breadth") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

p5 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = log(body.mass))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()+
  # geom_abline() +
  xlab("log(Body mass)") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

p6 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = hand.wing.ind)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  # geom_abline() +
  xlab("Hand-wing index") +
  ylab("Residuals")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1, 0.1)

p7 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = rel_brain_size)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth() +
  xlab("Relative brain size") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-0.1,0.1)

final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

final_plot

# ---- categorical traits ----

p1_bp <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = log(tot.innov), group = tot.innov)) +
  geom_boxplot() +
  # geom_violin() +
  ylab("Residuals") +
  xlab("log(Innovativeness)")  +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p2_bp <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = Trophic.Niche, group = Trophic.Niche)) +
  geom_boxplot()  +
  # geom_violin() +
  xlab("Trophic Niche") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p3_bp <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = Trophic.Level, group = Trophic.Level)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Trophic Level") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") #+
  # ylim(-3,5)

p4_bp <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = Migrant, group = Migrant)) +
  geom_boxplot() +
  # geom_violin() +
  xlab("Migratory status") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed")

boxplot <- p1_bp + p2_bp + p3_bp + p4_bp

boxplot

# ---- pop. trend plots ---- 

p1 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = ACAD.ind, group = ACAD.ind)) +
  geom_boxplot() +
  xlab("ACAD pop. trend") +
  ylab("Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

p2 <- ggplot(adj_r2_lc_traits, aes(y = residuals, x = sauer.trend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth()  +
  # geom_abline() +
  xlab("Sauer's pop. trend") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed") #+
# ylim(-5,3)

pop_trend <- p1 + p2

pop_trend
# ---- var.imp plots -----

ggplot(var.imp.pc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio)) +
  geom_point() +
  geom_vline(xintercept = 1)

ggplot(var.imp.lc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio)) +
  geom_point() +
  geom_vline(xintercept = 1)


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

# ---- Anova ----



anova.data <- adj_r2_lc_traits %>%
  na.omit() %>%
  select(-c(bird, adj.r2, adj.r2_clim, adj.r2_lc,
            Common.Name, tot_diet_div, shannon, Clutch))

anova.lc <- aov(residuals ~ GenLength, data = anova.data)

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

species.traits <- species.traits %>% rename(bird =  animal_jetz)

adj_r2_pc_traits <- adj_r2_pc %>% left_join(species.traits, by = "bird") %>%
  left_join(var.imp.pc, by = "bird") %>%
  mutate(na.num = rowSums(is.na(.))) %>%
  filter(na.num != 17) %>%
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


# ---- var.imp plots -----

ggplot(var.imp.pc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio)) +
  geom_point() +
  geom_vline(xintercept = 1)

ggplot(var.imp.lc, aes(y = reorder(bird, var.imp.ratio), x = var.imp.ratio)) +
  geom_point() +
  geom_vline(xintercept = 1)


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

# ---- Anova ----

lm.mig.pc <- lm(residuals ~ var.imp.ratio, data = adj_r2_pc_traits)
anova.mig.pc <- anova(lm.mig.pc)

# ========== Old ----
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
