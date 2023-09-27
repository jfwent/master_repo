# Land use variable PCA
# Author: Jon Went
# Date: 22.09.2023

# ---- libraries ----
library(tidyverse)
library(ade4)
library(factoextra)
# library(corrplot)

# ---- load data ----

load("data/Land_use/land_use_area_t1_t2.rda")

load("data/hfp_t1_t2.rda")

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
  select(year, segment, contains("log"), contains("hfp")) %>%
  na.omit() %>%
  tibble::rowid_to_column("ID")

rm(land_use_area, hfp.df)

# ----- PCA ----

lc.pca <- dudi.pca(land.use.df[,4:12],
                   scannf = F,
                   nf = 9)

var <- get_pca_var(lc.pca)
var$cos2

PC1 <- lc.pca$li$Axis1
PC2 <- lc.pca$li$Axis2

lc.pcs <- tibble(segment = land.use.df$segment,
                 year = land.use.df$year,
                 PC1 = PC1,
                 PC2 = PC2) %>%
  group_by(segment) %>%
  arrange(segment) %>%
  mutate(delta.PC1 = PC1 - lag(PC1),
         delta.PC2 = PC2 - lag(PC2)) %>%
  na.omit()

# corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(lc.pca, choice = "var", axes = 1:2)
corrplot::corrplot(var$contrib, is.corr=FALSE)

# ---- plot results ----

var_contrib_PC1 <- fviz_contrib(lc.pca, choice = "var", axes = 1, top = 10)
var_contrib_PC2 <- fviz_contrib(lc.pca, choice = "var", axes = 2)
fviz_contrib(lc.pca, choice = "var", axes = 1:2, top = 10)


fviz_pca_ind(lc.pca, geom = "point", col.ind = "cos2", alpha.ind = "contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.05)

fviz_pca_ind(lc.pca, geom = "point", col.ind = "contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.06)

fviz_eig(lc.pca)

fviz_pca_var(lc.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


biplot <- fviz_pca_biplot(lc.pca, label ="var",
                          col.ind="cos2", alpha.ind = "contrib") +
  scale_color_gradient2(low="yellow", mid="#E7B800",
                        high="#FC4E07", midpoint=0.35) +
  theme_bw()

biplot

# ggsave("figures/PCA_LandUse_biplot.png", plot = biplot, width = 8, height = 6, dpi = 300)
# ggsave("figures/var_contrib_PC1.png", plot = var_contrib_PC1, width = 8, height = 6, dpi = 300)
# ggsave("figures/var_contrib_PC2.png", plot = var_contrib_PC2, width = 8, height = 6, dpi = 300)


# ----- old -----

# lc.trans <- land.use.df %>% select(-c(year, segment, ID)) %>% t()

# lc.trans.pca <- dudi.pca(lc.trans,
#                    scannf = F,
#                    nf = 5)

# lc.trans.ind <- fviz_pca_ind(lc.trans.pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# loadings_tib <- tibble(variable = rownames(lc.trans.pca$li),
#                        PC1 = lc.trans.pca$li[,1],
#                        PC2 = lc.trans.pca$li[,2]) %>%
#   arrange(desc(PC1)) %>%
#   mutate(
#     top_variables_PC1 = if_else(row_number() <= 3, variable, "")
#   ) %>%
#   arrange(desc(PC2)) %>%
#   mutate(
#     top_variables_PC2 = if_else(row_number() <= 3, variable, "")
#   )
