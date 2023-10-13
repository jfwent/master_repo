# Change illustrations
# date: 13.10.2023

# --- libs ----

library(xtable)
library(stargazer)
library(tidyverse)
library(rgdal)
library(patchwork)

# --- data ----

# run the 01_start file for species data
load("data/Climate/climate_df.rda")
load("data/Land_use/land_use_area_t1_t2.rda")
load("data/species_traits.rda")

# ---- spatial data -----

seg_final <- unique(abund.min40.lc$segment)

bbs.routes.segmented.final <- sf::st_read("/Users/jonwent/Downloads/5_segments_final.shp")

epsg_5070 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "

seg_tibble <- tibble(segs_fin = seg_final) %>%
  separate(segs_fin, sep = "_", into = c("state", "route", "seg"), remove = F) %>%
  unite(state, route,col= "U_S_R_I")

bbs.routes <- bbs.routes.segmented.final %>%
  separate(partition, into = c("statenum", "route", "segnum"), sep = "_", remove = F) %>%
  na.omit() %>%
  rename(segment = partition) %>%
  separate(segment, sep = "_", into = c("state", "route", "seg"), remove = F) %>%
  unite(state, route,col= "U_S_R_I") %>%
  filter(
    U_S_R_I %in% seg_tibble$U_S_R_I
  ) %>%
  sf::st_transform(crs = epsg_5070)

bbs.routes.buffered <- bbs.routes %>% sf::st_buffer(500)

usa_state <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- sf::st_as_sf(maps::map("usa", fill=TRUE, plot =FALSE))
usa_5070 <- sf::st_transform(usa, crs = epsg_5070)

rm(bbs.routes, bbs.routes.segmented.final, seg_tibble)

# ---- climate change ----

climate.df <- climate_df %>%
  select(-c(contains("var"), contains("median")),
         contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, pr.diff.mean))

dclim <- climate.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit() %>% 
  filter(segment %in% abund.min40.lc$segment)

# summary(dclim)

bbs.clim <- bbs.routes.buffered %>% left_join(dclim, by = "segment") %>% na.omit()

rm(climate.df, climate_df)

# ---- land cover change ----

lc.df <- land_use_area %>%
  select(-c(ecoregion, tot.area.m2, route, barren.area.m2, wet.area.m2)) %>%
  mutate(urban.area.m2 = urban.high.area.m2 + urban.low.area.m2,
         # all.grass.area.m2 = grass.area.m2 + pasture.area.m2
  ) %>%
  select(-c(urban.high.area.m2, urban.low.area.m2,
            # grass.area.m2,
            # pasture.area.m2,
            crop.area.m2
  )) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"))

dlc <- lc.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(
    across(
      .cols = matches('area') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), . - lag(.), NA),
      .names = "delta.{col}"
    )
  ) %>%
  select(segment, contains("delta")) %>%
  na.omit()

bbs.lc <- bbs.routes.buffered %>% left_join(dlc, by = "segment") %>% na.omit()

rm(land_use_area, lc.df)


# --- stats and tables ----

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
  pivot_longer(cols = 1:8, names_to = "var", values_to = "values")

dabund_ridges <-
  abund.min40.lc %>%
  select(animal_jetz, segment, delta.abund) %>%
  group_by(animal_jetz) %>%
  mutate(
    mean = mean(delta.abund),
    median = median(delta.abund),
    sd = sd(delta.abund)
  ) %>%
  ggplot(
    aes(y = reorder(animal_jetz, delta.abund),
        x = delta.abund,
        # color = reorder(animal_jetz, delta.abund),
        fill = reorder(animal_jetz, delta.abund),
        # fill = after_stat(delta.abund)
        )
        # group = as.factor(delta.abund)
        ) +
  # ggridges::geom_density_ridges(
  #   rel_min_height = 0.001,
  #   # stat = "identity",
  #   # scale = 1
  #   ) +
  ggridges::geom_density_ridges_gradient(
    rel_min_height = 0.001,
    bandwidth = 1,
    scale = 3
    ) +
  scale_fill_viridis_d(option = "magma", guide = "none", alpha = 1) +
  # viridis::scale_colour_viridis(discrete = T) +
  xlim(-80, 80) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
  theme_bw() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text=element_text(size=7)) +
  ylab("") +
  xlab("delta Abundance")

ggsave(dabund_ridges, file = "figures/D.abund.ridges.png", width = 8, height = 6, dpi = 300)

abund.min40.lc %>%
  select(animal_jetz, segment, delta.abund) %>%
  ungroup() %>%
  summarize(
    mean = mean(delta.abund),
    median = median(delta.abund),
    sd = sd(delta.abund)
  )

median.sauer.plot <- abund.min40.lc %>%
  summarize(
    median.abund = median(delta.abund)
  ) %>%
  left_join(species.traits, by = "animal_jetz") %>%
  select(animal_jetz, median.abund, sauer.trend, ACAD.ind) %>%
  ggplot(aes(
    x = sauer.trend,
    y = median.abund),
    fill = animal_jetz) +
  geom_point(alpha = 0.8) +
  # scale_color_viridis_c(option = "magma") +
  geom_smooth(method = "lm", color = "black")+
  theme_bw() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text=element_text(size=7)) +
  ylab("Median delta abundance") +
  xlab("Population trend (Sauer)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  ylim(-4, 1)
  
median.boxplot <- abund.min40.lc %>%
  summarize(
    median.abund = median(delta.abund)
  ) %>%
  left_join(species.traits, by = "animal_jetz") %>%
  drop_na(ACAD.ind) %>%
  select(animal_jetz, median.abund, sauer.trend, ACAD.ind) %>%
  ggplot(aes(
    # x = reorder(ACAD.ind, median.abund),
    x = ACAD.ind,
    y = median.abund,
    group = ACAD.ind
    )) +
  geom_boxplot() +
  # scale_color_viridis_c(option = "magma") +
  # geom_smooth(method = "lm", color = "black")+
  theme_bw() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text=element_text(size=7)) +
  geom_hline(yintercept = 0, linetype ="dashed", color = "grey30") +
  ylab("") +
  xlab("Population trend (ACAD)") +
  ylim(-4, 1)

median.boxplot.sauer.acad <- abund.min40.lc %>%
  summarize(
    median.abund = median(delta.abund)
  ) %>%
  left_join(species.traits, by = "animal_jetz") %>%
  drop_na(ACAD.ind) %>%
  select(animal_jetz, median.abund, sauer.trend, ACAD.ind) %>%
  ggplot(aes(
    # x = reorder(ACAD.ind, median.abund),
    x = ACAD.ind,
    y = sauer.trend,
    group = ACAD.ind
  )) +
  geom_boxplot() +
  # scale_color_viridis_c(option = "magma") +
  # geom_smooth(method = "lm", color = "black")+
  theme_bw() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text=element_text(size=7)) +
  geom_hline(yintercept = 0, linetype ="dashed", color = "grey30") +
  ylab("Population trend (Sauer)") +
  xlab("Population trend (ACAD)")  +
  ylim(-4, 1)

median.abund.valid <- median.sauer.plot + median.boxplot + median.boxplot.sauer.acad

median.abund.valid

ggsave(median.abund.valid, file = "figures/abund.change.valid.png", width = 8, height = 6, dpi = 300)

table.dat <- abund.min40.lc %>%
  select(animal_jetz, segment, delta.abund) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  summarize(
    mean = mean(delta.abund),
    median = median(delta.abund),
    sd = sd(delta.abund),
    n_obs = n()
  )

xtable(table.dat, caption = "Delta Abundances")

# ---- climate change maps ----

usa_map_tmax <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.clim,
          aes(
            # fill = delta.tmax.mean,
            color = delta.tmax.mean
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma")+
  labs("delta Tmax")

ggsave(usa_map_tmax, file = "figures/usa_map_tmax.png", width = 8, height = 6, dpi = 300)

usa_map_cmi <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.clim,
          aes(
            # fill = delta.cmi.diff.mean,
            color = delta.cmi.diff.mean
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma") +
  labs("delta CMI")

ggsave(usa_map_cmi, file = "figures/usa_map_cmi.png", width = 8, height = 6, dpi = 300)

usa_map_tmin <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.clim,
          aes(
            fill = delta.cmi.diff.mean,
            color = delta.tmin.mean
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_color_viridis_c(option = "magma",
                         # guide = guide_legend(title = "delta Tmin")
                         )

ggsave(usa_map_tmin, file = "figures/usa_map_tmin.png", width = 8, height = 6, dpi = 300)

usa_map_swb <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.clim,
          aes(
            fill = delta.cmi.diff.mean,
            color = delta.swb.mean
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma",
                         # guide = guide_legend(title = "delta SWB")
                         )

ggsave(usa_map_swb, file = "figures/usa_map_swb.png", width = 8, height = 6, dpi = 300)


# ---- land cover change maps ----

usa_map_forest <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.lc,
          aes(
            # fill = delta.tmax.mean,
            color = delta.forest.area.m2.log
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma")+
  labs("delta Forest")

ggsave(usa_map_forest, file = "figures/usa_map_forest.png", width = 8, height = 6, dpi = 300)

usa_map_urban <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.lc,
          aes(
            # fill = delta.tmax.mean,
            color = delta.urban.area.m2.log
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma")+
  labs("delta Urban")

ggsave(usa_map_urban, file = "figures/usa_map_urban.png", width = 8, height = 6, dpi = 300)

usa_map_grass <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.lc,
          aes(
            # fill = delta.tmax.mean,
            color = delta.grass.area.m2.log
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma")+
  labs("delta Grass")

ggsave(usa_map_grass, file = "figures/usa_map_grass.png", width = 8, height = 6, dpi = 300)

usa_map_pasture <- ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0.8
  ) +
  geom_sf(data = bbs.lc,
          aes(
            # fill = delta.tmax.mean,
            color = delta.pasture.area.m2.log
          ),
          # linewidth = 1,
          alpha = 0.7
  ) +
  guides(fill="none") +
  theme_bw() +
  scale_colour_viridis_c(option = "magma")+
  labs("delta Pasture")

ggsave(usa_map_pasture, file = "figures/usa_map_pasture.png", width = 8, height = 6, dpi = 300)
