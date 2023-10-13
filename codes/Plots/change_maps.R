# Change illustrations
# date: 13.10.2023

# --- libs ----

library(xtable)
library(stargazer)
library(tidyverse)

# --- data ----

# run the 01_start file for species data
load("data/Climate/climate_df.rda")


# ---- spatial data -----

seg_final <- unique(abund.min40.lc$segment)

bbs.routes.segmented.final <- sf::st_read("/Users/jonwent/Downloads/5_segments_final.shp")

epsg_5070 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "

bbs.routes <- bbs.routes.segmented.final %>%
  separate(partition, into = c("statenum", "route", "segnum"), sep = "_", remove = F) %>%
  na.omit() %>%
  rename(segment = partition) %>%
  separate(segment, sep = "_", into = c("state", "route", "seg"), remove = F) %>%
  unite(state, route,col= "U_S_R_I") %>%
  filter(
    U_S_R_I %in% seg_tibble$U_S_R_I
    # state %in% seg_tibble$state,
    # route %in% seg_tibble$route
  ) %>%
  sf::st_transform(crs = epsg_5070)

bbs.routes.buffered <- bbs.routes %>% sf::st_buffer(500)

usa_state <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- sf::st_as_sf(maps::map("usa", fill=TRUE, plot =FALSE))
usa_5070 <- sf::st_transform(usa, crs = epsg_5070)

rm(bbs.routes, bbs.routes.segmented.final)

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

bbs.clim <- bbs.routes.buffered %>% left_join(dclim, by = "segment")

rm(climate.df, climate_df)
# ---- 

# usa_map_tmax

ggplot() +
  geom_sf(data = usa_5070,
          fill = "white", color = "grey20", linewidth = 0.3, alpha = 0
  ) +
  geom_sf(data = bbs.clim,
          aes(
            fill = segment,
            color = delta.tmax.mean
          ),
          # linewidth = 1
  ) +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(option = "D")