## Land cover plots together with bird abundance/biodiversity indeces

#We discussed plots where the change of land cover is visualized across the time span
# simultaneous with bird abundances (or biodiversity indices)
# to visually detect a dependence of land cover change with bird abundance


###======= load data ============
# Load the data
rm(list=ls())
library(dplyr); library(tidyr); library(purrr)

load("data/Lica/BBS_land_years.rda")
load("data/Lica/biodiv_indices.rda")
load("data/Lica/BBS_partition_abundance_2021.rda")
load("data/Lica/BBS_partition_abundance.rda")
load("/Users/jonwent/Downloads/sp_mat_list.rda")

abund2021 <- bbs2021; rm(bbs2021)
abund <- BBS_partition_abundance; rm(BBS_partition_abundance)
abund.all <- bind_rows(abund, abund2021); rm(abund2021, abund)

ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

land <- BBS_land_years; rm(BBS_land_years)
biodiv <- biodiv.df; rm(biodiv.df)

#==========data preparation=====
land <- land %>%
  rowwise() %>%
  mutate(urban = sum(urban.low, urban.high)) %>%
  select(-urban.low, -urban.high, -route) %>%
  arrange(partition)

land_ID <- unite(land, ID, year, partition, sep = "-")
biodiv_ID <- unite(biodiv, ID, year, segment, sep = "-")
abund_ID <- unite(abund.all, ID, year, partition, sep = "-")
full_df <- inner_join(land_ID, biodiv_ID, by = "ID")
full_df <- separate(full_df, ID, sep = "-", into = c("year", "partition"))
full_df <- inner_join(full_df, ecoreg, by = "partition")
full_df <- full_df %>% select(-FID, -Kilometers)

birds <- unique(abund.all$animal_jetz)
years.all <- unique(abund.all$year)
segments <- unique(full_df$partition)
years <- unique(full_df$year)

#=============== check for biggest changes========

for(i in seq_along(segments)){
  
  tmp <- full_df %>%
    filter(partition == segments[i])
  
  df_name <- paste0("diff_df")
  
  if(exists(df_name)){
    
    tmp.now <- tmp %>%
      select(-partition, -Ecoregion, -year)
    
    # d_tmp <- c(NA, diff(tmp.now))
    diff_tmp <- as.data.frame(apply(tmp.now, 2, diff))
    diff_tmp$year <- tmp$year[2:length(tmp$year)]
    diff_tmp$partition <- segments[i]
    diff_tmp$Ecoregion <- tmp$Ecoregion[2:length(tmp$Ecoregion)]
  
    diff_tmp <- relocate(diff_tmp, year, partition, .before = forest) 
    
    diff_df <- rbind(diff_df, diff_tmp)
    
  }else{
    
    tmp.now <- tmp %>%
      select(-partition, -Ecoregion, -year)
    
    # d_tmp <- c(NA, diff(tmp.now))
    diff_tmp <- as.data.frame(apply(tmp.now, 2, diff))
    diff_tmp$year <- tmp$year[2:length(tmp$year)]
    diff_tmp$partition <- segments[i]
    diff_tmp$Ecoregion <- tmp$Ecoregion[2:length(tmp$Ecoregion)]
    
    diff_tmp <- relocate(diff_tmp, year, partition, .before = forest) 
    
    diff_df <- diff_tmp
  }
}

rm(diff_tmp, tmp, tmp.now, i)

diff_yrs <- diff_df %>%
  group_by(year) %>%
  summarize(sum_forest = sum(forest),
            mean_forest = mean(forest),
            sd_forest = sd(forest),
            sum_urban = sum(urban),
            mean_urban = mean(urban),
            sd_urban = sd(urban),
            sum_crop = sum(crop),
            mean_crop = mean(crop),
            sd_crop = sd(crop),
            sum_grass = sum(grass),
            mean_grass = mean(grass),
            sd_grass = mean(grass),
            sum_pasture = sum(pasture),
            mean_pasture = mean(pasture),
            sd_pasture = sd(pasture),
            sum_wet = sum(wet),
            mean_wet = mean(wet),
            sd_wet = sd(wet)
            )

for(i in years){
  
  tmp <- full_df %>%
    filter(year == years[1])
  
  tmp.mat <- data.matrix(tmp[,3:9])
  
  heatmap <- heatmap(tmp.mat, scale="column")
  
}

#=========plot land cover difference as map and with overlaying sp diversity indices =======
library(rgdal);library(mapview); library(leaflet); library(RColorBrewer); library(leaflet.extras2)

base_color <- "#FF0000"
pal <- colorRampPalette(c(base_color, base_color))(length(unique(full_df$Ecoregion)))

routes <- readOGR("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp")
ecoreg_shp <- readOGR("/Users/jonwent/Desktop/ETHZ/master_thesis/Data/us_eco_l3.shp")

LC_map_list <- list()
dLC_map_list <- list()

for(i in seq_along(unique(full_df$year))){
  
  mapname1 <- paste0("LC_", years[i])
  mapname2 <- paste0("dLC_", years[i])
  
  LC.now <- full_df %>%
    filter(year == years[i])
  
  dLC.now <- diff_df %>%
    filter(year == years[i])
  
  map.now <- merge(routes, LC.now, by="partition")
  dmap.now <- merge(routes, dLC.now, by="partition")
  
  dLC_map_list[[mapname2]] <- dmap.now
  LC_map_list[[mapname1]] <- map.now
}

rm(map.now, dmap.now, dLC.now, i)



LC_map_forest_rich <- mapview(LC_map_list, lwd = "shannon", alpha = 0.5, zcol = "forest", legend=F)

LC_map_forest_rich

(LC_map_forest_rich | LC_map_forest_rich)


mapview(LC_map_list, lwd = "forest", #map.types = "leaflet", layer.name = "year",
        alpha = 0.5, zcol = "simpson", legend=F)
mapview(dLC_map_list, lwd = "richness", alpha = 0.5, zcol = "forest", legend=F)

sldf.now <- LC_map_list[[1]]

sldf_list <- list()

for(col_name in sldf.now@data[-1]){
  
  sldf <- SpatialLinesDataFrame(sldf.now@lines,
                                data = data.frame(ID = sldf.now$partition, 
                                                  Value = sldf.now@data[[col_name]]))
  attr(sldf, "mapview.layer.name") <- col_name
  
  sldf_list[[col_name]] <- sldf
  
}

mapview(dLC_map_list)

#=============plot Evolution of LC & sp diversity indices==========
#

library(plotly)
ecoregs <- unique(full_df$Ecoregion)

Ecoreg_plots <- list()

for(i in seq_along(ecoregs)){
  full_df_years <- full_df %>%
    filter(Ecoregion == ecoregs[i]) %>%
    group_by(year) %>%
    summarise(richness_mean = mean(richness), forest_mean = mean(forest),
              grass_mean = mean(grass), pasture_mean = mean(pasture), crop_mean = mean(crop),
              wet_mean = mean(wet), barren_mean = mean(barren),
              urban_mean = mean(urban), shannon_mean = mean(shannon),
              simpson_mean = mean(simpson)
    )
  
  fit_rich <- fitted(loess(full_df_years$richness_mean ~ full_df_years$year))
  fit_shannon <- fitted(loess(full_df_years$shannon_mean ~ full_df_years$year))
  fit_simpson <- fitted(loess(full_df_years$simpson_mean ~ full_df_years$year))
  fig1 <- plot_ly(data = full_df_years, x = ~year, y = ~urban_mean,
                  type = "bar", name= 'Urban')
  fig1 <- fig1 %>% add_trace(y = ~forest_mean, name = 'Forest')
  fig1 <- fig1 %>% add_trace(y = ~grass_mean, name = 'Grass')
  fig1 <- fig1 %>% add_trace(y = ~pasture_mean, name = 'Pasture')
  fig1 <- fig1 %>% add_trace(y = ~crop_mean, name = 'Crop')
  fig1 <- fig1 %>% add_trace(y = ~wet_mean, name = 'Wet')
  fig1 <- fig1 %>% add_trace(y = ~barren_mean, name = 'Barren')
  fig1 <- fig1 %>% add_lines(y = fit_rich, name = 'Sp richness')
  fig1 <- fig1 %>% add_lines(y = fit_shannon, name = 'Shannon index', yaxis = "y2")
  fig1 <- fig1 %>% add_lines(y = fit_simpson, name = 'Simpson index', color = 'grey', yaxis = "y2")
  
  ay <- list(
    side="right",
    overlaying = "y"
  )
  
  fig1 <- fig1 %>% layout(
    title = paste0("Ecoregion: ", ecoregs[i]), yaxis2 = ay,
    xaxis = list(title = "Year"),
    yaxis = list(title = "Land cover [%]")
  )
  
  Ecoreg_plots[[i]] <- fig1
}

save(Ecoreg_plots, file = "figures/ecoreg_plots.rda")

load("figures/ecoreg_plots.rda")
Ecoreg_plots[[70]]

library(htmlwidgets)

for(i in seq_along(unique(ecoreg$Ecoregion))){
  print(Ecoreg_plots[[i]])
}

