# cluster characterization


###---- **LAND USE** -----
  #---- load data----
rm(list=ls())
load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

  #---- find dominant land cover in cluster and assign name -----
library(dplyr)

years <- sort(unique(land$year))

cluster_means <- land %>%
  group_by(year, cluster) %>%
  summarise(
    urban_mean = mean(urban),
    forest_mean = mean(forest),
    grass_mean = mean(grass),
    pasture_mean = mean(pasture),
    crop_mean = mean(crop),
    wet_mean = mean(wet),
    barren_mean = mean(barren),
    other_mean = mean(other)
  )

cluster_means$dominant_habitat <- apply(cluster_means[, c('urban_mean', 'forest_mean', 'grass_mean', 'pasture_mean', 'crop_mean', 'wet_mean', 'barren_mean', 'other_mean')], 1, function(x) {
  land_cover_types <- c('urban', 'forest', 'grass', 'pasture', 'crop', 'wet', 'barren', 'other')
  land_cover_types[which.max(x)]
})

cluster_means <- relocate(cluster_means, dominant_habitat, .before = urban_mean)

# interesting ecoregions:
# Arkansas_Valley_1: dominant habitat from forest to urban in 2006 - 2008
# Blue_Ridge_1: from pasture to forest in 2004 - 2006
# Cascades_1: from forest to grass in 2013 - 2016
# Cascades_2: from grass to forest in 2013-2016 --> investigate if fragment of analysis because the switch is in both clusters
# Central_Appalachians_1: switch from forest to barren to pasture to forest
# Central_Appalachians_2: similar diversity of dominant land cover acorss years
# Central_Basin: in both clusters there are switches from barren to grass and back
# Central_Great_Plains clusters 1+2: switches from grass to crop and back
# Central_Irregular_Plains_1: switches from crop to pasture to urban and back
# Central_Irregular_Plains_2: switches from pasture to urban and back
# Chihuahuan_Desert_2: switch from crop to urban

# Side note: Blue_Mountains_2 dominant habitat is other, consider leaving this out?

  #---- plot dominant habitats ----
library(plotly); library(viridis)

# Abbreviate cluster names
cluster_means$cluster_abbrev <- abbreviate(cluster_means$cluster, minlength = 4)

# Define a colorblind-friendly palette
color_palette <- viridis(n = length(unique(cluster_means$dominant_habitat)), option = "D")

# Visualize the dominant habitats for each cluster across years with colorblind-friendly palette
ggplot(cluster_means, aes(x = year, y = cluster_abbrev, fill = dominant_habitat)) +
  geom_tile() +
  labs(x = "Year", y = "Cluster", fill = "Dominant Habitat") +
  scale_fill_manual(values = color_palette) +
  theme(
    axis.text.y = element_text(size = 6, angle = 45)
  ) +
  scale_y_discrete(expand = c(0, 0.5)) +
  scale_x_continuous(breaks = unique(cluster_means$year))

##### make subplots for better readability of plots 

# Identify the unique clusters
unique_clusters <- sort(unique(cluster_means$cluster))

# Divide the clusters into groups for subplots
num_subplots <- 4
clusters_per_subplot <- ceiling(length(unique_clusters) / num_subplots)

# Create a list to store the subplots
subplot_list <- list()
subplot_list_abbrev <- list()

# Loop through the clusters and create subplots
for (i in 1:num_subplots) {
  # Get the clusters for the current subplot
  cluster_subset <- unique_clusters[((i-1)*clusters_per_subplot + 1):(i*clusters_per_subplot)]
  
  # Filter the dominant habitat data for the current clusters
  dominant_clusters <- cluster_means %>%
    filter(cluster %in% cluster_subset) %>%
    group_by(cluster) %>%
    #filter(dominant_habitat == max(dominant_habitat)) %>%
    ungroup()
  
  # Abbreviate cluster names
  dominant_clusters$cluster_abbrev <- abbreviate(dominant_clusters$cluster, minlength = 4)
  
  # Define a colorblind-friendly palette
  color_palette <- viridis(n = length(unique(dominant_clusters$dominant_habitat)), option = "D")
  
  # Create the subplot and store it in the list
  subplot <- ggplot(dominant_clusters, aes(x = year, y = cluster, fill = dominant_habitat)) +
    geom_tile() +
    labs(x = "Year", y = "Cluster", fill = "Dominant Habitat") +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    scale_y_discrete(expand = c(0, 0.5)) +
    scale_x_continuous(breaks = unique(dominant_clusters$year),
                       guide = guide_axis(angle = 45)) +
    ggtitle(paste("Subplot", i))  # Add subplot title
  
  subplot_list[[i]] <- subplot
  
  # create subplot with abbreviated names
  subplot_abbrev <- ggplot(dominant_clusters, aes(x = year, y = cluster_abbrev, fill = dominant_habitat)) +
    geom_tile() +
    labs(x = "Year", y = "Cluster", fill = "Dominant Habitat") +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    scale_y_discrete(expand = c(0, 0.5)) +
    scale_x_continuous(breaks = unique(dominant_clusters$year),
                       guide = guide_axis(angle = 45)) +
    ggtitle(paste("Subplot", i))  # Add subplot title
  
  subplot_list_abbrev[[i]] <- subplot_abbrev
}

subplot_list[[1]]
subplot_list[[2]]
subplot_list[[3]]
subplot_list[[4]]

subplot_list_abbrev[[1]]
subplot_list_abbrev[[2]]
subplot_list_abbrev[[3]]
subplot_list_abbrev[[4]]

dominant_habitat_plots <- subplot_list

dominant_habitat_plots_abbrev <- subplot_list_abbrev

save(dominant_habitat_plots, file="figures/dominant_habitats_plots.rda")
save(dominant_habitat_plots_abbrev, file="figures/dominant_habitat_plots_abbrev.rda")

  #---- identify + plot habitat switches ----

library(ggplot2); library(viridis)

# Identify clusters that switched dominant habitats between consecutive years
switched_clusters <- cluster_means %>%
  group_by(cluster) %>%
  filter(dominant_habitat != lag(dominant_habitat)) %>%
  ungroup()

# Abbreviate cluster names
switched_clusters$cluster_abbrev <- abbreviate(switched_clusters$cluster, minlength = 4)

# Define a colorblind-friendly palette
color_palette <- viridis(n = length(unique(switched_clusters$dominant_habitat)), option = "D")

# Visualize the switched clusters
hab_switch_fig <- ggplot(switched_clusters, aes(x = year, y = factor(cluster_abbrev), fill = dominant_habitat)) +
  geom_tile() +
  labs(x = "Year", y = "Cluster", fill = "Dominant Habitat") +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  scale_y_discrete(expand = c(0, 0.5)) +
  scale_x_continuous(breaks = unique(switched_clusters$year),
                     guide = guide_axis(angle = 45))

jpeg(file="figures/hab_switch_fig.jpeg")
hab_switch_fig
dev.off()


###---- **DELTA LAND USE** ----
  #---- find out if clusters match ----
library(tidyr); library(dplyr)

rm(list=ls())

load("data/land_use_clustered.rda")
land_use <- combined_df; rm(combined_df)
load("data/delta_land_use_clustered.rda")
delta_land_use <- land_change_clustered; rm(land_change_clustered)

delta_land_use <- delta_land_use %>%
  separate(year, into = c("year1", "year2"), sep = "_", remove = F)

delta_land_use <- delta_land_use %>%
  unite(col = "cluster", c("ecoregion", "cluster_nr"), sep = "_", remove = F)


# Get unique years from land_use_change_data
years <- unique(delta_land_use$year1)
# years <- sort(unique(land_use$year))

# Iterate over each year and check if clusters match
for (year in years) {
  # Subset land_use_data for the current year
  land_use_data_year <- subset(land_use, year == year)
  
  # Subset land_use_change_data for the current year
  land_use_change_data_year <- subset(delta_land_use, year1 == year)
  
  # Perform an inner join based on common columns
  merged_data <- inner_join(land_use_data_year, land_use_change_data_year,
                            by = c("segment", "ecoregion", "cluster"))
  
  # Check if segments are in the same cluster for the current year
  if (nrow(merged_data) > 0) {
    print(paste("Segments are in the same cluster for year", year))
  } else {
    print(paste("Segments are not in the same cluster for year", year))
  }
}


  #---- recluster the land use change data based on the initial cluster of the ecoregion ----
  #----- 