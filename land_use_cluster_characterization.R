# cluster characterization


##---- land use -----
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
library(plotly)

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

##### try again with subplots

# Identify the unique clusters
unique_clusters <- sort(unique(cluster_means$cluster))

# Divide the clusters into groups for subplots
num_subplots <- 4
clusters_per_subplot <- ceiling(length(unique_clusters) / num_subplots)

# Create a list to store the subplots
subplot_list <- list()

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
    scale_x_continuous(breaks = unique(dominant_clusters$year)) +
    ggtitle(paste("Subplot", i))  # Add subplot title
  
  subplot_list[[i]] <- subplot
}

subplot_list[[1]]
subplot_list[[2]]
subplot_list[[3]]
subplot_list[[4]]

dominant_habitat_plots <- subplot_list

save(dominant_habitat_plots, file="figures/dominant_habitats_plots.rda")

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
ggplot(switched_clusters, aes(x = year, y = factor(cluster_abbrev), fill = dominant_habitat)) +
  geom_tile() +
  labs(x = "Year", y = "Cluster", fill = "Dominant Habitat") +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  scale_y_discrete(expand = c(0, 0.5)) +
  scale_x_continuous(breaks = unique(switched_clusters$year))

##---- land use change ----
#---- recluster the land use change data based on the initial cluster of the ecoregion ----
rm(list=ls())


#----- 