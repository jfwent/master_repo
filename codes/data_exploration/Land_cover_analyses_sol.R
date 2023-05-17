#==================================================#
#    LAND USE EXPLORATION FOR THE BBS PROJECT      #
#==================================================#

# SELECT ECOREGION WITH 10 OR MORE PARTITIONS
#==============================================

### GOAL: Subset the data, only using ecoregions with enough surveys (>10 years available)

land <- read.delim('data/Lica/bbs.land.years.txt', sep = "")
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

    land2 <- merge(land, ecoreg, by="partition")
    land2$urban <- land2$urban.low + land2$urban.high
    
    library(dplyr)
    land2sub <- land2 %>%
      group_by(Ecoregion, year) %>%
      filter(n() >= 10) %>%
      ungroup() %>%
      group_by(Ecoregion) %>%
      filter(all(table(year) >= 10)) %>%
      ungroup()
    land2sub[] <- lapply(land2sub, function(x) if(is.factor(x)) factor(x) else x)
    


# COMPARE PERCENTAGES OF DIFFERENT HABITATS BY ECOREGION
#=========================================================

### GOAL: Produce plots describing habitats changes within Ecoregions

      library(ggplot2); library(tidyr)
      
      # Sample data
      set.seed(123)
      
      # estimate mean % of each habitat per ecoregion in each year
      habmean <- land2sub %>%
        group_by(Ecoregion, year) %>%
        summarize(forest_mean = mean(forest),
                  urban_mean = mean(urban),
                  crop_mean = mean(crop),
                  pasture_mean = mean(pasture), 
                  grass_mean = mean(grass),
                  wet_mean = mean(wet),
                  barren_mean = mean(barren)
        )
      
      # Reshape data
      
      df_long <- habmean %>%
        gather(key = "habitat", value = "percentage", -c(Ecoregion, year))
      
      
      
      # Plot single ecoregion
      subset_df <- df_long[df_long$Ecoregion == "Arizona/New_Mexico_Mountains", ]
      
      ggplot(subset_df, aes(x = year, y = percentage, color = habitat)) +
        geom_line() +
        labs(x = "Year", y = "% of area") +
        scale_color_manual(values = c("forest_mean" = "darkgreen", "urban_mean" = "gray", "crop_mean" = "yellow", "pasture_mean" = "lightblue", "grass_mean" = "black", "wet_mean" = "purple", "barren_mean" = "#66FFFF")) +
        theme_minimal()
      
      # Plot and print all ecoregions
      
      ERs <- unique(df_long$Ecoregion)
      num_ecoregions <- length(ERs)
      
      library(viridisLite)
      
      
      for(i in seq(1, num_ecoregions, 12)) {
        subset_df <- subset(df_long, Ecoregion %in% ERs[i:(i+11)])
        
        p <- ggplot(subset_df, aes(x = year, y = percentage, color = habitat)) +
          geom_line() +
          labs(x = "Year", y = "% of area") +
          scale_color_manual(values = c("forest_mean" = "#009392", 
                                        "urban_mean" = "black", 
                                        "crop_mean" = "yellow", 
                                        "pasture_mean" = "#eeb479", 
                                        "grass_mean" = "#9ccb86",
                                        "wet_mean" = "#7F8C8D",
                                        "barren_mean" = "#66FFFF"))  +  
          theme_bw() +
          facet_wrap(~ Ecoregion, nrow = 4, ncol = 4)
        
        ggsave(paste0("Habitats_Ecoregions", i, "_to_", i+11, ".pdf"), p, width = 10, height = 6, units = "in")
      }
      
      


# COMPARE % HABITATS ACROSS PARTITIONS WITH PARTICULAR ECOREGIONS
#=================================================================

### GOAL: Produce plots describing habitats changes within all partitions of the ecoregion

    Piedmont <- land2sub %>%   #you change the ecoregion by search and replace
      subset(Ecoregion == "Piedmont")
    
    length(table(Piedmont$partition))
    
    
    # Reshape data
    
    df_long <- Piedmont[,c(1,2,6:11,15)] %>%
      gather(key = "habitat", value = "percentage", -c(partition, year))
    
    
    # Plot and print
    
    ERs <- unique(df_long$partition)
    num_partitions <- length(ERs)
    
    library(viridisLite)
    
    
    for(i in seq(1, num_partitions, 12)) {
      subset_df <- subset(df_long, partition %in% ERs[i:(i+11)])
      
      p <- ggplot(subset_df, aes(x = year, y = percentage, color = habitat)) +
        geom_line() +
        labs(x = "Year", y = "% of area") +
        scale_color_manual(values = c("forest" = "#009392", 
                                      "urban" = "black", 
                                      "crop" = "yellow", 
                                      "pasture" = "#eeb479", 
                                      "grass" = "#9ccb86",
                                      "wet" = "#7F8C8D",
                                      "barren" = "#66FFFF"))  +  
        theme_bw() +
        facet_wrap(~ partition, nrow = 4, ncol = 4)
      
      ggsave(paste0("Habitats_partition_Piedmont", i, "_to_", i+11, ".pdf"), p, width = 10, height = 6, units = "in")
    }
    


#    CLUSTERING PARTITIONS BY HABITAT 
#========================================

### GOAL: To pool together all partitions from a same ecoregion with a cluster analysis

    # We will pool partitions that in 2001 had similar habitats using a K means cluster
    # This will provide better resolution for the survey data
    
    ## Subset the data for a single ecoregion
    
    ecoregion_name <- "Piedmont"
    Piedmont <- land2sub %>% 
      subset(Ecoregion == ecoregion_name)
    Piedmont2001 <- as.data.frame(subset(Piedmont, year == "2001"))
    
    hab_Piedmont2001 <- Piedmont2001[,c(6:11,15)]
    rownames(hab_Piedmont2001) <- Piedmont2001$partition
    
    
    
    ## Now we use a cluster analysis to create homogenous groups of partitions
    
    # Estimate optimal number of clusters with "elbow method". 
    # This method involves plotting the within-cluster sum of squares (WSS) as a function of the number of clusters,
    # and selecting the number of clusters where the change in WSS begins to level off or form an "elbow".
    # This point is often considered to be a good compromise between model complexity and explanatory power. 
    # In the code, the elbow method is implemented by computing the WSS for a range of cluster numbers (2 to 10), 
    # and selecting the number of clusters that minimizes the WSS.
    
    wss <- (nrow(hab_Piedmont2001)-1)*sum(apply(hab_Piedmont2001,2,var))
    for (i in 2:10) wss[i] <- sum(kmeans(hab_Piedmont2001, centers=i)$withinss)
    plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    k_opt <- which.min(wss)
    
    # Run k-means clustering with the optimal number of clusters
    cluster <- kmeans(hab_Piedmont2001, centers=k_opt)$cluster
    
    # Add a prefix to the cluster code based on the Ecoregion
    cluster <- paste0(ecoregion_name, "_", cluster)
    
    # Assign the cluster codes to the surveys for the current Ecoregion
    hab_Piedmont2001$cluster <- cluster
    
    # View updated data
    head(hab_Piedmont2001)
    # View(hab_Piedmont2001)

    

#     ESTIMATING HETEROGENEITY WITHIN PARTITIONS
#===================================================   

### GOAL: Estimate habitat heterogeneity in the data 

  # Load required package
  library(vegan)

  # Calculate Simpson's and Shannon's Diversity Indices for habitats
  
  # Shannon's index takes into account both the number of habitats and their relative abundances, 
    # while Simpson's index focuses more on the dominance of the most abundant habitat. 

  habitat_matrix <- as.matrix(hab_Piedmont2001[, c(1:7)])  

  shannon <- diversity(habitat_matrix, index = "shannon")
    hab_Piedmont2001$shannon <- shannon
  
  simpson <- diversity(habitat_matrix, index = "simpson")
    hab_Piedmont2001$simpson <- simpson
  
    
  # We study how different habitats contribute to heterogeneity
    
    hab_Piedmont2001$partition <- row.names(hab_Piedmont2001)
    df_long <- hab_Piedmont2001[,-c(9,10)] %>%
      gather(key = "habitat", value = "percentage", -c(cluster, partition))
    df_long <- merge(df_long, hab_Piedmont2001[,c(9,10,11)], by="partition")
    
    fig1_Piedmont <- ggplot(df_long, aes(x = simpson, y = percentage, color = habitat)) +
      geom_smooth() +
      labs(x = "Simpson index", y = "% of area") +
      scale_color_manual(values = c("forest" = "#009392", 
                                    "urban" = "black", 
                                    "crop" = "yellow", 
                                    "pasture" = "#eeb479", 
                                    "grass" = "#9ccb86",
                                    "wet" = "#7F8C8D",
                                    "barren" = "#66FFFF"))  +  
          theme_bw()

    fig2_Piedmont <- ggplot(df_long, aes(x = shannon, y = percentage, color = habitat)) +
      geom_smooth() +
      labs(x = "Shannon index", y = "% of area") +
      scale_color_manual(values = c("forest" = "#009392", 
                                    "urban" = "black", 
                                    "crop" = "yellow", 
                                    "pasture" = "#eeb479", 
                                    "grass" = "#9ccb86",
                                    "wet" = "#7F8C8D",
                                    "barren" = "#66FFFF"))  +  
          theme_bw()
    
    fig1_Piedmont
    fig2_Piedmont
    ggsave(paste0("Simpson_Piedmont_2001.pdf"),fig1_Piedmont, width = 10, height = 6, units = "in")
    ggsave(paste0("Shannon_Piedmont_2001.pdf"),fig2_Piedmont, width = 10, height = 6, units = "in")
    
    
    