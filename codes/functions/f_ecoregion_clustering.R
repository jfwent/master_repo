####Clustering function to cluster segments due to their land cover characteristics
# data set up:
# df where all rows are unique observations
# needs columns segment, year, ecoregion, urban, forest, grass, pasture, crop, wet, barren, other,
# the ecoregions need > 2 segments per year, < 2 observations are not allowed for the employed clustering algorithm

cluster_data <- function(df, # specify name of data frame with above characteristics
                         year, # specify year to filter data
                         ecoregion, # specify ecoregion to be clustered
                         clust_method = "ward.D2" # specify clustering method, default "ward.D2"
                                                    # others that are allowed: see method.hclust in pvclust
                         ) {
  
  library(tidyverse); library(pvclust); library(magrittr)
  
  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    `rownames<-`(.[,1]) %>%
    select(c(urban, forest, grass, pasture, crop, wet, barren, other)) %>%
    t()
  
  # Perform clustering using pvclust
  pv_fit <- pvclust(clustDat, method.hclust = clust_method, quiet = T, parallel = T)
  
  pv_clust <- pvpick(pv_fit, alpha=.80, pv="au", type="geq")
  
  for(clust_num in seq_along(pv_clust$clusters)){
    
    clust_tmp <- pv_clust$clusters[[clust_num]]
    
    df_subset$cluster_nr[df_subset$segment %in% clust_tmp] <- clust_num
  }
  
  return(df_subset)
}
