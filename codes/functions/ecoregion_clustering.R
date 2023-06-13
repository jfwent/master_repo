####Clustering function to cluster segments due to their land cover characteristics
# data set up:
# df where all rows are unique observations
# needs columns segment, year, ecoregion, urban, forest, grass, pasture, crop, wet, barren, other, 


cluster_data <- function(df, year, ecoregion, clust_method = "ward.D2", cores = 3) {
  
  library(tidyverse); library(pvclust); library(magrittr)
  
  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    `rownames<-`(.[,1]) %>%
    select(c(urban, forest, grass, pasture, crop, wet, barren, other)) %>%
    t()
  
  # Perform clustering using pvclust
  pv_fit <- pvclust(clustDat, method.hclust = clust_method, quiet = T, parallel = cores)
  
  pv_clust <- pvpick(pv_fit, alpha=.80, pv="au", type="geq")
  
  for(clust_num in seq_along(pv_clust$clusters)){
    
    clust_tmp <- pv_clust$clusters[[clust_num]]
    
    df_subset$cluster_nr[df_subset$segment %in% clust_tmp] <- clust_num
  }
  
  return(df_subset)
}
