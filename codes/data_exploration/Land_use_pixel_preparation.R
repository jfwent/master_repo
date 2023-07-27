# Land use data set preparation with pixels inside a 500 m buffer around the North American BBS segments
#Author: Jon Went
# Date: 27.07.2023

# ---- load libraries ----
library(tidyverse)
library(lubridate)

# ---- load data set ----

rm(list=ls())

land_use_pxsum <- read.delim("/Users/jonwent/Desktop/ETHZ/master_thesis/BBS.land_all.routes.pxsum.txt",
                             header = T, sep = "") %>%
  rename(segment = partition)

# load the land use data set that I characterised the segments into habitat clusters to compare
load("data/land_use_clustered_complete_df.rda")

# load the ecoregion txt file 

eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition)
 
# ---- compare the segments in both data sets ----

seg_pct <- unique(sort(land_complete$segment)) # 1923 segments
seg_px <- unique(sort(land_use_pxsum$segment)) # 3273 segments
seg_eco <- unique(sort(eco_txt$segment)) # 3326 segments

# ---- add ecoregion to land use pxsum data ----

land_use_pxsum_eco <- land_use_pxsum %>%
  left_join(eco_txt, by = "segment")

# check NA's
sum(is.na(land_use_pxsum_eco$Ecoregion)) # 10'800 NA's

# check what segments are not in the ecoregion file
missing_eco_segments <-  seg_eco %in% seg_px
sum(missing_eco_segments == F) # 1403 segments are in the ecoregion file but not in the land_use_pxsum file
missing_segs <- seg_eco[missing_eco_segments == F]
print(missing_segs) 
# missing are the 2nd and 4th segment of each route
# which we decided not to use to reduce overlap of the buffers
rm(missing_eco_segments, missing_segs)

land_use_pxsum_eco <- land_use_pxsum %>%
  left_join(eco_txt, by = "segment") %>%
  na.omit() %>%
  rename(ecoregion = Ecoregion) %>%
  mutate(urban.tot = urban.low+urban.high) %>%
  relocate(urban.tot, .before=forest)


# --- calculate areas ---- 

land_use_area <- land_use_pxsum_eco %>%
  mutate(
    across(
      .cols = c(4:12),
      .fns = list(
        area = \(x) x*(30*30)
        ),
      .names = '{col}.area.m2'
      )
  ) %>%
  select(-c(4:12)) %>%
  rowwise() %>%
  mutate(tot.area = sum(urban.tot.area.m2,
                        forest.area.m2,
                        grass.area.m2,
                        pasture.area.m2,
                        crop.area.m2,
                        wet.area.m2,
                        barren.area.m2))


# ---- get only ecoregions that have at > 2 segments ----

land_use_pxsum_sub <- land_use_pxsum_eco %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments > 2 ) %>%
  ungroup() %>%
  select(-c(3:5, tot_segments))

land_use_area_sub <- land_use_area %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments > 2 ) %>%
  ungroup() %>%
  select(-c(3:5, tot_segments))


# ---- function for pxsum ----

cluster_data <- function(df, year, ecoregion, clust_method = "ward.D2") {
  
  library(tidyverse); library(pvclust); library(magrittr)
  
  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    column_to_rownames(var = "segment") %>%
    select(c(urban.tot, forest, grass, pasture, crop, wet, barren)) %>%
    t()
  
  # Perform clustering using pvclust
  pv_fit <- pvclust(clustDat, method.hclust = clust_method, quiet = T)
  
  pv_clust <- pvpick(pv_fit, alpha=.80, pv="au", type="geq")
  
  for(clust_num in seq_along(pv_clust$clusters)){
    
    clust_tmp <- pv_clust$clusters[[clust_num]]
    
    df_subset$cluster_nr[df_subset$segment %in% clust_tmp] <- clust_num
  }
  
  return(df_subset)
}

tst <- cluster_data(land_use_pxsum_sub, year = 2004, ecoregion = ecoregs[1])
rm(tst)

# ----- loop over pxsum data ----

results <- list()

ecoregs <- sort(unique(land_use_pxsum_sub$ecoregion))

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

years <- unique(land_use_pxsum_sub$year)

for(j in years){
  
  init[j] <- Sys.time()
  
  #------------
  
  for(i in 1:n_iter){
    
    result <- cluster_data(land_use_pxsum_sub, year = j, ecoregion = ecoregs[i])
    results[[paste0(ecoregs[i], "_", j)]] <- result
    
  }
  
  #------------
  
  end[j] <- Sys.time()
  
  setTxtProgressBar(pb, j)
  time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}

tmp <- results %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(cluster = paste0(ecoregion, "_", cluster_nr))

land_use_pxsum_clustered <- bind_rows(results)

land_use_pxsum_clustered$cluster <- paste0(land_use_pxsum_clustered$ecoregion, "_", land_use_pxsum_clustered$cluster_nr)

summary(land_use_pxsum_clustered$cluster_nr) # 15 NA's

# ---- save files ----

