# Land use data set preparation with pixels inside a 500 m buffer around the North American BBS segments
#Author: Jon Went
# Date: 27.07.2023

# ---- load libraries ----
library(tidyverse)
library(lubridate)

# ---- load data set ----

eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition)

land_use_pxsum <- read.delim("/Users/jonwent/Desktop/ETHZ/master_thesis/BBS.land_all.routes.pxsum.txt",
                             header = T, sep = "") %>%
  rename(segment = partition) %>%
  left_join(eco_txt, by = "segment") %>%
  na.omit() %>%
  rename(ecoregion = Ecoregion) %>%
  rowwise() %>%
  mutate(px.sum = sum(urban.low,
                      urban.high,
                      forest,
                      grass,
                      pasture,
                      crop,
                      wet,
                      barren)) %>%
  filter(year %in% 2001 | year %in% 2019)

land_use_area <- land_use_pxsum %>%
  mutate(
    across(
      .cols = c(4:11),
      .fns = list(
        area = \(x) x*(30*30)
      ),
      .names = '{col}.area.m2'
    )
  ) %>%
  select(-c(4:11, px.sum)) %>%
  rowwise() %>%
  mutate(tot.area.m2 = sum(urban.low.area.m2,
                           urban.high.area.m2,
                           forest.area.m2,
                           grass.area.m2,
                           pasture.area.m2,
                           crop.area.m2,
                           wet.area.m2,
                           barren.area.m2)) %>%
  filter(year %in% 2001 | year %in% 2019)

save(land_use_area, file = "data/land_use_area_t1_t2.rda")
save(land_use_pxsum, file = "data/land_use_pxsum_t1_t2.rda")

# ====================== old stuff ----
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
  mutate(tot.area.m2 = sum(urban.tot.area.m2,
                        forest.area.m2,
                        grass.area.m2,
                        pasture.area.m2,
                        crop.area.m2,
                        wet.area.m2,
                        barren.area.m2))


# ---- get only ecoregions that have > 2 segments ----

land_use_pxsum_sub <- land_use_pxsum_eco %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments > 2) %>% # filter out ecoregions with <= 2 segments, 
  rowwise() %>%
  mutate(check = sum(c(urban.tot, forest, grass, pasture, crop, wet, barren))) %>%
  filter(check != "0") %>% # filter out all segments with 0 characterized land use, will cause error in cluster functino
  ungroup() %>%
  select(-c(3:5, tot_segments, check))

land_use_area %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments <= 2) %>%
  mutate(cluster_nr = ifelse(tot_segments == 1, tot_segments, NA)) %>%
  filter(is.na(cluster_nr)) %>%
  arrange(segment)
  

land_use_area_sub <- land_use_area %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments > 2 ) %>%
  ungroup() %>%
  select(-c(route, 5:6, tot_segments)) %>%
  rowwise() %>%
  mutate(check = sum(urban.tot.area.m2, forest.area.m2, grass.area.m2, pasture.area.m2, 
                     crop.area.m2, wet.area.m2, barren.area.m2)) %>%
  filter(check != 0) %>%
  ungroup() %>%
  select(-check)


# ---- clustering function for pxsum ----

cluster_data <- function(df, year, ecoregion, clust_method = "ward.D2") {
  
  library(tidyverse); library(pvclust); library(magrittr)
  
  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    column_to_rownames(var = "segment") %>%
    select(c(urban.tot, forest, grass, pasture, crop, wet, barren)) %>%
    t()
  
  # Debugging: Inspect clustDat before clustering
  # browser()
  
  # Perform clustering using pvclust
  pv_fit <- pvclust(clustDat, method.hclust = clust_method, quiet = T)
  
  pv_clust <- pvpick(pv_fit, alpha=.80, pv="au", type="geq")
  
  for(clust_num in seq_along(pv_clust$clusters)){
    
    clust_tmp <- pv_clust$clusters[[clust_num]]
    
    df_subset$cluster_nr[df_subset$segment %in% clust_tmp] <- clust_num
  }
  
  return(df_subset)
}

# ----- test function -----


tst3 <- cluster_data(land_use_pxsum_sub, year = 2001, ecoregion = "Ridge_and_Valley")
rm(tst)

ecoregs <- sort(unique(land_use_pxsum_sub[land_use_pxsum_sub$year == "2001",]$ecoregion))
years <- unique(land_use_pxsum_sub[land_use_pxsum_sub$year == "2001",]$year)

tst <- list()

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(length(ecoregs))
end <- numeric(length(ecoregs))

for(i in 1:n_iter){
  
  init[i] <- Sys.time()
  
  #--------
  
  tmp <- cluster_data(land_use_pxsum_sub, year = 2001, ecoregion = ecoregs[i])
  tst[[paste0(ecoregs[i], "_", 2001)]] <- tmp
  
  #------------
  
  end[i] <- Sys.time()
  
  setTxtProgressBar(pb, i)
  time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
  
}

tst <- tst %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(cluster = paste0(ecoregion, "_", cluster_nr))


sum(is.na(tst$cluster_nr)) # 19 NA's

tst %>%
  filter(is.na(cluster_nr))

rm(tst, tmp, NA_tmp)

# ----- loop over pxsum data ----

set.seed(123)

results <- list()

ecoregs <- sort(unique(land_use_pxsum_sub$ecoregion))
years <- unique(land_use_pxsum_sub$year)

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(length(years))
end <- numeric(length(years))

for(j in 1:length(years)){
  
  init[j] <- Sys.time()
  
  #------------
  
  for(i in 1:n_iter){
    
    result <- cluster_data(land_use_pxsum_sub, year = years[j], ecoregion = ecoregs[i])
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

land_use_pxsum_clustered <- results %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(cluster = paste0(ecoregion, "_", cluster_nr))

summary(land_use_pxsum_clustered) # 159 NA's in the cluster nr

# ---- run again for the NA's ----

pxsum_not_clustered <- land_use_pxsum_clustered %>%
  filter(is.na(cluster_nr))

pxsum_run_again <- land_use_pxsum_sub %>%
  filter(year %in% pxsum_not_clustered$year,
         ecoregion %in% pxsum_not_clustered$ecoregion)

results.NA <- list()

ecoregs <- sort(unique(pxsum_run_again$ecoregion))
years <- unique(pxsum_run_again$year)

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(length(years))
end <- numeric(length(years))

for(j in years){
  
  init[j] <- Sys.time()
  
  #------------
  
  for(i in 1:n_iter){
    
    result <- cluster_data(pxsum_run_again, year = j, ecoregion = ecoregs[i])
    results.NA[[paste0(ecoregs[i], "_", j)]] <- result
    
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

pxsum_NA_clustered <- results.NA %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(cluster = paste0(ecoregion, "_", cluster_nr))

sum(is.na(pxsum_NA_clustered$cluster_nr)) # 164 NA's

# ------ cluster the land_use_area_sub file ----

cluster_area_data <- function(df, year, ecoregion, clust_method = "ward.D2") {
  
  library(tidyverse); library(pvclust); library(magrittr)
  
  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    column_to_rownames(var = "segment") %>%
    select(c(4:10)) %>%
    t()
  
  # Debugging: Inspect clustDat before clustering
  # browser()
  
  # Perform clustering using pvclust
  pv_fit <- pvclust(clustDat, method.hclust = clust_method, quiet = T)
  
  pv_clust <- pvpick(pv_fit, alpha=.80, pv="au", type="geq")
  
  for(clust_num in seq_along(pv_clust$clusters)){
    
    clust_tmp <- pv_clust$clusters[[clust_num]]
    
    df_subset$cluster_nr[df_subset$segment %in% clust_tmp] <- clust_num
  }
  
  return(df_subset)
}

set.seed(123)

results.area <- list()

ecoregs <- sort(unique(land_use_area_sub$ecoregion))
years <- unique(land_use_area_sub$year)

n_iter <- length(years)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(length(years))
end <- numeric(length(years))

for(j in 1:length(years)){
  
  init[j] <- Sys.time()
  
  #------------
  
  for(i in 1:n_iter){
    
    result <- cluster_data(land_use_pxsum_sub, year = years[j], ecoregion = ecoregs[i])
    results.area[[paste0(ecoregs[i], "_", years[j])]] <- result
    
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

land_use_area_clustered <- results.area %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(cluster = paste0(ecoregion, "_", cluster_nr))

sum(is.na(land_use_area_clustered$cluster_nr)) # 164 NA's

#---- add previously removed segments -----

land_use_pxsum_rest <- land_use_pxsum_eco %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments <= 2) %>%
  select(-tot_segments, -route, -urban.low, -urban.high) %>%
  mutate(cluster_nr = 1,
         cluster = paste0(ecoregion, "_", cluster_nr))

land_use_area_rest <- land_use_area %>%
  group_by(ecoregion, year) %>%
  mutate(tot_segments = n()) %>%
  arrange(tot_segments) %>%
  filter(tot_segments <= 2) %>%
  select(-tot_segments, -route, -tot.area.m2, -urban.low.area.m2, -urban.high.area.m2) %>%
  mutate(cluster_nr = 1,
         cluster = paste0(ecoregion, "_", cluster_nr))

land_use_area_complete <- land_use_area_clustered %>%
  bind_rows(land_use_area_rest)

land_use_pxsum_complete <- land_use_pxsum_clustered %>%
  bind_rows(land_use_pxsum_rest)

tst <- land_use_area %>%
  anti_join(land_use_area_complete, by = c("year", "segment", "ecoregion"))

# Piedmont segment 63_024_5 has 0 land use characterization therefore is excluded

# ---- save files ----

save(land_use_area_complete, file = "data/land_use_area_clustered.rda")
save(land_use_pxsum_complete, file = "data/land_use_pxsum_clustered.rda")
