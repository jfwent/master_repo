##### Clustering the land cover data
#####################################

###==== prepare data sets =======
rm(list=ls())

library(dplyr)

load("data/Lica/BBS_land_years.rda")
land <- BBS_land_years; rm(BBS_land_years)
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")
dat <- merge(land, ecoreg, by="partition"); rm(land, ecoreg)

# summarize urban land covers
dat$urban <- dat$urban.high+dat$urban.low

dat$sum_landcover <- rowSums(dat[, 4:11]) # calculate the land cover that is characterized
dat <- dat %>% mutate(other = 100 - sum_landcover) # calculate the uncharacterized land cover

dat <- dat %>% 
  relocate(urban, .before = forest) %>%
  relocate(other, sum_landcover, .before = FID)

dat <- dat %>%
  select(-route, -urban.high, -urban.low, -FID, -Kilometers)

###==== normalize data =====

dat_norm <- as.data.frame(scale(dat[,3:10], center=T, scale=T))
dat_norm$segment <- dat$partition
dat_norm$year <- dat$year
dat_norm$ecoregion <- dat$Ecoregion
dat_norm <- dat_norm %>%
  relocate(segment, year, ecoregion, .before = urban)

###==== count data points for each ecoregion in each year======

grouped <- dat_norm %>%
  group_by(ecoregion, year) %>%
  summarise(segment_count = n())

summary(grouped$segment_count)
sd(grouped$segment_count)

grouped_filter <- dat_norm %>%
  group_by(ecoregion, year) %>%
  summarise(segment_count = n()) %>%
  filter(segment_count > 2) %>%
  ungroup()

dat_norm_filt <- dat_norm[dat_norm$ecoregion %in% unique(grouped_filter$ecoregion),]

###==== hierarchical bottom up clustering function for 1 Ecoregion in 1 year =====

cluster_data <- function(df, year, ecoregion, clust_method = "ward.D2") {
  
  library(tidyverse); library(pvclust); library(magrittr)

  df_subset <- df %>%
    filter(year == {{ year }}, ecoregion == {{ ecoregion }})
  
  # Select columns for clustering
  clustDat <- df_subset %>%
    `rownames<-`(.[,1]) %>%
    select(c(urban, forest, grass, pasture, crop, wet, barren, other)) %>%
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


###==== try function ====

ecoregs <- unique(dat_norm$ecoregion)

clust_2001_ecoreg1 <- cluster_data(df = dat_norm, year = 2001, ecoregion = ecoregs[1])

head(clust_2001_ecoreg1)

rm(clust_2001_ecoreg1)

###==== build function to analyze all ecoregions ====

results <- list()

ecoregs <- unique(dat_norm_filt$ecoregion)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(ecoregs), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for (ecoreg in ecoregs) {
  
  result <- cluster_data(dat_norm, 2001, ecoregion = ecoreg)
  results[[paste0(ecoreg, "_", 2001)]] <- result
  
  setTxtProgressBar(pb, ecoreg)
}

combined_df <- bind_rows(results)

ecoregs_clustered_2001 <- unique(combined_df$ecoregion)

ecoregs_missing <- lubridate::setdiff(ecoregs, ecoregs_clustered_2001)

wyoming_clust = cluster_data(df = dat_norm, year = 2001, ecoregion = "Wyoming_Basin")

north_casc_clust = cluster_data(df = dat_norm, year = 2001, ecoregion = "North_Cascades")

col_plateau_clust = cluster_data(df = dat_norm, year = 2001, ecoregion = "Columbia_Plateau")

combined_df_all_2001 <- bind_rows(combined_df, wyoming_clust, north_casc_clust, col_plateau_clust)

sum(is.na(combined_df_all_2001$cluster_nr)) # 19 data points could not be clustered

rm(results, combined_df, combined_df_all, wyoming_clust, north_casc_clust, col_plateau_clust)
rm(result, pb, wasatch_clust, all_results)
rm(ecoreg, ecoregs, ecoregion, ecoregs_clustered_2001, ecoregs_missing, ecoregs_tmp)
rm(n_iter, cluster_all_data)

###==== build loop to analyze all years ====
library(lubridate)

results <- list()

ecoregs <- unique(dat_norm_filt$ecoregion)

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

years <- unique(dat_norm_filt$year)

for(j in years){
  
  for(i in 1:n_iter){
    init[i] <- Sys.time()
    
    #------------
    
    result <- cluster_data(dat_norm, year = j, ecoregion = ecoregs[i])
    results[[paste0(ecoregs[i], "_", j)]] <- result
    
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
}

combined_df <- bind_rows(results)

combined_df$cluster <- paste0(combined_df$ecoregion, "_", combined_df$cluster_nr)

summary(combined_df$cluster_nr) # 150 NA's --> approx 19 per year

save(combined_df, file = "data/land_use_clustered.rda")

###=====

changes <- combined_df %>%
  group_by(segment, year) %>%
  summarise(cluster_changes = n_distinct(cluster))

changed_segments <- changes %>%
  filter(cluster_changes > 1) %>%
  arrange(desc(cluster_changes)) # O segments changed cluster

###==== previous attempt 1 ====

cluster_info <- rep(NA, nrow(dat_norm))

m <- c( "average", "single", "complete", "ward")

names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(df_tmp_vars, method = x)$ac
}


for(years in unique(dat_norm$year)){
  
  tmp_sub_yr <- subset(dat_norm, year %in% years)
  
  for(ecoregions in dat_norm$ecoregion){
    
    tmp_sub <- subset(tmp_sub_yr, ecoregion %in% ecoregions)
    
    optics_result <- optics(tmp_sub, eps = 0.5)
    clusters <- extractDBSCAN(optics_result)
    
  }
  
}

ecoregions <- unique(dat_norm$ecoregion)

cluster_ecoreg <- function(df, year){
  
  library(clValid); library(stats); library(cluster); library(purrr); library(pvclust)
  
  df <- subset(dat_norm, year == 2004)
  
  cluster_results <- matrix(NA, nrow = length(df$segment), ncol = 2)
  
  for(ecoregions in length(unique(df$ecoregion))){
    
    df_tmp <- subset(df, ecoregion %in% ecoregions[2])
    df_tmp_vars <- df_tmp[,4:10]
    rownames(df_tmp_vars) <- df_tmp$segment
    
    # cl_valid_pied <- clValid(obj = df_tmp_vars, nClust = 2:20,
    #                          clMethods = c("hierarchical"),
    #                          metric = "euclidean")
    # 
    # hcl_opt <- optimalScores(cl_valid_pied)
    # 
    # hclust_temp <- agnes(df_tmp_vars, method = "complete")
    
    res <- map_dbl(m, ac)
    method_res <- names(res)[which.max(res)]
    
    # plot(hclust_temp)
    # hclusters <- cutree(hclust_temp, k = min(hcl_opt$Clusters))
    
    df_tmp_vars_trans <- t(df_tmp_vars)
    
    fit_pvc <- pvclust(df_tmp_vars_trans, method.hclust = method_res)
    
    pvrect(fit_pvc, alpha=.80, pv="au", type="geq")
    clusters_pvpick <- pvpick(fit_pvc, alpha=.80, pv="au", type="geq")
    
    df_tmp$clusters_pvpick <- NA
    
    for(clust_num in seq_along(clusters_pvpick$clusters)){
      
      clust_tmp <- clusters_pvpick$clusters[[clust_num]]
      
      df_tmp$clusters_pvpick[df_tmp$segment %in% clust_tmp] <- clust_num
    }
    
  }
  
}

###==== previous attempt 2 ====
library(NbClust)

cluster_df_now <- data.frame()
cluster_df <- data.frame()

df_sub <- subset(dat_norm, year == 2004)
clusters <- rep(NA, nrow(df_sub))
count = 0

for (ecoregion_name in unique(df_sub$Ecoregion)) {
  
  sub_dat <- subset(df_sub, Ecoregion == ecoregion_name)[, 4:11]
  
  if (nrow(sub_dat) >= 10) {
    
    wss <- rep(NA, 10)
    
    for (i in 2:10) {
      wss[i] <- sum(kmeans(sub_dat, centers = i)$withinss)
    }
    
    k_opt <- which.min(wss)
    
    if (k_opt >= 1 && k_opt <= nrow(sub_dat)) {
      
      cluster_now <- kmeans(sub_dat, centers = k_opt)$cluster
      cluster_now <- paste0(ecoregion_name, "_", cluster_now)
      clusters[df_sub$Ecoregion %in% ecoregion_name] <- cluster_now
      
    } else {
      
      print(paste0("Skipping ecoregion ", ecoregion_name, " for year ", 2004, 
                   " due to invalid number of cluster centers."))
      
      count <- count + 1
    }
    
  } else {
    
    count = count + 1
    
    print(paste0("Skipping ecoregion ", ecoregion_name, " for year ", 2004, 
                 " due to insufficient distinct data points. There were ", count, " skipped ecoregions."))
  }
}

sum(is.na(clusters))
sub_dat$cluster <- clusters

###==== previous attmept 3 ====

cluster_ecoreg <- function(df, year_now){
  
  df_sub <- subset(df, year == year_now)
  clusters <- rep(NA, nrow(df_sub))
  count = 0
  
  for (ecoregion_name in unique(df_sub$Ecoregion)) {
    
    sub_dat <- subset(df_sub, Ecoregion == ecoregion_name)[, 4:11]
    
    if (nrow(sub_dat) >= 10) {
      
      wss <- rep(NA, 10)
      
      for (i in 2:10) {
        wss[i] <- sum(kmeans(sub_dat, centers = i)$withinss)
      }
      
      k_opt <- which.min(wss)
      
      if (k_opt >= 1 && k_opt <= nrow(sub_dat)) {
        
        cluster_now <- kmeans(sub_dat, centers = k_opt)$cluster
        cluster_now <- paste0(ecoregion_name, "_", cluster_now)
        clusters[df_sub$Ecoregion %in% ecoregion_name] <- cluster_now
        
      } else {
        
        print(paste0("Skipping ecoregion ", ecoregion_name, " for year ", year_now, 
                     " due to invalid number of cluster centers."))
        
        count <- count + 1
      }
      
    } else {
      
      count = count + 1
      
      print(paste0("Skipping ecoregion ", ecoregion_name, " for year ", year_now, 
              " due to insufficient distinct data points. There were ", count, " skipped ecoregions."))
    }
  }
  
  df_sub$cluster <- clusters
  
  return(df_sub)
}
    
    