### Clustering land cover change
####################################
#---- preparing the data -----

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

#---- calculate the land cover change over all years ----

segments = unique(dat$partition)

for(i in seq_along(segments)){
  
  tmp <- dat %>%
    filter(partition == segments[i]) %>%
    arrange(year)
  
  df_name <- paste0("diff_df")
  
  if(exists(df_name)){
    
    tmp.now <- tmp %>%
      select(-partition, -Ecoregion, -year, -sum_landcover)
    
    # d_tmp <- c(NA, diff(tmp.now))
    diff_tmp <- as.data.frame(apply(tmp.now, 2, diff))
    diff_tmp$year <- paste0(tmp$year[1:(length(tmp$year)-1)] ,"_", tmp$year[2:length(tmp$year)])    # tmp$year[2:length(tmp$year)]
    diff_tmp$partition <- segments[i]
    diff_tmp$Ecoregion <- tmp$Ecoregion[2:length(tmp$Ecoregion)]
    
    diff_tmp <- relocate(diff_tmp, year, partition, .before = urban) 
    
    diff_df <- rbind(diff_df, diff_tmp)
    
  }else{
    
    tmp.now <- tmp %>%
      select(-partition, -Ecoregion, -year, -sum_landcover)
    
    # d_tmp <- c(NA, diff(tmp.now))
    diff_tmp <- as.data.frame(apply(tmp.now, 2, diff))
    diff_tmp$year <- paste0(tmp$year[1:(length(tmp$year)-1)] ,"_", tmp$year[2:length(tmp$year)])
    diff_tmp$partition <- segments[i]
    diff_tmp$Ecoregion <- tmp$Ecoregion[2:length(tmp$Ecoregion)]
    
    diff_tmp <- relocate(diff_tmp, year, partition, .before = urban) 
    
    diff_df <- diff_tmp
  }
}

rm(diff_tmp, tmp, tmp.now)

save(diff_df, file = "data/land_cover_change.rda")

#-------- normalize ------

load("data/land_cover_change.rda")

diff_norm <- as.data.frame(scale(diff_df[,3:10], center=T, scale=T))
diff_norm$segment <- diff_df$partition
diff_norm$year <- diff_df$year
diff_norm$ecoregion <- diff_df$Ecoregion
diff_norm <- diff_norm %>%
  relocate(segment, year, ecoregion, .before = urban)

#------ select only ecoregions with >2 segments ----
grouped <- diff_norm %>%
  group_by(ecoregion, year) %>%
  summarise(segment_count = n())

grouped_filter <- diff_norm %>%
  group_by(ecoregion, year) %>%
  summarise(segment_count = n()) %>%
  filter(segment_count > 2) %>%
  ungroup()

diff_norm_filt <- diff_norm[diff_norm$ecoregion %in% unique(grouped_filter$ecoregion),] # 42 segments across all years filtered

#----- cluster 1 Ecoregion in 1 year ----

source("codes/functions/f_ecoregion_clustering.R")

ecoregs <- unique(diff_norm_filt$ecoregion)

clust_2001_ecoreg1 <- cluster_data(df = diff_norm_filt, year = "2001_2004", ecoregion = ecoregs[1])

head(clust_2001_ecoreg1)
rm(clust_2001_ecoreg1)

#---- build loop to cluster all ecoregions in all years ----

library(lubridate)

results <- list()

ecoregs <- unique(diff_norm_filt$ecoregion)
years <- unique(diff_norm_filt$year)

n_iter <- length(ecoregs)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = (n_iter*length(years)), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

for(j in years){
  
  #init[i] <- Sys.time()
  
  for(i in 1:n_iter){
    
    result <- cluster_data(diff_norm_filt, year = j, ecoregion = ecoregs[i])
    results[[paste0(ecoregs[i], "_", j)]] <- result
    
    setTxtProgressBar(pb, i)
  }
  
  #end[i] <- Sys.time()
  
  
  # time <- round(seconds_to_period(sum(end - init)), 0)
  # 
  # # Estimated remaining time based on the
  # # mean time that took to run the previous iterations
  # est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  # remainining <- round(seconds_to_period(est), 0)
  # 
  # cat(paste(" // Execution time:", time,
  #           " // Estimated time remaining:", remainining), "")
}

land_change_clustered <- bind_rows(results)
save(land_change_clustered, file="data/land_change_clustered.rda")

#----- find ecoregions with most cluster changes ----
library(dplyr)
load("data/delta_land_use_clustered.rda")

changes <- land_change_clustered %>%
  group_by(segment) %>%
  summarise(cluster_changes = n_distinct(cluster_nr))

changed_segments <- changes %>%
  filter(cluster_changes > 1) %>%
  arrange(desc(cluster_changes))

highlighted_data <- land_change_clustered %>%
  left_join(changed_segments, by = c("segment")) %>%
  mutate(cluster_changed = if_else(!is.na(cluster_changes), "Changed", "Unchanged"))

filtered_segments <- highlighted_data %>%
  filter(cluster_changes > 3) %>%
  distinct(segment)

ecoregions_with_cluster_changes <- land_change_clustered %>%
  inner_join(filtered_segments, by = "segment") %>%
  distinct(ecoregion)

print(paste0("Characterizing land use change: The ecoregions with segments changing clusters ",
             "the most often between years are found in: ",
             ecoregions_with_cluster_changes, "."))
