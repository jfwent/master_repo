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

land_cover_changes <- dat %>%
  select(partition, year, urban, forest, grass, pasture, barren, crop, wet, other)

land_cover_changes_pivot <- land_cover_changes %>%
  pivot_longer(cols = starts_with("urban"), names_to = "land_cover_variable", values_to = "value")

land_cover_changes_summary <- land_cover_changes_pivot %>%
  group_by(partition, land_cover_variable) %>%
  mutate(change = value - lag(value))

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

#----- cluster ----


