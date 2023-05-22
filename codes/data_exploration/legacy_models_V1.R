##### Legacy models exploration
##########################################


### The goal of this is to explore different set-ups of legacy models, according to Bird diversity~Land cover
### I want to compare 2 models to each other:
#1. Bird diversity in 2019 explained by land cover in 2019. This model is according to no legacy effects or rapid change.
#2. Bird diversity in 2019 explained by past land cover (for example 2001). This model is according to legacy effects.
#3. If time permits I want to build models of each year's bird diversity data with the corresponding land cover data (2001-2019),
# and predict to 2021 to see how the prediction strength of the models changes.


###===================
# Load the data
library(dplyr); library(tidyr)
rm(list=ls())
load("data/Lica/BBS_land_years.rda")
load("data/Lica/BBS_partition_abundance.rda")
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

land <- BBS_land_years; #rm(BBS_land_years)
abund <- BBS_partition_abundance; rm(BBS_partition_abundance)

land <- land %>%
  rowwise() %>%
  mutate(urban = sum(urban.low, urban.high)) %>%
  select(-urban.low, -urban.high, -route)

land_eco <- merge(land, ecoreg, by="partition", all.x = T, all.y = F)

land <- land_eco %>%
  select(-FID, -Kilometers)

land <- land %>%
  group_nest(year)

rm(land_eco)

#### calculate diversity indices for birds for all years

segments <- unique(abund$partition)
years <- unique(abund$year)
birds <- unique(abund$animal_jetz)

library(vegan)

mat_res <- matrix(NA, nrow=length(segments), ncol = length(birds))
colnames(mat_res) <- birds
rownames(mat_res) <- segments

div_list <- list()

tmp_list <- list()

for(i in seq_along(years)){
  
  tmp <- abund %>%
    subset(year == years[i]) %>%
    group_by(partition)
  
  tmp_list[[i]] <- tmp
  
  tmp <- tmp[,-(2:8)]
  
  seg_list <- list()
  
  for(j in seq_along(segments)){
    
    tmp_seg <- tmp[tmp$partition == segments[j],]
    
    tmp_pivot <- pivot_wider(tmp_seg, names_from = animal_jetz, 
                             values_from = seg_abundance, values_fill = 0)
    
    seg_list[[j]] <- tmp_pivot
  }
  
  mat_res[is.na(mat_res)] <- 0
  
  div_df <- as.data.frame(matrix(NA, nrow = length(segments), ncol=4))
  colnames(div_df) <- c("shannon", "simpson", "richness", "segment")
  
  div_df$shannon <- diversity(mat_res, index = "shannon")
  div_df$simpson <- diversity(mat_res, index = "simpson")
  div_df$richness <- rowSums(mat_res != 0)
  div_df$segment <- segments
  
  div_list[[i]] <- div_df
}

rm(mat_res, i, j, tmp, div_df)
rm(tmp_list, segment, species, abundance)

names(div_list) <- years

div_2000 <- div_list[[1]]

sum(div_2000$richness != 0)

###### Baustelle



# if (!require(reshape2)){
#   install.packages('reshape2')
#   library(reshape2)
# }
# 
# mydt = dcast(tmp, partition ~ animal_jetz,
#              fun.aggregate = length,
#              value.var =  "seg_abundance")
# 
# rownames(mydt) <- mydt$partition
# mydt <- mydt[,-1]

tmp <- abund %>%
  subset(year == 2000) %>%
  group_by(partition)

tmp <- tmp[,-(2:8)]

tmp_seg1 <- tmp[tmp$partition == segments[1],]

tmp_pivot <- pivot_wider(tmp_seg1, names_from = animal_jetz, 
                         values_from = seg_abundance, values_fill = 0)

a <- seg_list[[1]]
