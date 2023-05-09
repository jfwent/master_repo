###### land use change data exploration
######

##set up
rm(list=ls())

# install.packages('funrar')
# install.packages('ggplot2')
# install.packages('maps')
# install.packages('mapdata')
# install.packages('rgdal')
# install.packages('tmap')
library('funrar')
library('ggplot2')
library('maps')
library('mapdata')
library('rgdal')
library('tmap')
library('sf')
library('dplyr')
library('mapview')

#read in data
landuse <- read.delim('bbs.land.years.txt', sep = "")
ecoregions <- read.delim('US_ecoregions.txt', sep = "")

#clean land-use data --> all segments with designated land-cover use percentage < 60% are discarded
landuse$percentage <- rowSums(landuse[, 4:11])
landuse_subset <- landuse[landuse$percentage >= 60, ] # 154 entries don't fulfill 60% landcover characterization

unique_segments <- unique(landuse_subset$partition) # get unique segments
years <- unique(landuse_subset$year) #we have data on the years: 2001 2004 2006 2008 2011 2013 2016 2019
years_diff <- c()
for(y in 1:7){
  years_diff <- append(years_diff,paste0(years[y],"_",years[y+1]))
}
years <- years_diff
rm(years_diff)

land_cover_types <- names(landuse_subset[4:11]) #get vector of all land use types

#calculate areas with big changes in land use
#calculate difference in each land use category for each segment between all the years and then show trends?

ecoregions_subset <- ecoregions[ecoregions$partition %in% unique_segments,] #get ecoregions for all segments we keep

land_use_change <- list() # empty list to store results

for(i in seq_along(unique_segments)){ #create loop to calculate the change in each landcover type between years for each segment

  tmp <- landuse_subset[which(landuse_subset$partition == unique_segments[i]),]
  
  if(nrow(tmp) == 8 & ncol(tmp)==12){ #only apply if sufficient data (all years)
    diff_df <- data.frame(matrix(NA, nrow=nrow(tmp)-1, ncol=ncol(tmp)-4))
    colnames(diff_df) <- names(tmp[4:11])
    
    for (h in 2:nrow(tmp)) {
      diff_df[h - 1,] <- tmp[h, 4:11] - tmp[h - 1, 4:11]
    }
    #diff_df$ecoregion <- ecoregions_subset$Ecoregion[i]
    diff_df$segment <- unique_segments[i]
    diff_df$years <- years
    land_use_change[[i]] <- diff_df
    #names(land_use_change)[i] <- unique_segments[i]
  }
}
rm(tmp, diff_df, h, y)

land_use_change <- bind_rows(land_use_change) #unfold into dataframe
names(land_use_change)[9] <- "partition"

land_use_2001_2004 <- subset(land_use_change, years == "2001_2004")
land_use_2004_2006 <- subset(land_use_change, years == "2004_2006")
land_use_2006_2008 <- subset(land_use_change, years == "2006_2008")
land_use_2008_2011 <- subset(land_use_change, years == "2008_2011")
land_use_2011_2013 <- subset(land_use_change, years == "2011_2013")
land_use_2013_2016 <- subset(land_use_change, years == "2013_2016")
land_use_2016_2019 <- subset(land_use_change, years == "2016_2019")

#summarize for what? counties? states? ecoregions?
routes <- readOGR("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp")
ecoregions_shp <- readOGR("/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/us_eco_l3/us_eco_l3.shp")

common_ids <- intersect(routes$partition, land_use_change$partition)

# subset the shapefile to keep only the features with matching IDs
routes_subset <- subset(routes, partition %in% common_ids)
rm(routes)

ecoMap <- mapview(ecoregions_shp, zcol = "NA_L3NAME", legend=F)
ecoMap

land_use_2001_2004_shp <- merge(routes_subset, land_use_2001_2004, by = "partition")

map2001_2004 <- mapview(land_use_2001_2004_shp, zcol = "forest", legend=F)

map2001_2004

load(file.path('/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/USBBS_Biodiversity_LandCover_Delays-master/USBBS_data/landscape_data/landcover_data/landcover_%_&_delta_extracted',
                                  file='lc_centroid_6000.rda'))
map_haddou <- mapview(lc)
map_haddou

#calculate land cover diversity? land cover richness?


#map results on north america



#map the segments


