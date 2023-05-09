### BBS data exploration


#set up
rm(list=ls())
install.packages("plotly")
library(viridis)
library(dplyr)
library(sf)
library(poorman)
library(leaflet.extras2)
library(mapshot)
library(ggplot2)
library(hrbrthemes)
# library(streamgraph)
library(plotly)

###get data
#landuse
bbs_landuse <- read.delim('bbs.land.years.txt', sep = "")
years <- unique(bbs_landuse$year)
bbs_landuse$percentage <- rowSums(bbs_landuse[, 4:11])
bbs_landuse_subset <- bbs_landuse[bbs_landuse$percentage >= 60, ] # 154 entries don't fulfill 60% landcover characterization
unique_segments <- unique(bbs_landuse_subset$partition) # get unique segments
US_ecoregions <- read.delim('US_ecoregions.txt', sep="")
US_ecoregions <- US_ecoregions[US_ecoregions$partition %in% unique_segments,]

#abundance
bbs_abundance <- read.delim('BBS.partition.abundance.txt', sep = "")
bbs_filt_aou <- read.delim('bbs.filt.AOU.txt', sep = "")
AOU_jetz <- read.delim('AOU.filt.jetz.txt', sep = "")


#haddou et al., 2022 data
load(file.path('/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/USBBS_Biodiversity_LandCover_Delays-master/USBBS_data/diversity_data/2years_x_timepoint',
               file='USBBS_years_subset.rda'))

sp_matrix_max <- load(file.path('/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/USBBS_Biodiversity_LandCover_Delays-master/USBBS_data/diversity_data/2years_x_timepoint',
     file='vegan_spmatrix_t1_max.rda'))

sp_matrix_mean <- load(file.path('/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/USBBS_Biodiversity_LandCover_Delays-master/USBBS_data/diversity_data/2years_x_timepoint',
               file='vegan_spmatrix_t1_mean.rda'))

sp_matrix_min <- load(file.path('/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/USBBS_Biodiversity_LandCover_Delays-master/USBBS_data/diversity_data/2years_x_timepoint',
               file='vegan_spmatrix_t1_min.rda'))

###diversity indices

#clean bird data
#exclude species hybrids, nocturnal and aquatic birds, 
#to reduce stochastity similar to Davis et al. (2023): sum route and species level count over 3 years,
#minimal detection amount? in Davis et al. (2023) min 100 detections across all routes and more than 10 routes


#abundance

#explore trends in three exemplary species representing the life-history (fast-slow) strategy spectrum
#excluding seabirds and waterfowl and introduced species, restricting to Passeriformes
#American crow, corvus brachyrhynchos as the slow lived extreme
#Carolina wren, Thryothorus ludovicianus as the medium
  #other option: Eastern bluebird, Sialia sialis, as the medium or American robin, Turdus migratorius
#Common grackle, Quiscalus quiscula, as the short lived extreme

#check if american crow is in the data set
american_crow_check <- c("Corvus_brachyrhynchos", "Passeriformes", "Corvidae", "Corvus")
american_crow_check[1] %in% bbs_abundance$animal

carolina_wren_check <- c("Thryothorus_ludovicianus", "Passeriformes", "Troglodytidae", "Thryothorus")
carolina_wren_check[1] %in% bbs_abundance$animal

common_grackle_check <- c("Quiscalus_quiscula", "Passeriformes", "Icteridae", "Quiscalus")
common_grackle_check[1] %in% bbs_abundance$animal

#extract the american crow data
c_brachyrhynchos <- bbs_abundance[bbs_abundance$animal == "Corvus_brachyrhynchos",]
head(c_brachyrhynchos)
c_brachyrhynchos_subset <- c_brachyrhynchos[c_brachyrhynchos$year %in% years,]
c_brachyrhynchos_subset <- c_brachyrhynchos_subset[c_brachyrhynchos_subset$partition %in% unique_segments,]
c_brachyrhynchos_2001 <- c_brachyrhynchos_subset[c_brachyrhynchos_subset$year == 2001,]
c_brachyrhynchos_2019 <- c_brachyrhynchos_subset[c_brachyrhynchos_subset$year == 2019,]

t_ludovicianus <- bbs_abundance[bbs_abundance$animal == "Thryothorus_ludovicianus",]
head(t_ludovicianus)
t_ludovicianus_subset <- t_ludovicianus[t_ludovicianus$year %in% years,]
t_ludovicianus_subset <- t_ludovicianus_subset[t_ludovicianus_subset$partition %in% unique_segments,]
t_ludovicianus_2001 <- t_ludovicianus_subset[t_ludovicianus_subset$year == 2001,]
t_ludovicianus_2019 <- t_ludovicianus_subset[t_ludovicianus_subset$year == 2019,]

q_quiscula <- bbs_abundance[bbs_abundance$animal == "Quiscalus_quiscula",]
head(q_quiscula)
q_quiscula_subset <- q_quiscula[q_quiscula$year %in% years,]
q_quiscula_subset <- q_quiscula_subset[q_quiscula_subset$partition %in% unique_segments,]
q_quiscula_2001 <- q_quiscula_subset[q_quiscula_subset$year == 2001,]
q_quiscula_2019 <- q_quiscula_subset[q_quiscula_subset$year == 2019,]

#how do I define a population? In what scales do I investigate trends?

#maps
routes <- readOGR("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp")
ecoregions_shp <- readOGR("/Users/jonwent/Desktop/ETHZ/Master Thesis/R/Data/us_eco_l3/us_eco_l3.shp")
common_ids <- intersect(routes$partition, unique_segments)
routes_subset <- subset(routes, partition %in% common_ids)

ecoMap <- mapview(ecoregions_shp, zcol = "NA_L3NAME", legend=F, alpha=0.1)
ecoMap

#merge data sets

c_brachyrhynchos_2001_shp <- merge(routes_subset, c_brachyrhynchos_2001, by = "partition")
c_brachyrhynchos_2019_shp <- merge(routes_subset, c_brachyrhynchos_2019, by = "partition")
#check non-unique entries in c_brachyrhynchos_2001
sum(duplicated(c_brachyrhynchos_2001$partition) == T) #31 non-unique entries --> 62 duplicates in column partition
sum(duplicated(c_brachyrhynchos_2019$partition) == T) #33 non unique entries --> 66 duplicates

# duplicated.partitions <- duplicated(c_brachyrhynchos_2001$partition) | duplicated(c_brachyrhynchos_2001$partition, fromLast = TRUE)
# duplicated.data <- c_brachyrhynchos_2001[duplicated.partitions,]

c_brachyrhynchos_2001_dup <- c_brachyrhynchos_2001 %>%
  group_by(partition) %>%
  summarize(seg_abundance_2 = sum(seg_abundance))

c_brachyrhynchos_2019_dup <- c_brachyrhynchos_2019 %>%
  group_by(partition) %>%
  summarize(seg_abundance_2 = sum(seg_abundance))

sum(duplicated(c_brachyrhynchos_2001_dup$partition) == T) # check again for duplicates, non detected
sum(duplicated(c_brachyrhynchos_2019_dup$partition) == T) # check again for duplicates, non detected

c_brachyrhynchos_2001_dup_shp_eco <- merge(c_brachyrhynchos_2001_dup_shp, US_ecoregions, by="partition")
c_brachyrhynchos_2001_dup_shp <- merge(routes_subset, c_brachyrhynchos_2001_dup, by="partition")
c_brachyrhynchos_2019_dup <- merge(c_brachyrhynchos_2019_dup, US_ecoregions, by="partition")
c_brachyrhynchos_2019_dup_shp <- merge(routes_subset, c_brachyrhynchos_2019_dup, by="partition")

t_ludovicianus_2001_shp <- merge(routes_subset, t_ludovicianus_2001, by = "partition")
t_ludovicianus_2001_shp <- merge(t_ludovicianus_2001_shp, US_ecoregions, by="partition")
t_ludovicianus_2019_shp <- merge(routes_subset, t_ludovicianus_2019, by="partition")
t_ludovicianus_2019_shp <- merge(t_ludovicianus_2019_shp, US_ecoregions, by="partition")

q_quiscula_2001_shp <- merge(routes_subset, q_quiscula_2001, by="partition")
q_quiscula_2001_shp <- merge(q_quiscula_2001_shp, US_ecoregions, by="partition")
q_quiscula_2019_shp <- merge(routes_subset, q_quiscula_2019, by="partition")
q_quiscula_2019_shp <- merge(q_quiscula_2019_shp, US_ecoregions, by="partition")

#map the 2001 populations for the three species, color according to Ecoregion and size according to abundance
t_ludo_2001_map <- mapview(t_ludovicianus_2001_shp, lwd = "seg_abundance", alpha = 0.5, zcol="Ecoregion", legend=F)
t_ludo_2001_map # mainly present in the eastern and south-eastern US
t_ludo_2019_map <- mapview(t_ludovicianus_2019_shp, lwd = "seg_abundance", alpha = 0.5, zcol="Ecoregion", legend=F)
t_ludo_leaflet <- t_ludo_2001_map | t_ludo_2019_map

c_brachyrhynchos_2001_eco_map <- mapview(c_brachyrhynchos_2001_dup_shp_eco, 
                                         lwd="seg_abundance_2", alpha=0.5, zcol="Ecoregion", legend=F)
c_brachyrhynchos_2001_eco_map #present thoughout eastern US and west coast

c_brachyrhynchos_2019_map <- mapview(c_brachyrhynchos_2019_dup_shp, lwd="seg_abundance_2", alpha=0.5, zcol="Ecoregion", legend=F)
c_brachyrhynchos_2019_map

c_brachyrhynchos_leaflet <- c_brachyrhynchos_2001_eco_map | c_brachyrhynchos_2019_map

q_quiscula_2001_map <- mapview(q_quiscula_2001_shp, lwd = "seg_abundance", alpha = 0.6,
                               zcol="Ecoregion", legend=F)
q_quiscula_2001_map #present in eastern US and north-eastern US

q_quiscula_2019_map <- mapview(q_quiscula_2019_shp, lwd = "seg_abundance", alpha = 0.6,
                               zcol="Ecoregion", legend=F)
q_quiscula_2019_map #present in eastern US and north-eastern US
q_quiscula_leaflet <- q_quiscula_2001_map | q_quiscula_2019_map


mapshot(q_quiscula_leaflet, url=paste0(getwd(), "/q_quiscula_comparison.html"))
mapshot(c_brachyrhynchos_leaflet, url=paste0(getwd(), "/c_brachyrhynchos_comparison.html"))
mapshot(t_ludo_leaflet, url=paste0(getwd(), "/t_ludovicianus_comparison.html"))

# plot timeseries for ecoregions

unique(US_ecoregions$Ecoregion) # 83 Ecoregions

#Corvus brachyrhynchos

t_ludo_subset <- merge(t_ludovicianus_subset, US_ecoregions, by="partition")
t_ludo_summed <- t_ludo_subset %>%
  group_by(year, Ecoregion) %>%
  summarize(abundance = sum(seg_abundance))

q_quiscu_subset <- merge(q_quiscula_subset, US_ecoregions, by="partition")
q_quiscu_summed <- q_quiscu_subset %>%
  group_by(year, Ecoregion) %>%
  summarize(abundance = sum(seg_abundance))

c_brachy_subset <- merge(c_brachyrhynchos_subset, US_ecoregions, by="partition")
c_brachy_summed <- c_brachy_subset %>%
  group_by(year, Ecoregion) %>%
  summarize(abundance=sum(seg_abundance))

c_brachy_plot <- c_brachy_summed %>%
  ggplot(aes(x=year, y=abundance, fill=Ecoregion, name=Ecoregion)) +
    geom_area( ) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="none") +
    ggtitle("Abundance C brachyrhynchos in US") +
    theme_ipsum() +
    theme(legend.position="none")
c_brachy_plot

t_ludo_plot <- t_ludo_summed %>%
  ggplot(aes(x=year, y=abundance, fill=Ecoregion, name=Ecoregion)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Abundance T ludovicianus in US") +
  theme_ipsum() +
  theme(legend.position="none")
t_ludo_plot

q_quiscu_plot <- q_quiscu_summed %>%
  ggplot(aes(x=year, y=abundance, fill=Ecoregion, name=Ecoregion)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Abundance Q quiscula in US") +
  theme_ipsum() +
  theme(legend.position="none")
q_quiscu_plot


#relative abundance
#one method described in Evans et al. 2019: adjust count data for detectability with unmarked package

#alpha-diversity

#beta-diversity

#functional diversity


###extinctions
#Find definitions for extinction --> absence for several years after 
#