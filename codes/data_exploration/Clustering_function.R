
##########################################################
##### Function for automated clustering of land cover data 
##########################################################

### Goal and function description
#================================

# This function works with the land cover data set assembled by Dr Lisieux Fuzessy which incorporates the land cover data
# for all routes of the North American Breeding Bird Survey in the contiguous USA. Each route is organized in 5 segments 
# and the land cover data extracted for a 500 m buffer surrounding each segment.

# The goal of this function is to automatically subset all segments of ecoregions with > 10 years of data availability.
# Then for selected ecoregion(s), and year(s)


#================================

rm(list=ls())

load("data/Lica/BBS_land_years.rda")
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

land <- BBS_land_years; rm(BBS_land_years)

land_merged <- merge(land, ecoreg, by = "partition")
land_merged$urban <- land_merged$urban.low + land_merged$urban.high

length(unique(ecoreg$Ecoregion)) # 83 ecoregions
length(unique(land$year)) # 8 years
length(unique(land$partition)) # 1923 partitions


##======

library(ggplot2); library(dplyr); library(viridis)

summary <- land_merged %>%
  group_by(Ecoregion, year) %>%
  summarise(count = length(partition))

ggplot(summary, aes(x = year, y = count, color = Ecoregion)) +
  geom_point() +
  scale_fill_viridis() +
  guides(color=F)


##================================

library(factoextra); library(tidyr)

# try out clustering for each year separately
land_years <- list() # prepare list to store subgroups
years <- sort(unique(land_merged$year)) # get all years

land_merged_sub <- land_merged %>%
  group_by(Ecoregion, year) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  group_by(Ecoregion) %>%
  filter(all(table(year) >= 10)) %>%
  ungroup() # create merged data set of Ecoregions and Land cover data

land_merged_sub[] <- lapply(land_merged_sub, function(x) if(is.factor(x)) factor(x) else x) # create factors

land_merged_sub <- arrange(land_merged_sub, year) # order the years

land_years <- land_merged_sub %>%
  group_nest(year)

###===== 

#start by trying to cluster the land cover of the segments for 2001

land2001 <- land_years %>%
  subset(year == 2001) %>%
  unnest(data)  # extract 2001 data

hab2001 <- land2001[,c(6:11,15)] # extract habitat 

hab2001_norm <- scale(hab2001[,], center=T, scale=T) # normalize: center and scale
rownames(hab2001_norm) <- land2001$partition # get all partition names and set as rownames

hab2001_dist <- dist(hab2001_norm) # create distance matrix

fviz_dist(hab2001_dist, show_labels = FALSE)+
  labs(title = "hab_2001") #using visual inspection the data can be seen clustering

pairs(hab2001_norm) #look at pair plot

### K means clustering of 2001 land cover data
set.seed(123)

fviz_nbclust(hab2001_norm, kmeans, method = "wss")+ theme_classic() # WCSS method shows best solution is 7 clusters

k_hab2001 <- kmeans(hab2001_norm, centers = 7, nstart = 25)

fviz_cluster(k_hab2001, data = hab2001_norm) # The data is strongly overlapping, which causes problems for K means clustering.
# This means we have to look for another clustering method.

str(k_hab2001) # The WCSS is also extremely high, which indicates incorrect clustering

### Silhoutte width clustering of 2001 land cover data

library(NbClust); library(cluster)

fviz_nbclust(hab2001_norm, pam, method = "silhouette")+ theme_classic()
# the Silhouette Width method:  6 clusters explain the data best

pam.res_hab2001 <- pam(hab2001_norm, 6,  metric = "euclidean", stand = FALSE)
fviz_silhouette(pam.res_hab2001, palette = "jco", ggtheme = theme_classic())
# Average Silhoutte width is 0.48, the closer the value to 1, the better the clusters
# Some data may not be clustering correctly, as seen for data below the 0 line.
# This is especially the case in cluser 1 and 5.

### hierarchical clustering 

library(ggdendro)

hclust_2001 <- hclust(hab2001_dist) # hierarchical clustering

dendrogram_2001 <-ggdendrogram(hclust_2001, rotate = FALSE, size = 2) # create dendrogram and plot
dendrogram_2001

cutoff_values <- 2:20 # set cut off values

assign_clusters <- function(hc, cutoff) {
  cutree(hc, h = cutoff)
}

cluster_labels_dendro <- lapply(cutoff_values, function(cutoff) assign_clusters(hclust_2001, cutoff)) # assign cluster labels in range 2:20

fviz_dend(hclust_2001, k = 20, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = F) # show dendrogram with 20 clusters

### density based clustering using DBSCAN
library(fpc)

epsilon <- 0.5

db_clust <- dbscan(hab2001_norm, eps = epsilon) 

fviz_cluster(db_clust, hab2001_norm, geom="point") # visualize data
# 7 clusters detecter, strongly overlapping, several outliers
# probably need to subset data more to cluster them

### Model based clustering using Gaussian Mixture Models
library(mclust)

GMM_clust <- Mclust(hab2001_norm)
plot(GMM_clust, what = c("classification"))

cs = cluster.stats(hab2001_dist, GMM_clust$classification)
cs[c("within.cluster.ss","avg.silwidth")]

##================================

# select land cover data for 3 Ecoregion subsets: Middle atlantic coastal plain, piedmont, Erie drift plain

n_ecoregs <- land_merged_sub %>%
  group_by(Ecoregion) %>%
  nest()

selected_ecoregs_vec <- c("Middle_Atlantic_Coastal_Plain", "Piedmont", "Erie_Drift_Plain")

selected_ecoregs <- n_ecoregs %>%
  subset(Ecoregion == selected_ecoregs_vec) %>%
  unnest(data)  # extract 3 ecoregions

MACP <- n_ecoregs %>%
  subset(Ecoregion == selected_ecoregs_vec[1]) %>%
  unnest(data)

selected_ecoregs <- bind_rows(MACP, selected_ecoregs)

selected_ecoregs_hab <- selected_ecoregs %>% select(-c(route, FID, Kilometers, urban.low, urban.high))

MACP <- selected_ecoregs_hab %>% filter(Ecoregion == selected_ecoregs_vec[1])
MACP_nest <- MACP %>%
  group_by(year) %>%
  nest()

Piedmont <- selected_ecoregs_hab %>% filter(Ecoregion == selected_ecoregs_vec[2])

Piedmont_nest <- Piedmont %>%
  group_by(year) %>%
  nest()

EDP <- selected_ecoregs_hab %>% filter(Ecoregion == selected_ecoregs_vec[3])
EDP_nest <- EDP %>%
  group_by(year) %>%
  nest()

##========================================

## investigate the Piedmont data and cluster all years

Piedmont2001 <- Piedmont_nest %>%
  subset(year == 2001) %>%
  unnest(data)

hab_Piedmont2001 <- Piedmont2001[,-c(1:3)]
rownames(hab_Piedmont2001) <- Piedmont2001$partition

pairs(hab_Piedmont2001) #look at data

# normalize (scale and center) data for subsequent cluster analysis
normalized_hab_Piedmont2001 <- scale(hab_Piedmont2001, center=T, scale=T)

# look at clustering tendency using hopkins test

res <- get_clust_tendency(normalized_hab_Piedmont2001, n = nrow(normalized_hab_Piedmont2001)-1, graph = FALSE)
res$hopkins_stat # data is clusterable, Hopkins >0.5


fviz_dist(dist(normalized_hab_Piedmont2001), show_labels = FALSE)+
  labs(title = "hab_Piedmont_2001")
# visual inspection of distance plot also shows tendencies to cluster

##=========================================

# find number of clusters using 3 methods
set.seed(123)

### K Means
fviz_nbclust(normalized_hab_Piedmont2001, kmeans, method = "wss")+ theme_classic() # WCSS method shows 2-3 clusters optimal
k_res <- kmeans(normalized_hab_Piedmont2001, centers = 2, nstart = 25)
fviz_cluster(list(data = normalized_hab_Piedmont2001, cluster = k_res$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
str(k_res)

### Gaussian Model 

GMM_clust_Pied2001 <- Mclust(normalized_hab_Piedmont2001)
plot(GMM_clust_Pied2001, what = c("classification"))
plot(GMM_clust_Pied2001, "density")

cs_Pied2001 = cluster.stats(dist(normalized_hab_Piedmont2001), GMM_clust_Pied2001$classification)
str(cs_Pied2001)

#Silhoutte width method
fviz_nbclust(normalized_hab_Piedmont2001, pam, method = "silhouette")+ theme_classic()
# the Silhouette Width shows that  7 clusters explain the data best

fviz_nbclust(normalized_hab_Piedmont2001, pam, method = "gap_stat")
# ideally choose cluster number with highest gap statistic
# in real world choose cluster number where gap statistic start to level off: around 6-7

clusternum <- NbClust((normalized_hab_Piedmont2001), distance="euclidean", method="kmeans")
# majority vote is 2 clusters, doesn't seem to apply to this data set?

pam.res3 <- pam(normalized_hab_Piedmont2001, 7,  metric = "euclidean", stand = FALSE)

??pam

fviz_silhouette(pam.res3, palette = "jco", ggtheme = theme_classic())

data_clustered3 <- cbind(normalized_hab_Piedmont2001, pam.res3$clustering)
aggregate(data_clustered3[,1:7], by = list(data_clustered3[,8]), mean)
pairs(normalized_hab_Piedmont2001[,1:7], col = pam.res3$clustering)


##===========================================

library(clValid)

cl_valid_pied <- clValid(obj = normalized_hab_Piedmont2001, nClust = 2:5,
                         clMethods = c("hierarchical", "kmeans"),
                         metric = "euclidean")

summary(cl_valid_pied)
optimalScores(cl_valid_pied)
plot(cl_valid_pied)

#### hierarchical clustering
#euclidean distance
hc_pied2001_a <- eclust(normalized_hab_Piedmont2001, "hclust", k=2, hc_metric = "euclidean", 
                      hc_method = "ward.D2", graph = FALSE)

fviz_dend(hc_pied2001_a, k_colors = "jco", show_labels = T, as.ggplot = TRUE)

fviz_cluster(list(data = normalized_hab_Piedmont2001, cluster = hc_pied2001_a$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

hc_pied2001_a2 <- eclust(normalized_hab_Piedmont2001, "hclust", k=4, hc_metric = "euclidean", 
                        hc_method = "ward.D2", graph = FALSE)

fviz_dend(hc_pied2001_a2, k_colors = "jco", show_labels = T, as.ggplot = TRUE)

fviz_cluster(list(data = normalized_hab_Piedmont2001, cluster = hc_pied2001_a2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#spearman correlation
hc_pied2001_b <- eclust(normalized_hab_Piedmont2001, "hclust", k=3, hc_metric = "spearman", 
                      hc_method = "ward.D2", graph = FALSE)

fviz_dend(hc_pied2001_b, k_colors = "jco", show_labels = T, as.ggplot = TRUE)

fviz_cluster(list(data = normalized_hab_Piedmont2001, cluster = hc_pied2001_b$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#pearson correlation
hc_pied2001_c <- eclust(normalized_hab_Piedmont2001, "hclust", k = 3, hc_metric = "pearson", 
                       hc_method = "ward.D2", graph = FALSE)

fviz_dend(hc_pied2001_c, k_colors = "jco", show_labels = T, as.ggplot = TRUE)

fviz_cluster(list(data = normalized_hab_Piedmont2001, cluster = hc_pied2001_c$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

##=============================================

# ecoregion_habitat_cluster <- function(df, ecoreg, years){
#   set.seed(123)
# }
