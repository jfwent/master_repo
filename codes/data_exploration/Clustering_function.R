
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

load("data/Lica/BBS_land_years.rda")
ecoreg <- read.delim('data/Lica/US_ecoregions.txt', sep = "")

land <- bbs_land_years; rm(bbs_land_years)

land_merged <- merge(land, ecoreg, by = "partition")
land_merged$urban <- land_merged$urban.low + land_merged$urban.high

library(dplyr)

land_merged_sub <- land_merged %>%
  group_by(Ecoregion, year) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  group_by(Ecoregion) %>%
  filter(all(table(year) >= 10)) %>%
  ungroup()
land_merged_sub[] <- lapply(land_merged_sub, function(x) if(is.factor(x)) factor(x) else x)

ecoregs <- unique(ecoreg$Ecoregion)

Piedmont <- land_merged_sub %>%   #you change the ecoregion by search and replace
  subset(Ecoregion == "Piedmont")

length(table(Piedmont$partition))

df_long <- Piedmont[,c(1,2,6:11,15)] %>%
  gather(key = "habitat", value = "percentage", -c(partition, year))


ecoregion_name <- "Piedmont"
Piedmont <- land2sub %>% 
  subset(Ecoregion == ecoregion_name)
Piedmont2001 <- as.data.frame(subset(Piedmont, year == "2001"))

hab_Piedmont2001 <- Piedmont2001[,c(6:11,15)]
rownames(hab_Piedmont2001) <- Piedmont2001$partition

wss <- (nrow(hab_Piedmont2001)-1)*sum(apply(hab_Piedmont2001,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(hab_Piedmont2001, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
k_opt <- which.min(wss)

dist_mat_hab_peidmont_2001 <- dist(hab_Piedmont2001)
hierarchical_cluster_piedmont <- hclust(dist_mat_hab_peidmont_2001)


library(cluster)
set.seed(123)
items <- c("bread", "milk", "eggs", "fruit", "vegetables", "cheese")
customers <- paste0("customer_", 1:20)
purchases <- matrix(sample(c(0,1), 120, replace=TRUE), ncol=length(items), dimnames=list(customers, items))
dist_mat <- dist(purchases)
hc <- hclust(dist_mat)

ggplot(as.dendrogram(hc), labels=TRUE, theme_minimal()) +
  labs(title="Hierarchical Clustering of Customer Purchases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("Distance") +
  scale_x_continuous(expand=c(0,0)) +
  geom_hline(yintercept=0.5, linetype="dashed", color="red") +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=1) +
  coord_flip()



ecoregion_habitat_cluster <- function(df, ecoreg, years){
  set.seed(123)
  
}
