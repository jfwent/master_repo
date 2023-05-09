#==========================#
#   BBS ANALYSES EXAMPLE   #
#==========================#

## LIBRARIES

library(brms)


## DATASETS

setwd("/Users/daniel/Documents/Research/BBS project/Data & Analyses/Data")

bbs <- read.table("bbs.filt.AOU.txt", h=T)
part <- read.table("BBS.partition.abundance.txt", h=T)
land <- read.table("BBS.land.years.txt", h=T)
bioreg <- read.table("US_ecoregions.txt", h=T)


## EXAMPLE CODE 

### GOAL: To test how land use change affects changes in species abundance

# We focus on changes in 2001 and 2019 
# Surveys will then be:
  # 2000-2001-2002 for the first period (P1)
  # 2015-2016-2017 for the second period (P2)


## EXAMPLE WITH Spizella passerina

df <- subset(part,part$animal_jetz=="Spizella_passerina")
View(df)
names(df)
df[1:30,c(1,10,11)]


## PREPARE SURVEY DATA

# We take only the partitions where the species was present in P1 (i.e. it was present in the three years)
# and estimate the average of seg_abundance for the three years

# subset data for years 2000-2002
df_before <- subset(df, year %in% c(2000, 2001, 2002) & seg_abundance > 0)

# calculate average seg_abundance by partition for years 2000-2002
AbundBefore <- aggregate(seg_abundance ~ partition, df_before, sum)

# subset data for years 20015-2017, note that here we include zeros
df_after <- subset(df, year %in% c(2015, 2016, 2017) & seg_abundance)

# calculate average seg_abundance by partition for years 2007-2009
AbundAfter <- aggregate(seg_abundance ~ partition, df_after, sum)

# merge the two data.frames based on partition
df_merged <- merge(AbundBefore, AbundAfter, by = "partition")

# rename the columns
names(df_merged) <- c("partition", "AbundBefore", "AbundAfter")

df_final <- df_merged


## PREPARE HABITAT DATA

df_before <- subset(land, year %in% c(2001))
names(df_before) <- c("year","partition","route","urban.low_before","urban.high_before","forest_before","grass_before","pasture_before","crop_before","wet_before","barren_before")
df_before <- df_before[,-1]

df_after<- subset(land, year %in% c(2016))
names(df_after) <- c("year","partition","route","urban.low_after","urban.high_after","forest_after","grass_after","pasture_after","crop_after","wet_after","barren_after")
df_after <- df_after[,-1]

if(all(df_after$partition == df_before$partition)) {
  print("ok")
} else {
  print("different")
}

df_merged <- cbind(df_before, df_after[,-c(1,2)])
names(df_merged)


## MERGE SURVEYS AND HABITAT

df_final <- merge(df_final, df_merged, by="partition")
df_final <- merge(df_final, bioreg, by="partition")
View(df_final)


## START MODEL

# intensity of anthropogenic alterations
# positive values mean that the altered area has increased

df_final$alterations <- (df_final$pasture_after+df_final$crop_after+df_final$urban.high_after+df_final$urban.low_after) - (df_final$pasture_before+df_final$crop_before+ df_final$urban.high_before+df_final$urban.low_before) 


# model changes in abundance after urbanization
model <- brm(AbundAfter ~ AbundBefore + alterations + (1|Ecoregion) + (1|route),
             family = negbinomial(),  control = list(adapt_delta = 0.9999, max_treedepth = 12),
             cores = 6,  # cores cannot be higher than chains
             warmup=1000, iter=2000, chains=1,
             data = df_final)
summary(model)  # the coefficient associated with "alterations" in the model represents the estimated change in abundance after the habitat alteration, per unit increase in the intensity of habitat alteration, while holding other predictors constant.
pp_check(model) # we can compare the model fit, for instance graphically via posterior predictive checks.

p <- plot(conditional_effects(model, surface = TRUE)) #The corresponding plot method returns a named list of ggplot objects, which can be further customized using the ggplot2 package

ce1 <- conditional_effects(model, 
                          pred = alterations,  # specifies the predictor variable
                          predictor = "alterations", # specifies the name of the predictor variable
                          surface = TRUE, # specifies that we want to plot the predicted surface
                          predictor_range = list(alterations = c(min(df_final$alterations), max(df_final$alterations)))) # specifies the range of the predictor variable to be plotted
plot(ce1)

ce2 <- conditional_effects(model, 
                          pred = AbundBefore,  # specifies the predictor variable
                          predictor = "AbundBefore", # specifies the name of the predictor variable
                          surface = TRUE, # specifies that we want to plot the predicted surface
                          predictor_range = list(AbundBefore = c(min(df_final$AbundBefore), max(df_final$AbundBefore)))) # specifies the range of the predictor variable to be plotted
plot(ce2)


