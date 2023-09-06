### Species Distribution Models

setwd("C:/Users/salom/Dropbox/LandscapeModelling_Eagle/Data")
setwd("C:/Users/morit/Dropbox/LandscapeModelling_Eagle/Data")
#setwd("/Users/jonwent/Dropbox/LandscapeModelling_Eagle/Data")


#load libraries
library(sp)
library(terra)
library(raster)
library(RColorBrewer)
library(gam)
library(pander)
library(gstat)
library(rgdal)
library(rgeos)
library(tree)
library(ecospat)
library(dismo)
library(PresenceAbsence)
library(AUC)
library(randomForest)
#library(gurobi)
#library(prioritizr)
library(dismo)

#########################################
########## Data Preparation #############
#########################################

#set extent to be used to crop data
ext <- extent(-85,-60,-30,15)

#----Temperatures----

## Mean annual air temperature
mean.ann.air <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
plot(mean.ann.air)
mean.ann.air.crop <- crop(mean.ann.air, ext, filename = "bio1_1981-2010_MeanAnnualAirTemperatureCropped.tif")

#check if results are satisfying
plot(mean.ann.air.crop)

#check resolution of dataset
res(mean.ann.air)
#'all Chelsa datasets have a resolution of 0.0083333° x 0.0083333°, which translates to 1km x 1 km

## Mean daily maximum air temperature of the warmest month
max.temp.wm <- raster("CHELSA_bio5_1981-2010_V.2.1.tif")
plot(max.temp.wm)
max.temp.wm.crop <- crop(max.temp.wm, ext, filename = "bio5_1981-2010_MeanDailyMaximumAirTemperatureWarmestMonth.tif")

## Mean daily minimum air temperature of the coldest month
min.temp.cm <- raster("CHELSA_bio6_1981-2010_V.2.1.tif")
plot(min.temp.cm)
min.temp.cm.crop <- crop(min.temp.cm, ext, filename = "bio6_1981-2010_MeanDailyMinimumAirTemperatureColdestMonth.tif")

#----Precipitation----

##Annual Precipitation Amount
ann.prec <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")
plot(ann.prec)
ann.prec.crop <- crop(ann.prec, ext, filename = "bio12_1981-2010_AnnualPrecipitatonAmount.tif")

##Precipitation Amount of the Wettest Month
prec.wm <- raster("CHELSA_bio13_1981-2010_V.2.1.tif")
plot(prec.wm)
prec.wm.crop <- crop(prec.wm, ext, filename = "bio13_1981-2010_PrecipitationWettestMonth.tif")

##Precipitation Amount of the Driest Month
prec.dm <- raster("CHELSA_bio14_1981-2010_V.2.1.tif")
plot(prec.dm)
prec.dm.crop <- crop(prec.dm, ext, filename = "bio14_1981-2010_PrecipitationDriestMonth.tif")


#----Growing Season----
##Length of the Growing Season
len.grow <- raster("CHELSA_gsl_1981-2010_V.2.1.tif")
plot(len.grow)
len.grow.crop <- crop(len.grow, ext, filename = "gsl_1981-2010_LengthOfGrowingSeason.tif")


#----Snow Cover----
##Snow cover days
snow.cov <- raster("CHELSA_scd_1981-2010_V.2.1.tif")
plot(snow.cov)
snow.cov.crop <- crop(snow.cov, ext, filename = "scd_1981-2010_SnowCoverDays.tif")

#----Forest----

#Evergreen Needleleaf
evergreen.needleleaf <- raster("consensus_full_class_1.tif")
plot(evergreen.needleleaf)
#these do not show up in our study area, so let's ignore this one

#Evergreen Broadleaf
evergreen.broadleaf <- raster("consensus_full_class_2.tif")
plot(evergreen.broadleaf)
evergreen.broadleaf <- crop(evergreen.broadleaf, ext, filename = "evergreen_broadleaf.tif")
plot(evergreen.broadleaf.crop)

#Deciduous Broadleaf
deciduous.broadleaf <- raster("consensus_full_class_3.tif")
plot(deciduous.broadleaf)
#this one might also not be relevant, let's leave it out for now

#Mixed/Other Trees
mixed <- raster("consensus_full_class_4.tif")
plot(mixed)
mixed.forest <- crop(mixed, ext, filename = "mixed.tif")
plot(mixed.forest)

#----elevation----
elevation <- raster("TOPO_30_merged_SA.tif")
extent(elevation)
elevation.crop <- crop(elevation, ext, filename = 'elevation_SA_cropped.tif')
plot(elevation.crop)

#----Slope----
slope <- terrain(elevation.crop, opt="slope", unit="degrees", filename = "Slope.tif", overwrite = T)
plot(slope)
typeof(slope)


################################################################################

####Future data, GFDL model####

##SSP1-RCP2.6
#mean annual air temperature
gfdl.rcp26.mean.ann <- raster("CHELSA_bio1_2041-2070_gfdl-esm4_ssp126_V.2.1.tif")
gfdl.rcp26.mean.ann.crop <- crop(gfdl.rcp26.mean.ann, ext,filename = "gfdl_rcp26_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
gfdl.rcp26.prec.wm <- raster("CHELSA_bio13_2041-2070_gfdl-esm4_ssp126_V.2.1.tif")
gfdl.rcp26.prec.wm.crop <- crop(gfdl.rcp26.prec.wm, ext, filename = "gfdl_rcp26_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
gfdl.rcp26.grow <- raster("CHELSA_gsl_2041-2070_gfdl-esm4_ssp126_V.2.1.tif")
gfdl.rcp26.grow.crop <- crop(gfdl.rcp26.grow, ext, filename = "gfdl_rcp26_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
gfdl.rcp26.snow.cov <- raster("CHELSA_scd_2041-2070_gfdl-esm4_ssp126_V.2.1.tif")
gfdl.rcp26.snow.cov.crop <- crop(gfdl.rcp26.snow.cov, ext, filename = "gfdl_rcp26_scd_2041-2070_SnowCoverDays.tif")


##SSP3-RCP7.0
#mean annual air temperature
gfdl.rcp70.mean.ann <- raster("CHELSA_bio1_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")
gfdl.rcp70.mean.ann.crop <- crop(gfdl.rcp70.mean.ann, ext, filename = "gfdl_rcp70_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
gfdl.rcp70.prec.wm <- raster("CHELSA_bio13_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")
gfdl.rcp70.prec.wm.crop <- crop(gfdl.rcp70.prec.wm, ext, filename = "gfdl_rcp70_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
gfdl.rcp70.grow <- raster("CHELSA_gsl_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")
gfdl.rcp70.grow.crop <- crop(gfdl.rcp70.grow, ext, filename = "gfdl_rcp70_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
gfdl.rcp70.snow.cov <- raster("CHELSA_scd_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")
gfdl.rcp70.snow.cov.crop <- crop(gfdl.rcp70.snow.cov, ext, filename = "gfdl_rcp70_scd_2041-2070_SnowCoverDays.tif")


##SSP5-RCP8.5
#mean annual air tempreature
gfdl.rcp85.mean.ann <- raster("CHELSA_bio1_2041-2070_gfdl-esm4_ssp585_V.2.1.tif")
gfdl.rcp85.mean.ann.crop <- crop(gfdl.rcp85.mean.ann, ext, filename = "gfdl_rcp85_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
gfdl.rcp85.prec.wm <- raster("CHELSA_bio13_2041-2070_gfdl-esm4_ssp585_V.2.1.tif")
gfdl.rcp85.prec.wm.crop <- crop(gfdl.rcp85.prec.wm, ext, filename = "gfdl_rcp85_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
gfdl.rcp85.grow <- raster("CHELSA_gsl_2041-2070_gfdl-esm4_ssp585_V.2.1.tif")
gfdl.rcp85.grow.crop <- crop(gfdl.rcp85.grow, ext, filename = "gfdl_rcp85_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
gfdl.rcp85.snow.cov <- raster("CHELSA_scd_2041-2070_gfdl-esm4_ssp585_V.2.1.tif")
gfdl.rcp85.snow.cov.crop <- crop(gfdl.rcp85.snow.cov, ext, filename = "gfdl_rcp85_scd_2041-2070_SnowCoverDays.tif")


####Future data, MPI model####

##SSP1-RCP2.6
#mean annual air temperature
mpi.rcp26.mean.ann <- raster("CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif")
mpi.rcp26.mean.ann.crop <- crop(mpi.rcp26.mean.ann, ext,filename = "mpi_rcp26_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
mpi.rcp26.prec.wm <- raster("CHELSA_bio13_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif")
mpi.rcp26.prec.wm.crop <- crop(mpi.rcp26.prec.wm, ext, filename = "mpi_rcp26_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
mpi.rcp26.grow <- raster("CHELSA_gsl_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif")
mpi.rcp26.grow.crop <- crop(mpi.rcp26.grow, ext, filename = "mpi_rcp26_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
mpi.rcp26.snow.cov <- raster("CHELSA_scd_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif")
mpi.rcp26.snow.cov.crop <- crop(mpi.rcp26.snow.cov, ext, filename = "mpi_rcp26_scd_2041-2070_SnowCoverDays.tif")


##SSP3-RCP7.0
#mean annual air temperature
mpi.rcp70.mean.ann <- raster("CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp370_V.2.1.tif")
mpi.rcp70.mean.ann.crop <- crop(mpi.rcp70.mean.ann, ext, filename = "mpi_rcp70_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
mpi.rcp70.prec.wm <- raster("CHELSA_bio13_2041-2070_mpi-esm1-2-hr_ssp370_V.2.1.tif")
mpi.rcp70.prec.wm.crop <- crop(mpi.rcp70.prec.wm, ext, filename = "mpi_rcp70_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
mpi.rcp70.grow <- raster("CCHELSA_gsl_2041-2070_mpi-esm1-2-hr_ssp370_V.2.1.tif")
mpi.rcp70.grow.crop <- crop(mpi.rcp70.grow, ext, filename = "mpi_rcp70_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
mpi.rcp70.snow.cov <- raster("CHELSA_scd_2041-2070_mpi-esm1-2-hr_ssp370_V.2.1.tif")
mpi.rcp70.snow.cov.crop <- crop(mpi.rcp70.snow.cov, ext, filename = "mpi_rcp70_scd_2041-2070_SnowCoverDays.tif")


##SSP5-RCP8.5
#mean annual air temperature
mpi.rcp85.mean.ann <- raster("CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif")
mpi.rcp85.mean.ann.crop <- crop(mpi.rcp85.mean.ann, ext, filename = "mpi_rcp85_bio1_2041-2070_MeanAnnualAirTemperature.tif")

#precipitation during the wettest month
mpi.rcp85.prec.wm <- raster("CHELSA_bio13_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif")
mpi.rcp85.prec.wm.crop <- crop(mpi.rcp85.prec.wm, ext, filename = "mpi_rcp85_bio13_2041-2070_PrecipitationWettestMonth.tif")

#length of the growing season
mpi.rcp85.grow <- raster("CHELSA_gsl_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif")
mpi.rcp85.grow.crop <- crop(mpi.rcp85.grow, ext, filename = "mpi_rcp85_gsl_2041-2070_LengthOfGrowingSeason.tif")

#snow cover
mpi.rcp85.snow.cov <- raster("CHELSA_scd_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif")
mpi.rcp85.snow.cov.crop <- crop(mpi.rcp85.snow.cov, ext, filename = "mpi_rcp85_scd_2041-2070_SnowCoverDays.tif")


###Human Population Density
hum.pop <- raster("baseYr_total_2000.tif")
plot(hum.pop)

hum.rur <- raster("baseYr_rural_2000.tif")
plot(hum.rur)

hum.urb <- raster("baseYr_urban_2000.tif")
plot(hum.urb)

hum.pop.crop <- crop(hum.pop, ext, filename = "human_pop_density_2000.tif")
plot(hum.pop.crop)
summary(hum.pop.crop)

###Future Human Population Density
#2040
hum.pop.40 <- raster("ssp2_total_2040.tif")
hum.pop.40.crop <- crop(hum.pop.40, ext, filename = "human_pop_density_2040.tif")

#2070
hum.pop.70 <- raster("ssp2_total_2070.tif")
hum.pop.70.crop <- crop(hum.pop.70, ext, filename = "human_pop_density_2070.tif")

#2100
hum.pop.100 <- raster("ssp2_total_2100.tif")
hum.pop.100.crop <- crop(hum.pop.100, ext, filename = "human_pop_density_2100.tif")


#PERU Shapefile
peru <- readOGR("UIA_Latitude_Longitude_Graticules_and_World_Countries_Boundaries.shp")
world <- readOGR("99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
plot(world)
plot(peru)



###Load all prepared and cropped predictor variables

mean.ann.air.crop <- raster("bio1_1981-2010_MeanAnnualAirTemperatureCropped.tif")
max.temp.wm.crop <- raster("bio5_1981-2010_MeanDailyMaximumAirTemperatureWarmestMonth.tif")
min.temp.cm.crop <- raster("bio6_1981-2010_MeanDailyMinimumAirTemperatureColdestMonth.tif")
ann.prec.crop <- raster("bio12_1981-2010_AnnualPrecipitatonAmount.tif")
prec.wm.crop <- raster("bio13_1981-2010_PrecipitationWettestMonth.tif")
prec.dm.crop <- raster("bio14_1981-2010_PrecipitationDriestMonth.tif")
len.grow.crop <- raster("gsl_1981-2010_LengthOfGrowingSeason.tif")
snow.cov.crop <- raster("scd_1981-2010_SnowCoverDays.tif")
humanfootprint_crop <- raster("wildareas-v3-2009-human-footprint_Projected.tif")
human.pop.density.crop <- raster("human_pop_density_2000.tif")
elevation.crop <- raster('elevation_SA_cropped.tif')
slope <- raster("Slope.tif")
evergreen.broadleaf <- raster("evergreen_broadleaf.tif")
mixed.forest <- raster("mixed.tif")

#check resolution
res(humanfootprint_crop)



####################################
### Create Pseudoabsence Data ######
#####################################

### Species Data - Presence and Absence
#read eagle presence data
eagle <- gbif("spizaetus", species="isidori", args=NULL, geo=TRUE, sp=FALSE, 
              removeZeros=FALSE, download=TRUE, ntries=5, start=1, end=Inf)

#remove NA
eagle <- eagle[complete.cases(eagle$lat,eagle$lon),]

#remove sampled presences from before 1981
eagle$year <- as.numeric(eagle$year)
eagle_1981 = subset(eagle, eagle$year > "1980" & eagle$basisOfRecord != "PRESERVED_SPECIMEN")

#transform into spatial points
eagle_spatialPoints <- SpatialPoints(coords=data.frame(lon=eagle_1981$lon, lat = eagle_1981$lat))

#Choose extent: 85W-60W, 30S-15N
extent(eagle_spatialPoints)

#Use Elevation as raster layer
plot(elevation.crop)
points(eagle_spatialPoints, col=brewer.pal(9, 'Reds'), box=F, axes=F, legend=T)


### Sample pseudoabsence data

#Create buffer of 100 km around each presence because of pop.density of 2 birds per 100km^2
#exclude that buffer zone from random sampling
EagleBuffer <- gBuffer(eagle_spatialPoints, width=1) #in kilometers
plot(elevation.crop)
plot(EagleBuffer, col="light blue", add=T)


#now create random pseudo-absence except where buffer is
species_absence_buffer <- mask(elevation.crop, EagleBuffer, inverse = TRUE)
plot(species_absence_buffer)

#now sample 2200 pseudoabsences (same amount as presence data)
species_absence_definite <- as.data.frame(sampleRandom(species_absence_buffer, 2200, xy=T)[,1:2])
plot(elevation.crop)
points(species_absence_definite, col='red', pch=4, cex=0.7)

#assign 1 for presence and 0 for absence and combine the two datasets to one
eagle_1981_pres <- eagle_1981[eagle_1981$acceptedScientificName == "Spizaetus isidori (Des Murs, 1845)", c('lon', 'lat')]
eagle_1981_pres$presence <- 1
colnames(species_absence_definite) <- c('lon', 'lat')
species_absence_definite$presence <- 0
eagle_occurence_1981 <- rbind(eagle_1981_pres, species_absence_definite)


plot(elevation.crop)
points(species_absence_definite, col='red', pch=4, cex=0.7)
points(eagle_1981_pres, col='blue', pch=4, cex=0.7)

write.csv(eagle_occurence_1981, file = "Eagle_Pres_Abs_1981.csv")



############################################################################
####### Variable Selection ###################################################
############################################################################

#Load Eagle Presence/Absence data
eagle_occurence_1981 <- read.csv2("Eagle_Pres_Abs_1981.csv", header = T, sep = ",")
eagle_occurence_1981= subset(eagle_occurence_1981, select = -c(X))

eagle_occurence_1981$lon <- as.numeric(eagle_occurence_1981$lon)
eagle_occurence_1981$lat <- as.numeric(eagle_occurence_1981$lat)


# We create a matrix with the response variable and predictors at the locations where the response variable was sampled
eagle.model.matrix <- cbind(eagle_occurence_1981, elev=extract(elevation.crop, eagle_occurence_1981[,c("lon","lat")]),
                            slope = extract(slope, eagle_occurence_1981[,c("lon","lat")]),
                            mean.temp=extract(mean.ann.air.crop, eagle_occurence_1981[,c("lon","lat")]),
                            max.temp.wm=extract(max.temp.wm.crop, eagle_occurence_1981[,c("lon","lat")]),
                            min.temp.cm=extract(min.temp.cm.crop, eagle_occurence_1981[,c("lon","lat")]),
                            ann.prec=extract(ann.prec.crop, eagle_occurence_1981[,c("lon","lat")]),
                            wm.prec=extract(prec.wm.crop, eagle_occurence_1981[,c("lon","lat")]),
                            dm.prec=extract(prec.dm.crop, eagle_occurence_1981[,c("lon","lat")]),
                            grow.season=extract(len.grow.crop, eagle_occurence_1981[,c("lon","lat")]),
                            snow.cover=extract(snow.cov.crop, eagle_occurence_1981[,c("lon","lat")]),
                            humanfootprint=extract(humanfootprint_crop, eagle_occurence_1981[,c("lon","lat")]),
                            human.pop.density = extract(human.pop.density.crop, eagle_occurence_1981[,c("lon","lat")]),
                            evergreen.broad=extract(evergreen.broadleaf, eagle_occurence_1981[,c("lon","lat")]),
                            mixed.forest=extract(mixed.forest, eagle_occurence_1981[,c("lon","lat")]))
dim(eagle.model.matrix)
summary(eagle.model.matrix)

#remove NAs
eagle.model.matrix <- eagle.model.matrix[complete.cases(eagle.model.matrix),]

#extract the eagle occurence again to have the same occurence data used in the matrix
eagle_occurence_1981 <- eagle.model.matrix[,1:3]


#check for correlations among predictor variables
ecospat.cor.plot(eagle.model.matrix[,4:17])

#The following variables are highly correlated with each other:
#elevation with mean temperature and max temperature and min temperature
#annual precipitation with precipitation in the wettest month and precipitation in the driest month (BUT WETTEST AND DRIEST DO NOT CORRELATE STRONGLY???)
#human footprint and human pop. density

##########################
#### Look at predictive power of our variables

for(i in 4:17){
  glm.bi <- glm(eagle.model.matrix[,3] ~ eagle.model.matrix[,i]+I(eagle.model.matrix[, i]^2), family = 'binomial')
  cat('Predictive Power', names(eagle.model.matrix[i]), '=', ecospat.adj.D2.glm(glm.bi)*100, '\n')
}

#Predictive Power elev = 65.10047 
#Predictive Power slope = 42.84395 
#Predictive Power mean.temp = 62.17729 
#Predictive Power max.temp.wm = 63.47954 
#Predictive Power min.temp.cm = 54.18186 
#Predictive Power ann.prec = 8.109943 
#Predictive Power wm.prec = 13.89212 
#Predictive Power dm.prec = 4.317854 
#Predictive Power grow.season = 12.10357 
#Predictive Power snow.cover = 1.307185 
#Predictive Power humanfootprint = 14.58928 
#Predictive Power human.pop.density = 0 
#Predictive Power evergreen.broad = 15.77521 
#Predictive Power mixed.forest = 2.134216 


#'based on the correlations and the predictive powers of our variables, we select the following variables for our model:
#'(we use mean.temp instead of elevation, because of the future predictions that will change temp but not elevation)

#'mean.temp - mean annual temperature
#'slope - slope
#'precipitation during the wettest month - wm.prec
#'length of the growing season - grow.season
#'snow cover- snow.cover
#'human footprint - humanfootprint
#'evergreen boradleaf cover - evergreen.broad
#'mixed forest cover - mixed.forest


#########################################################################################################################
########################################################   GLM   ########################################################
#########################################################################################################################


#fit formula
form.glm <- as.formula(presence ~ mean.temp + I(mean.temp^2) + slope + I(slope^2) + wm.prec + I(wm.prec^2) + grow.season + I(grow.season^2) 
                       + snow.cover + I(snow.cover^2) + humanfootprint + I(humanfootprint^2) + evergreen.broad + I(evergreen.broad^2) + mixed.forest + I(mixed.forest^2))

#set up glm
glm.eagle.full <- glm(form.glm, data = eagle.model.matrix, family = 'binomial')
summary(glm.eagle.full)
ecospat.adj.D2.glm(glm.eagle.full) #0.8291838

#variable reduction using stepwise model selection
glm.eagle.step <- step(glm.eagle.full, directions = 'both', trace = 0)
summary(glm.eagle.step)
ecospat.adj.D2.glm(glm.eagle.step) #0.8292337

#very high D2 values for both models


###Model validation

##cross validation
#Set up the function
cv.model <- function(model, K, data = model$data){
  ks <- kfold(model$data, k = K, by = model$data[,as.character(formula(model)[2])])
  cvpreds <- data.frame(row = row.names(data), cvpred = numeric(length = nrow(data)))
  for(i in 1:K){
    train <- data[ks!=i,]
    test <- data[ks==i,]
    modtmp <- update(model, data = train)
    cvpreds[which(ks==i),2] <- predict(modtmp, newdata = test, type = 'response')
  }
  cvpreds
}

#perform cross validation
xval.glm.eagle.step <- cv.model(glm.eagle.step, K = 5)

head(xval.glm.eagle.step)

plot(glm.eagle.step$fitted.values, xval.glm.eagle.step$cvpred,
     xlab = 'fitted values from stepwise optimize model',
     ylab = 'predicted values from cross-validation',
     main = 'Cross validation GLM', cex.lab=0.75)
abline(0, 1, lwd = 3, col = "red")


##treshold optimization
#prepare data
glm.eagle.step.test <- data.frame(ID = 1:nrow(eagle.model.matrix), 
                                  observed = eagle.model.matrix$presence, predicted = glm.eagle.step$fitted)
xval.glm.eagle.step.test <- data.frame(ID = 1:nrow(eagle.model.matrix), 
                                       observed = eagle.model.matrix$presence, predicted = xval.glm.eagle.step$cvpred)

#calculate optimal threshold
(thres.glm.eagle.step <- optimal.thresholds(glm.eagle.step.test, threshold = 1001, opt.methods = 1:9))
(thres.glm.eagle.xval <- optimal.thresholds(xval.glm.eagle.step.test, threshold = 1001, opt.methods = 1:9))


#assess model performance using kappa
Kappa(cmx(glm.eagle.step.test, threshold = thres.glm.eagle.step[4,2])) # Kappa = 0.9362589 sd = 0.005318264

Kappa(cmx(xval.glm.eagle.step.test, threshold = thres.glm.eagle.xval[4,2])) #Kappa = 0.9316745 sd = 0.00550038


##ROC-plots and AUC
str(roc(glm.eagle.step.test$predicted, as.factor(glm.eagle.step.test$observed)))

#storing raw data for plotting ROC-curves and calculating AUC
roc.glm.eagle.step <- roc(glm.eagle.step.test$predicted, as.factor(glm.eagle.step.test$observed))
roc.glm.eagle.xval <- roc(xval.glm.eagle.step.test$predicted, as.factor(xval.glm.eagle.step.test$observed))


#plotting the ROC curves
plot(roc.glm.eagle.step, col = "grey20", lwd = 5, lty = 1, main='Eagle')
plot(roc.glm.eagle.xval, col = "grey70", lwd = 3, lty = 2, add = TRUE)


#comparing AUC values
auc(roc.glm.eagle.step) #0.9872774

auc(roc.glm.eagle.xval) #0.9864871

#'roc.glm.eagle.step has the better AUC, but only by a very small margin


##Spatial projection
#create stacked raster
eagle.extent.stack <- stack(mean.ann.air.crop,
                            slope,
                            prec.wm.crop,
                            len.grow.crop,
                            snow.cov.crop,
                            humanfootprint_crop,
                            evergreen.broadleaf,
                            mixed.forest)

#give raster names
names(eagle.extent.stack) <- c("mean.temp", 
                               "slope", 
                               "wm.prec", 
                               "grow.season", 
                               "snow.cover", 
                               "humanfootprint", 
                               "evergreen.broad", 
                               "mixed.forest")
#do spatial prediction
proj.eagle.prob <- predict(eagle.extent.stack, glm.eagle.step, type = "response")

#plot spatial prediction
cols <- colorRampPalette(c("grey90", "aquamarine1", "yellow", "darkgoldenrod2", "red", "firebrick4"))
par(mfrow = c(2, 1), mar = c(2, 3, 3, 3))
plot(proj.eagle.prob, col = cols(200), main = "Continuous Spatial Projection GLM", box=F, axes=F, 
     legend.args = list(text = 'Predicted Probability', side = 2, font = 4, cex = 0.9, line = 0.5), 
     legend.width = 2, legend.mar = 5, zlim = c(0, 1), legend.shrink = 0.9)
#add sampled eagle presence points
points(eagle_occurence_1981[eagle_occurence_1981$presence == 1, 'lon'], 
       eagle_occurence_1981[eagle_occurence_1981$presence == 1, 'lat'], pch = 16, cex = 0.5, col = "black")

#convert probability to binary raster
proj.eagle.bin <- proj.eagle.prob > thres.glm.eagle.step[4,2]


plot(proj.eagle.bin, col = c("grey90", "firebrick3"), 
     box=F, axes=F, legend=F, main='Binary spatial projection GLM')
legend("right", fill = c("firebrick3", "grey90"), legend = c('Presence', 'Absence'), xpd = TRUE, bty = 'n')
points(eagle_occurence_1981[eagle_occurence_1981$presence == 1, 'lon'], 
       eagle_occurence_1981[eagle_occurence_1981$presence == 1, 'lat'], pch = 16, cex = 0.5)
par(par_def)


#########################################################################################################################
########################################################   GAM   ########################################################
#########################################################################################################################

##fitting the model
#install.packages("gam")
library(gam)
form.gam.eagle <- as.formula(presence ~ s(mean.temp, df=5) + s(slope, df=5) + s(wm.prec,df=5)
                             + s(grow.season, df=5) + s(snow.cover,df=5) + s(humanfootprint, df=5)
                             + s(evergreen.broad, df=5) + s(mixed.forest, df=5))
gam.eagle.full <- gam::gam(form.gam.eagle, data = eagle.model.matrix, family = 'binomial')
ecospat.adj.D2.glm(gam.eagle.full) #0.8848379
summary(gam.eagle.full)

##variable reduction
scope.gam <- gam.scope(eagle.model.matrix[, c('mean.temp', 'slope', 'wm.prec', 
                                              'grow.season', 'snow.cover', 'humanfootprint', 
                                              'evergreen.broad', 'mixed.forest')], 
                       arg = 'df = 5')
gam.eagle.step <- step.Gam(gam.eagle.full, scope = scope.gam, direction = 'both', trace = 0)
ecospat.adj.D2.glm(gam.eagle.step) #0.8847036
summary(gam.eagle.step)


##cross validation

cv.model <- function(model, K, data = model$data){
  ks <- kfold(model$data, k = K, by = model$data[,as.character(formula(model)[2])])
  cvpreds <- data.frame(row = row.names(data), cvpred = numeric(length = nrow(data)))
  for(i in 1:K){
    train <- data[ks!=i,]
    test <- data[ks==i,]
    modtmp <- update(model, data = train)
    cvpreds[which(ks==i),2] <- predict(modtmp, newdata = test, type = 'response')
  }
  cvpreds
}

xval.gam.eagle.step <- cv.model(gam.eagle.step, K = 5)
par(mfcol = c(1, 1))
plot(gam.eagle.step$fitted.values, xval.gam.eagle.step$cvpred, 
     xlab = "fitted values from stepwise optimized model", 
     ylab = "predicted values from cross-validation",
     main="Cross validation GAM")
abline(0, 1, lwd = 3, col = "red")

##Model evaluation

#optimize threshold
gam.eagle.step.test <- data.frame(ID = 1:nrow(eagle.model.matrix),
                                  observed = eagle.model.matrix$presence,
                                  predicted = gam.eagle.step$fitted)
thres.gam.eagle.step <- optimal.thresholds(gam.eagle.step.test, threshold = 1001,
                                           opt.methods = 1:9)

#calculate Kappa
Kappa(cmx(gam.eagle.step.test, threshold = thres.gam.eagle.step[4,2])) #Kappa 0.945425 sd= 0.004929869

#assess AUC
auc(roc(gam.eagle.step.test$predicted, as.factor(gam.eagle.step.test$observed))) #0.9951703

#produce spatial projections
pred.gam.eagle.prob <- predict(eagle.extent.stack, gam.eagle.step, type = 'response')
pred.gam.eagle.bin <- pred.gam.eagle.prob > thres.gam.eagle.step[4,2]

cols <- colorRampPalette(c("grey90", "aquamarine1", "yellow", "darkgoldenrod2", "red", "firebrick4"))
par(mfrow = c(2, 1), mar = c(0.2, 0.75, 0.75, 0.75))
plot(pred.gam.eagle.prob, col = cols(200), main = "Continuous Spatial Projection GAM", box=F, axes=F, 
     legend.args = list(text = 'Predicted Probability', side = 2, font = 4, cex = 0.9, line = 0.5), 
     legend.width = 2, legend.mar = 5, zlim = c(0, 1), legend.shrink = 0.9)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.5)
plot(pred.gam.eagle.bin, col = c("grey90", "firebrick3"), 
     main = "Binary Spatial Projection GAM", box=F, axes=F, legend.width = 2, legend.mar = 5, legend = FALSE)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.5)
legend("right", fill = c("firebrick3", "grey90"), legend = c('Presence', 'Absence'), xpd = TRUE, bty = 'n')
par_def <- par(no.readonly = T)
par(par_def)


#########################################################################################################################
########################################################   CART  ########################################################
#########################################################################################################################

# We first fit a tree that is highly flexible and will produce a model that is overfit to the data, given the many branches
CART.eagle.full <- tree(factor(presence, levels=c(0,1)) ~ 
                          mean.temp + slope + wm.prec + grow.season + 
                          snow.cover + humanfootprint + evergreen.broad + mixed.forest, 
                        minsize = 2, mindev = 0, data = eagle.model.matrix)
plot(CART.eagle.full, type = 'uniform')
text(CART.eagle.full, cex = 0.4)


# We check the deviance using cross-validation
set.seed(123)
xval.CART.eagle.full <- cv.tree(CART.eagle.full, K = 5)
plot(xval.CART.eagle.full)
#Lowest deviance starting from about 8-10 terminal nodes


# We prune the tree and plot it
CART.eagle.prun <- prune.tree(CART.eagle.full, best = 10) #maybe adjust that number
plot(CART.eagle.prun, type = 'uniform')
text(CART.eagle.prun, cex = 0.4)
summary(CART.eagle.prun)

library(PresenceAbsence)
# Commpare manually to automatically optimized thresholds
CART.eagle.prun.test <- data.frame(ID = 1:nrow(eagle.model.matrix), 
                                   observed = eagle.model.matrix$presence, 
                                   predicted = predict(CART.eagle.prun)[,'1'])
(threshold.CART.Eagle.prun <- optimal.thresholds(CART.eagle.prun.test, threshold = 1001, opt.methods = 1:9))
table(predict(CART.eagle.prun, type = 'class'), predict(CART.eagle.prun)[,'1'] > threshold.CART.Eagle.prun[3,2])
table(predict(CART.eagle.prun, type = 'class'), predict(CART.eagle.prun)[,'1'] > threshold.CART.Eagle.prun[4,2])


# We calculate Kappa using the Kappa and cmx function from the PresenceAbsence package
Kappa(cmx(CART.eagle.prun.test , threshold = threshold.CART.Eagle.prun[4,2])) 
#Kappa = 0.9353406 0.005354741


# We further assess AUC using the auc function from the AUC package, 
auc(roc(CART.eagle.prun.test$predicted, as.factor(CART.eagle.prun.test$observed)))
#AUC = 0.9885144


###Produce Spatial Projections

# We create maps for potential distribution of the Black-and Chestnut Eagle
pred.CART.eagle.probability <- predict(eagle.extent.stack, CART.eagle.prun, index=2)
pred.CART.eagle.bin <- predict(eagle.extent.stack, CART.eagle.prun, type = 'class')

cols <- colorRampPalette(c("grey90", "aquamarine1", "yellow", "darkgoldenrod2", "red", "firebrick4"))
par(mfrow = c(2, 1), mar = c(2, 3, 3, 3))
plot(pred.CART.eagle.probability, col = cols(200), main = "Continuous Spatial Projection CART", 
     box=F, axes=F, legend.args = list(text = 'Predicted Probability', side = 2, font = 4, cex = 0.9, line = 0.5),
     legend.width = 2, legend.mar = 5, zlim = c(0, 1), legend.shrink = 0.9)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.3)
plot(pred.CART.eagle.bin, col = c("grey90", "firebrick3"), 
     main = "Binary Spatial Projection CART", box=F, axes=F, legend.width = 2, legend.mar = 5, legend = FALSE)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.3)
legend("right", fill = c("firebrick3", "grey90"), legend = c('Presence', 'Absence'), xpd = TRUE, bty = 'n')
par(par_def)


##########################################
############# Compare models #############
##########################################


par(mfrow=c(1,3), mar=c(0,0,0,0))
plot(proj.eagle.bin, col = c("grey90", "firebrick3"), 
     main = "GLM", box=F, axes=F, legend = FALSE, line = -3)
plot(pred.gam.eagle.bin, col = c("grey90", "coral2"), 
     main = "GAM", box=F, axes=F,legend = FALSE, line = -3)
plot(pred.CART.eagle.bin, col = c("grey90", "darkred"), 
     main = "CART", box=F, axes=F,legend = FALSE, line = -3)
par(par_def)


correlation.tech <- stack(c(proj.eagle.bin, pred.gam.eagle.bin, pred.CART.eagle.bin))
names(correlation.tech) <- c('GLM', 'GAM', 'CART')
cor(na.omit(values(correlation.tech)), method="pearson")

#     GLM       GAM      CART
#GLM  1.0000000 0.8256611 0.7445019
#GAM  0.8256611 1.0000000 0.7441760
#CART 0.7445019 0.7441760 1.0000000


##########################################
############# RANDOM FOREST ##############
##########################################

library(randomForest)
rf.eagle <- randomForest(presence ~ mean.temp + slope + wm.prec + grow.season + 
                           snow.cover + humanfootprint + evergreen.broad + mixed.forest, 
                         data=eagle.model.matrix)
summary(rf.eagle)

rf.eagle$importance #shows that mean.temp and slope are the most important predictor variables

#Optimize threshold
thres.rf.eagle <- optimal.thresholds(cbind(eagle.model.matrix$lon, eagle.model.matrix$presence, rf.eagle$predicted), 
                                     threshold = 1001, opt.methods = 1:9)
pred.rf.eagle.prob <- predict(eagle.extent.stack, rf.eagle, type='response')

#Create binary prediction according to threshold
pred.rf.eagle.bin <- pred.rf.eagle.prob > thres.rf.eagle[4,2]


# We create maps for potential distribution of the Rock Ptarmigan RF
cols <- colorRampPalette(c("grey90", "aquamarine1", "yellow", "darkgoldenrod2", "red", "firebrick4"))
par(mfrow = c(2, 1), mar = c(2, 3, 3, 3))
plot(pred.rf.eagle.prob, col = cols(200), main = "Continuous Spatial Projection", box=F, axes=F, 
     legend.args = list(text = 'Predicted Probability', side = 2, font = 4, cex = 0.9, line = 0.5), 
     legend.width = 2, legend.mar = 5, zlim = c(0, 1), legend.shrink = 0.9)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.3)
plot(pred.rf.eagle.bin, col = c("grey90", "firebrick3"), main = "Binary Spatial Projection",
     box=F, axes=F, legend.width = 2, legend.mar = 5, legend = FALSE)
points(eagle.model.matrix[eagle.model.matrix$presence == 1, 'lon'], 
       eagle.model.matrix[eagle.model.matrix$presence == 1, 'lat'], pch = 16, cex = 0.3)
legend("right", fill = c("firebrick3", "grey90"), legend = c('Presence', 'Absence'), xpd = TRUE, bty = 'n')
par(par_def)

#Compare all 4 models
par(mfrow=c(1,4), mar=c(0,0,0,0))
plot(proj.eagle.bin, main='GLM', box=F, axes=F, legend=F, line=-3)
plot(pred.gam.eagle.bin, main='GAM', box=F, axes=F, legend=F, line=-3)
plot(pred.CART.eagle.bin, main='CART', box=F, axes=F, legend=F, line=-3)
plot(pred.rf.eagle.bin, main = "RandomForest",box=F, axes=F, legend=F, line=-3)
par(par_def)

correlation.tech4 <- stack(c(proj.eagle.bin, pred.gam.eagle.bin, pred.CART.eagle.bin, pred.rf.eagle.bin))
names(correlation.tech4) <- c('GLM', 'GAM', 'CART', "RandomForest")
cor(na.omit(values(correlation.tech4)), method="pearson")


#             GLM       GAM      CART RandomForest
#GLM          1.0000000 0.8257997 0.7445013    0.7163115
#GAM          0.8257997 1.0000000 0.7442516    0.6942841
#CART         0.7445013 0.7442516 1.0000000    0.7548224
#RandomForest 0.7163115 0.6942841 0.7548224    1.0000000

save.image(file='workspace_SDM_4models.RData')


#################################################
# 5.2 Model ensemble
#################################################


load("workspace_SDM_4models.RData")


# We now plot the projected Eagle distribution of the three models

proj.eagle.ensemble <- proj.eagle.bin + pred.gam.eagle.bin + pred.CART.eagle.bin

par(mfrow=c(1,1), mar=c(1,1,1,1))
cols <- c("lightgrey","red","orange","#00A600FF")
plot(proj.eagle.ensemble, box=F, axes=F, legend=F,col=cols,  
     main='Current Eagle Distribution: Model agreement', cex.main=0.75)
legend("bottomleft",fill=cols,c("Absence","1 model","2 models","3 models"),cex=1)


# We plot the three different ensembles
#We isolate the area where the three models agree
proj.eagle.ensemble.1 <- proj.eagle.ensemble > 0
proj.eagle.ensemble.2 <- proj.eagle.ensemble > 1
proj.eagle.ensemble.3 <- proj.eagle.ensemble > 2

# and plot the ensembles
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(proj.eagle.ensemble.1, box=F, axes=F, legend=F, col = c("grey90", "royalblue4"),
     main='full ensemble', cex.main=0.75, line = -3)
plot(proj.eagle.ensemble.2, box=F, axes=F, legend=F, col = c("grey90", "steelblue3"),
     main='agreement of 2 models', cex.main=0.75, line = -3)
plot(proj.eagle.ensemble.3, box=F, axes=F, legend=F, col = c("grey90", "skyblue4"),
     main='agreement of 3 models', cex.main=0.75, line = -3)


##########################################
############ FUTURE SCENARIOS ############
##########################################

#Read in future data
gfdl.rcp26=gfdl.rcp70=gfdl.rcp85=mpi.rcp26=mpi.rcp70=mpi.rcp85=eagle.extent.stack

##GFDL model  
#RCP2.6
gfdl.rcp26[["mean.temp"]]=raster("gfdl_rcp26_bio1_2041-2070_MeanAnnualAirTemperature.tif")
gfdl.rcp26[["wm.prec"]]=raster("gfdl_rcp26_bio13_2041-2070_PrecipitationWettestMonth.tif")
gfdl.rcp26[["grow.season"]]=raster("gfdl_rcp26_gsl_2041-2070_LengthOfGrowingSeason.tif")
gfdl.rcp26[["snow.cover"]]=raster("gfdl_rcp26_scd_2041-2070_SnowCoverDays.tif")


#RCP7.0
gfdl.rcp70[["mean.temp"]]=raster("gfdl_rcp70_bio1_2041-2070_MeanAnnualAirTemperature.tif")
gfdl.rcp70[["wm.prec"]]=raster("gfdl_rcp70_bio13_2041-2070_PrecipitationWettestMonth.tif")
gfdl.rcp70[["grow.season"]]=raster("gfdl_rcp70_gsl_2041-2070_LengthOfGrowingSeason.tif")
gfdl.rcp70[["snow.cover"]]=raster("gfdl_rcp70_scd_2041-2070_SnowCoverDays.tif")
  

#RCP8.5
gfdl.rcp85[["mean.temp"]]=raster("gfdl_rcp85_bio1_2041-2070_MeanAnnualAirTemperature.tif")
gfdl.rcp85[["wm.prec"]]=raster("gfdl_rcp85_bio13_2041-2070_PrecipitationWettestMonth.tif")
gfdl.rcp85[["grow.season"]]=raster("gfdl_rcp85_gsl_2041-2070_LengthOfGrowingSeason.tif")
gfdl.rcp85[["snow.cover"]]=raster("gfdl_rcp85_scd_2041-2070_SnowCoverDays.tif")


##MPI model
#RCP2.6
mpi.rcp26[["mean.temp"]]=raster("mpi_rcp26_bio1_2041-2070_MeanAnnualAirTemperature.tif")
mpi.rcp26[["wm.prec"]]=raster("mpi_rcp26_bio13_2041-2070_PrecipitationWettestMonth.tif")
mpi.rcp26[["grow.season"]]=raster("mpi_rcp26_gsl_2041-2070_LengthOfGrowingSeason.tif")
mpi.rcp26[["snow.cover"]]=raster("mpi_rcp26_scd_2041-2070_SnowCoverDays.tif")


#RCP7.0
mpi.rcp70[["mean.temp"]]= raster("mpi_rcp70_bio1_2041-2070_MeanAnnualAirTemperature.tif")
mpi.rcp70[["wm.prec"]]=raster("mpi_rcp70_bio13_2041-2070_PrecipitationWettestMonth.tif")
mpi.rcp70[["grow.season"]]= raster("mpi_rcp70_gsl_2041-2070_LengthOfGrowingSeason.tif")
mpi.rcp70[["snow.cover"]]=raster("mpi_rcp70_scd_2041-2070_SnowCoverDays.tif")
  

#RCP8.5
mpi.rcp85[["mean.temp"]]= raster("mpi_rcp85_bio1_2041-2070_MeanAnnualAirTemperature.tif")
mpi.rcp85[["wm.prec"]]=raster("mpi_rcp85_bio13_2041-2070_PrecipitationWettestMonth.tif")
mpi.rcp85[["grow.season"]]=raster("mpi_rcp85_gsl_2041-2070_LengthOfGrowingSeason.tif")
mpi.rcp85[["snow.cover"]]= raster("mpi_rcp85_scd_2041-2070_SnowCoverDays.tif")
  

###Human data -> not used since predictive power = 0
#Human population density 2040
  #human.pop.density.40 <- raster("human_pop_density_2040.tif")
  
#Human population density 2040
  #human.pop.density.70 <- raster("human_pop_density_2070.tif")

#Human population density 2040
  #human.pop.density.100 <- raster("human_pop_density_2100.tif")
  

###########################
#Future prediction GLM ###
#########################

# Using our GLMs, we can now predict the binary bird species distribution for the future
#eagle, GFDL RCP26
proj.glm.eagle.prob.gfdl.rcp26 <- predict(gfdl.rcp26, glm.eagle.step, type = "response")
proj.glm.eagle.bin.gfdl.rcp26 <- proj.glm.eagle.prob.gfdl.rcp26 > thres.glm.eagle.step[4,2]

#eagle, GFDL RCP 70
proj.glm.eagle.prob.gfdl.rcp70 <- predict(gfdl.rcp70, glm.eagle.step, type = "response")
proj.glm.eagle.bin.gfdl.rcp70 <- proj.glm.eagle.prob.gfdl.rcp70 > thres.glm.eagle.step[4,2]

#eagle, GFDL RCP 85
proj.glm.eagle.prob.gfdl.rcp85 <- predict(gfdl.rcp85, glm.eagle.step, type = "response")
proj.glm.eagle.bin.gfdl.rcp85 <- proj.glm.eagle.prob.gfdl.rcp85 > thres.glm.eagle.step[4,2]

#eagle, MPI-ESM-MR RCP 26
proj.glm.eagle.mpi.rcp26 <- predict(mpi.rcp26, glm.eagle.step, type = "response")
proj.glm.eagle.bin.mpi.rcp26 <- proj.glm.eagle.mpi.rcp26 > thres.glm.eagle.step[4,2]

#eagle, MPI-ESM-MR RCP 70
proj.glm.eagle.prob.mpi.rcp70 <- predict(mpi.rcp70, glm.eagle.step, type = "response")
proj.glm.eagle.bin.mpi.rcp70 <- proj.glm.eagle.prob.mpi.rcp70 > thres.glm.eagle.step[4,2]

#eagle, MPI-ESM-MR RCP 85
proj.glm.eagle.prob.mpi.rcp85 <- predict(mpi.rcp85, glm.eagle.step, type = "response")
proj.glm.eagle.bin.mpi.rcp85 <- proj.glm.eagle.prob.mpi.rcp85 > thres.glm.eagle.step[4,2]


#save.image(file='workspace_SDM_4models.RData')

#load('workspace_SDM_4models.RData')

###################################################
# Plot future predictions (eagle_glm_prediction)
##################################################

par(mar=c(2,0,1,0), mfrow=c(3,3))
colkey <- c('#F2F2F2FF', '#00A600FF')
brks <- c(0,0.5,1.5)
plot(proj.glm.eagle.bin.gfdl.rcp26, box=F, axes=F, legend=F, col = c("grey90", "steelblue3"), breaks=brks,
     main='predicted distribution in 2100, GFDL-RCP 26', cex.main=0.75)
plot(proj.glm.eagle.bin.gfdl.rcp70, box=F, axes=F, legend=F, col = c("grey90", "skyblue4"), breaks=brks, 
     main='predicted distribution in 2100, GFDL-RCP 70', cex.main=0.75)
plot(proj.glm.eagle.bin.gfdl.rcp85, box=F, axes=F, legend=F, col = c("grey90", "royalblue4"), breaks=brks, 
     main='predicted distribution in 2100, GFDL-RCP 85', cex.main=0.75)
plot(proj.glm.eagle.bin.mpi.rcp26, box=F, axes=F, legend=F, col = c("grey90", "coral1"), breaks=brks, 
     main='predicted distribution in 2100, MPI-RCP 26', cex.main=0.75)
plot(proj.glm.eagle.bin.mpi.rcp70, box=F, axes=F, legend=F, col = c("grey90", "indianred"), breaks=brks,
     main='predicted distribution in 2070, MPI-RCP 70', cex.main=0.75)
plot(proj.glm.eagle.bin.mpi.rcp85, box=F, axes=F, legend=F, col = c("grey90", "maroon"), breaks=brks,
     main='predicted distribution in 2070, MPI-RCP 85', cex.main=0.75)
plot(proj.eagle.bin, box=F, axes=F, legend=F, col=colkey, breaks=brks,
     main='current distribution', cex.main=0.75)
par(par_def)

# We plot the predictions for the two GCMs (GFDL and MPI) for the chestnut eagle GLM model 
# and look were they overlap and where they differ

#RCP2.6
par(mar=c(2,0,1,0), mfrow=c(1,1))
cols <- c("#E5E4E299","#FC8D59","#756BB1","#99D594")
#we assign the value 2, to identify afterwards
proj.glm.eagle.bin.gfdl.rcp26[proj.glm.eagle.bin.gfdl.rcp26 > 0] <- 2 
proj.glm.eagle.diff.rcp26 <- proj.glm.eagle.bin.gfdl.rcp26 + proj.glm.eagle.bin.mpi.rcp26
plot(proj.glm.eagle.diff.rcp26, box=F, axes=F, legend=F, col=cols, 
     main='GCM agreement in 2100 under RCP2.6', cex.main=0.75)
legend("topright",fill=cols,c("Absence","GFDL-RCP","MPI-RCP","Overlap"),cex=0.7)

#RCP7.0
par(mar=c(2,0,1,0), mfrow=c(1,1))
cols <- c("#E5E4E299","#FC8D59","#756BB1","#99D594")
#we assign the value 2, to identify afterwards
proj.glm.eagle.bin.gfdl.rcp70[proj.glm.eagle.bin.gfdl.rcp70 > 0] <- 2 
proj.glm.eagle.diff.rcp70 <- proj.glm.eagle.bin.gfdl.rcp70 + proj.glm.eagle.bin.mpi.rcp70
plot(proj.glm.eagle.diff.rcp70, box=F, axes=F, legend=F, col=cols, 
     main='GCM agreement in 2100 under RCP7.0', cex.main=0.75)
legend("topright",fill=cols,c("Absence","GFDL-RCP","MPI-RCP","Overlap"),cex=0.7)

#RCP8.5
par(mar=c(2,0,1,0), mfrow=c(1,1))
cols <- c("#E5E4E299","#FC8D59","#756BB1","#99D594")
#we assign the value 2, to identify afterwards
proj.glm.eagle.bin.gfdl.rcp85[proj.glm.eagle.bin.gfdl.rcp85 > 0] <- 2 
proj.glm.eagle.diff.rcp85 <- proj.glm.eagle.bin.gfdl.rcp85 + proj.glm.eagle.bin.mpi.rcp85
plot(proj.glm.eagle.diff.rcp70, box=F, axes=F, legend=F, col=cols, 
     main='GCM agreement in 2100 under RCP8.5', cex.main=0.75)
legend("topright",fill=cols,c("Absence","GFDL-RCP","MPI-RCP","Overlap"),cex=0.7)


# Next, we plot the predictions for the three RCPs of GFDL for the eagle and look were they overlap and where they differ
par(mar=c(2,0,1,0), mfrow=c(1,1))
cols <- c("#E5E4E299","#FC8D59","#756BB1","#99D594", "gold1")
proj.glm.eagle.diff <- proj.glm.eagle.bin.gfdl.rcp26 + proj.glm.eagle.bin.gfdl.rcp70 + proj.glm.eagle.bin.gfdl.rcp85
plot(proj.glm.eagle.diff, box=F, axes=F, legend=F, col=cols, 
     main='RCP overlap in 2070 under GFDL', cex.main=0.75)
legend("topright",fill=cols,c("Absence","RCP8.5","RCP4.5", "RCP2.6","Overlap"),cex=0.7)
proj.glm.eagle.bin.gfdl.rcp26[proj.glm.eagle.bin.gfdl.rcp26] <- 1 #we give back the value 1


# Next, we plot the predictions for the three RCPs of MPI for the eagle and look were they overlap and where they differ
par(mar=c(2,0,1,0), mfrow=c(1,1))
cols <- c("#E5E4E299","#FC8D59","#756BB1","#99D594", "gold1")
proj.glm.eagle.diff <- proj.glm.eagle.bin.mpi.rcp26 + proj.glm.eagle.bin.mpi.rcp70 + proj.glm.eagle.bin.mpi.rcp85
plot(proj.glm.eagle.diff, box=F, axes=F, legend=F, col=cols, 
     main='RCP overlap in 2070 under MPI', cex.main=0.75)
legend("topright",fill=cols,c("Absence","RCP8.5","RCP4.5", "RCP2.6","Overlap"),cex=0.7)
proj.glm.eagle.bin.mpi.rcp26[proj.glm.eagle.bin.mpi.rcp26] <- 1 #we give back the value 1



################################
######Calculate the range change and the correlations between the models##########
#########################

# We compare the different GCMs
correlation.tech.rcp26 <- stack(proj.glm.eagle.bin.gfdl.rcp26, proj.glm.eagle.bin.mpi.rcp26)
correlation.tech.rcp70 <- stack(proj.glm.eagle.bin.gfdl.rcp70, proj.glm.eagle.bin.mpi.rcp70)
correlation.tech.rcp85 <- stack(proj.glm.eagle.bin.gfdl.rcp85, proj.glm.eagle.bin.mpi.rcp85)

names(correlation.tech.rcp26) <- c('GFDL.RCP2.6', 'MPI.RCP2.6')
names(correlation.tech.rcp70) <- c('GFDL.RCP7.0', 'MPI.RCP7.0')
names(correlation.tech.rcp85) <- c('GFDL.RCP8.5', 'MPI.RCP8.5')

cor(na.omit(values(correlation.tech.rcp26)), method="pearson")
cor(na.omit(values(correlation.tech.rcp70)), method="pearson")
cor(na.omit(values(correlation.tech.rcp85)), method="pearson")

#               GFDL.RCP2.6 MPI.RCP2.6
#GFDL.RCP2.6   1.0000000  0.9554258
#MPI.RCP2.6    0.9554258  1.0000000

#             GFDL.RCP7.0 MPI.RCP7.0
#GFDL.RCP7.0   1.0000000  0.9398514
#MPI.RCP7.0    0.9398514  1.0000000

#             GFDL.RCP8.5 MPI.RCP8.5
#GFDL.RCP8.5   1.0000000  0.9499756
#MPI.RCP8.5    0.9499756  1.0000000

# projectMod_future_GCMs
correlation.tech.gfdl <- stack(proj.glm.eagle.bin.gfdl.rcp26, proj.glm.eagle.bin.gfdl.rcp70, proj.glm.eagle.bin.gfdl.rcp85)
correlation.tech.mpi <- stack(proj.glm.eagle.bin.mpi.rcp26, proj.glm.eagle.bin.mpi.rcp70, proj.glm.eagle.bin.mpi.rcp85)

names(correlation.tech.gfdl) <- c("GFDL.RCP2.6", "GFDL.RCP7.0","GFDL.RCP8.5")
names(correlation.tech.mpi) <- c('MPI.RCP2.6', 'MPI.RCP7.0', 'MPI.RCP8.5')

cor(na.omit(values(correlation.tech.gfdl)), method="pearson")
cor(na.omit(values(correlation.tech.mpi)), method="pearson")

#             GFDL.RCP2.6 GFDL.RCP7.0 GFDL.RCP8.5
#GFDL.RCP2.6   1.0000000   0.9301081   0.9153021
#GFDL.RCP7.0   0.9301081   1.0000000   0.9702754
#GFDL.RCP8.5   0.9153021   0.9702754   1.0000000

#             MPI.RCP2.6 MPI.RCP7.0 MPI.RCP8.5
#MPI.RCP2.6  1.0000000  0.9381673  0.9223545
#MPI.RCP7.0  0.9381673  1.0000000  0.9696440
#MPI.RCP8.5  0.9223545  0.9696440  1.0000000



#We take the modelled distribution under current climate
eagle.curr <- proj.eagle.bin

#and build an ensemble with the two GCMs for all three RCPs
eagle.fut.rcp26 <- proj.glm.eagle.bin.gfdl.rcp26 + proj.glm.eagle.bin.mpi.rcp26
#we use the area predicted by both GCMs
eagle.fut.rcp26 <- eagle.fut.rcp26 > 1 
#we assign the value 2
eagle.fut.rcp26 <- eagle.fut.rcp26*2 

#we do the same for rcp7.0
eagle.fut.rcp70 <- proj.glm.eagle.bin.gfdl.rcp70 + proj.glm.eagle.bin.mpi.rcp70
#we use the area predicted by both GCMs
eagle.fut.rcp70 <- eagle.fut.rcp70 > 1 
#we assign the value 2
eagle.fut.rcp70 <- eagle.fut.rcp70*2 


#we do the same for rcp8.5
eagle.fut.rcp85 <- proj.glm.eagle.bin.gfdl.rcp85 + proj.glm.eagle.bin.mpi.rcp85
#we use the area predicted by both GCMs
eagle.fut.rcp85 <- eagle.fut.rcp85 > 1 
#we assign the value 2
eagle.fut.rcp85 <- eagle.fut.rcp85*2 

#now we calculate the range change
eagle.diff.rcp26 <- eagle.curr + eagle.fut.rcp26
eagle.diff.rcp70 <- eagle.curr + eagle.fut.rcp70
eagle.diff.rcp85 <- eagle.curr + eagle.fut.rcp85

#and now we plot
col.ro <- c("#E5E4E299", "darkred", "darkblue", "bisque2")
brks.ro <- c(0,0.5,1.5,2.5,3.5)
par(mfrow = c(1, 3), mar = c(1, 1, 1, 3))
plot(eagle.diff.rcp26, col = col.ro, cex.main=1, legend = FALSE, main = "Range change under RCP2.6", 
     box=F, axes=F, breaks=brks.ro)
legend("bottomleft", fill = col.ro, legend = c("Absence", "Loss", "Gain",
                                                 "Presence"), xpd = TRUE, bty = "n", cex=1.5)
plot(eagle.diff.rcp70, col = col.ro, cex.main=1, legend = FALSE, main = "Range change under RCP7.0",
     box=F, axes=F, breaks=brks.ro)
plot(eagle.diff.rcp85, col = col.ro, cex.main=1, legend = FALSE, main = "Range change under RCP8.5",
     box=F, axes=F, breaks=brks.ro)


###########################################################################
######Calculate overlap between distributions and protected areas##########
###########################################################################


#read in protected areas
prot.areas <- raster("PA_Raster_crop.tif")

#read in Peru shapefile source: https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1/explore?filters=eyJDTlRSWV9OQU1FIjpbIlBlcnUiXX0%3D&location=-6.940476%2C-64.276946%2C3.74
peru <- readOGR("UIA_Latitude_Longitude_Graticules_and_World_Countries_Boundaries.shp")

#Plot current distribution, protected areas and peru shapefile
plot(proj.eagle.bin, col = c("grey90", "peachpuff3"), main = "Current Distribution and Protected Areas", legend =F)
plot(prot.areas, add = T, col = "brown", legend = F)
plot(peru, add = T)


##Calculate overlap between current potential distribution GLM and protected areas 
#multiplying the two rasters will return 0 wherever either of the two rasters is 0
overlap.cur.pa <- prot.areas*proj.eagle.bin
plot(overlap.cur.pa)

#counting the number of tiles with 1 in the overlapped map
area.protected <- freq(overlap.cur.pa)[2,2]
area.protected
#counting the number of tiles with 1 in the potential distribution
area.total <- freq(proj.eagle.bin)[2,2]
area.total

#calculating percentage
perc.prot.cur <- area.protected/area.total*100
perc.prot.cur

##Calculate overlap between future potential distributions GLM and protected areas
#RCP2.6
overlap.fut.pa.26 <- prot.areas*(eagle.fut.rcp26/2)
area.prot.26 <- freq(overlap.fut.pa.26)[2,2]
area.total.26 <- freq(eagle.fut.rcp26/2)[2,2]
perc.prot.26 <- area.prot.26/area.total.26*100
perc.prot.26

#RCP7.0
overlap.fut.pa.70 <- prot.areas*(eagle.fut.rcp70/2)
area.prot.70 <- freq(overlap.fut.pa.70)[2,2]
area.total.70 <- freq(eagle.fut.rcp70/2)[2,2]
perc.prot.70 <- area.prot.70/area.total.70*100
perc.prot.70

#RCP8.5
overlap.fut.pa.85 <- prot.areas*(eagle.fut.rcp85/2)
area.prot.85 <- freq(overlap.fut.pa.85)[2,2]
area.total.85 <- freq(eagle.fut.rcp85/2)[2,2]
perc.prot.85 <- area.prot.85/area.total.85*100
perc.prot.85

#compile information into one table
perc.table <- data.frame("Current Percentage" = unname(perc.prot.cur), "Percentage RCP2.6" = unname(perc.prot.26), 
                         "Percentage RCP7.0" = unname(perc.prot.70), "Percentage RCP8.5" = unname(perc.prot.85))


##########################################################
###### Prioritzr #############################################
##################################################################

#install.packages('prioritizr')
#install.packages('slam')

#Install the Gurobi package

# #For Mac
#install.packages('/Library/gurobi950/macos_universal2/R/gurobi_9.5-0_R_4.1.1.tgz', repos=NULL)

#test if the installation worked
library(slam)
library(gurobi)
library(prioritizr)


######Initialise conversation planning project

###Reshape necessary variables to Peru shapefile
#current and future predictions of the GLM model
eagle.peru.current <- mask(intersect(eagle.curr, peru), peru)

#does it make more sense to take the difference between current and future or just future?
eagle.fut <- stack(eagle.fut.rcp26, eagle.fut.rcp70, eagle.fut.rcp85)
eagle.diff <- stack(eagle.diff.rcp26, eagle.diff.rcp70, eagle.diff.rcp85)

eagle.peru.fut <- mask(intersect(eagle.fut, peru), peru)
eagle.peru.diff <- mask(intersect(eagle.diff, peru), peru)

#reshape the ecological infrastructure/
evergreen.forest.peru <- mask(intersect(evergreen.broadleaf, peru), peru)
mixed.forest.peru <- mask(intersect(mixed.forest, peru), peru)

peru.forest <- evergreen.forest.peru | mixed.forest.peru

#reshape constraints
prot.areas.peru <- mask(intersect(prot.areas, peru), peru)

#reshape the cost variable
humanfootprint_peru <- mask(intersect(humanfootprint_crop, peru), peru)

#create features including the current and future distribution
conservation_feature1 <- stack(eagle.peru.current)
conservation_feature2 <- stack(eagle.peru.fut, eagle.peru.current)


###add a solver, objective, target, constraints, penalties (--> penalties too computationally expensive), 
###decision type

#--> which objective do we need?
library(parallel) 
detectCores() #detect the number of available cores

#Protect 20% of distribution
reserve_problem1 <- problem(humanfootprint_peru, conservation_feature1) %>%
  add_gurobi_solver(gap=0.01, threads = detectCores()) %>%
  add_min_set_objective()%>%
  add_relative_targets(0.2)%>%
  add_locked_out_constraints(prot.areas.peru)

#Protect 5% of distribution
reserve_problem1_1 <- problem(humanfootprint_peru, conservation_feature1) %>%
  add_gurobi_solver(gap=0.01, threads = detectCores()) %>%
  add_min_set_objective()%>%
  add_relative_targets(0.05)%>%
  add_locked_out_constraints(prot.areas.peru)

#Protect 20% of distribution including future distribution
reserve_problem2 <- problem(humanfootprint_peru, conservation_feature2) %>%
  add_gurobi_solver(gap=0.01, threads = detectCores()) %>%
  add_min_set_objective()%>%
  add_relative_targets(0.2)%>%
  add_locked_out_constraints(prot.areas.peru)

#Protect 5% of distribution including future distribution
reserve_problem2_1 <- problem(humanfootprint_peru, conservation_feature2) %>%
  add_gurobi_solver(gap=0.01, threads = detectCores()) %>%
  add_min_set_objective()%>%
  add_relative_targets(0.05)%>%
  add_locked_out_constraints(prot.areas.peru)

reserve_solution1 <- solve(reserve_problem1)
reserve_solution1_1 <- solve(reserve_problem1_1)
reserve_solution2 <- solve(reserve_problem2)
reserve_solution2_1 <- solve(reserve_problem2_1)


# Compare the 2 created solution rasters --> only use the solution with fut. and curr. distribution
par(mfrow=c(1,2), mar = c(0.75, 0.75, 6, 0.75))
plot(peru,main="Eagle reserves (20%)")
plot(eagle.peru.current, add=T, box= F, axes=F,legend=F,col=c(NA, 'bisque2'))
plot(reserve_solution2, axes=F, box=F, legend=F, add=T,col=c(NA, 'coral3'))
plot(prot.areas.peru, axes =F, box=F, legend=F, add=T, col="bisque4")
plot(peru,main="Eagle reserves (5%)")
plot(eagle.peru.current, add=T, box= F, axes=F,legend=F,col=c(NA, 'bisque2'))
plot(reserve_solution2_1, axes=F, box=F, legend=F,add=T, col=c(NA,'coral3'))
plot(prot.areas.peru, axes =F, box=F, legend=F, add=T, col="bisque4")
legend("bottomleft", legend=c("Distribution", "New Reserves", "Old Reserves"), 
       col=c('bisque2','coral3',"bisque4"), pch=18, pt.cex=2)


