# Climatic data

# load libraries ----

library(raster)

# Data preparation------

rm(list=ls())

#set extent to be used to crop data
ext <- extent(-125,-67, 25, 50)

## Mean annual air temperature
mean.ann.air <- raster("/Users/jonwent/Desktop/ETHZ/master_thesis/Climate_data/CHELSA_bio1_1981-2010_V.2.1.tif")
plot(mean.ann.air)

mean.ann.air.crop <- crop(mean.ann.air, ext, filename = "data/bio1_1981-2010_MeanAnnualAirTemperatureCropped.tif")

#check results
plot(mean.ann.air.crop)

#check resolution
res(mean.ann.air.crop)
#all Chelsa datasets have a resolution of 0.0083333° x 0.0083333°
# which translates to 1km x 1 km