
####################################################################
# Figure 4 maps of specimen and species sampling  
####################################################################

####################################################################
# load data
####################################################################

# Behrmann equal area (96 x 96km) grid shapefile
grid <- rgdal::readOGR(paste0(SpatialDataFolder,'BehrmannMeterGrid_WGS84_land.shp'))
# Country borders shapefile
countriesGeo <- rgdal::readOGR(paste0(SpatialDataFolder,'all_countries.shp'))
# gridded species geographic range data - Birdlife taxonomy 

#rangeData<-read.csv(paste0(SpatialDataFolder,'AllSpeciesBirdLifeMaps2019.csv'))

# gridded country data 
countryData<-read.csv(paste0(SpatialDataFolder,'CountryBehrmannMeterGrid_WGS84_land.csv'))
# trait data
dat<-read.csv(paste0(TraitDataFolder,'AVONET_Raw_Data.csv'))
# museum names and their lat-long coordinates
museumData<-read.csv(paste0(TraitDataFolder,'AVONET_Data_Sources.csv'))
# list of extant species 
dat_extant<-read.csv(paste0(TraitDataFolder,'AVONET_extant_species_list.csv'))

####################################################################
# data processing and cleaning
####################################################################

# set grid and country shapefile to Behrmann projection
P4S.Behr <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
gridB<-spTransform(grid,P4S.Behr)
countries <- spTransform(countriesGeo,P4S.Behr)

# simplify country shapefile - needed for plotting   
countriesS <- gSimplify(countries, tol=10000,topologyPreserve=TRUE)
# convert to simple feature for plotting with ggplot
countriesS2 <- st_as_sf(countriesS)

# extract columns from trait data indicating country and museum source  - only these needed for plotting
dat<-dat[,match(c("Species1_BirdLife","Source","Country_WRI"),names(dat))]

# set specimens where country is unknown to NA
dat$Country_WRI[which(dat$Country_WRI=="")]<-NA

# restrict set of extant species to birdlife taxonomy
dat_extant<-dat_extant[which(dat_extant$Taxonomy=="BirdLife"),]

# remove cells in range data that don't overlap with cells assigned to countries
# todrop<-setdiff(unique(rangeData$WorldID),unique(countryData$WorldID))
# todrop<-which(is.na(match(rangeData$WorldID,todrop))==F)
# rangeData<-rangeData[-todrop,]
# setdiff(unique(rangeData$WorldID),unique(countryData$WorldID))

#######################################################################
# make figure 4a - plot number of indvividuals from each museum
#######################################################################

sum(museumData$Specimens.sampled,na.rm=T)
# remove rows corresponding to field samples
museumData<-na.omit(museumData)
# total number of individuals measured across all museums
sum(museumData$Specimens.sampled,na.rm=T)

# make spatial points object of museum lat-long coordinates
d <- data.frame(Longitude=museumData$Longitude,Latitude=museumData$Latitude)
coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- proj4string(countriesGeo)
# set to Behrmann projection
d.proj <- spTransform(d, P4S.Behr)
# for plotting set circle sizes to indicate museum sample size
circleSizeBrks<-c(0,10,100,1000,50000)
circleSize <- findInterval(museumData$Specimens.sampled, circleSizeBrks, all.inside = TRUE)
circleSize[which(circleSize==4)]<-16
circleSize[which(circleSize==3)]<-8
circleSize[which(circleSize==2)]<-4
circleSize[which(circleSize==1)]<-2

# plot

inches<-4.5
res<-600

plot.f<-paste0(figFolder,'Museums.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
plot(countriesS,col="grey90",border="grey50")
plot(d.proj,add=TRUE,pch=1,lwd=4,cex=circleSize,col="orangered")
dev.off()
system(paste0('open ',plot.f))

#######################################################################
# make figure 4b - plot number of individuals from each country
#######################################################################

# calculate number of indivdiuals from each country and assign to shapefile
# NB this is the country where the individual was caught (not the museum where it was housed)
# check country names match
setdiff(unique(dat$Country_WRI),countries@data$name)
SpecPerCountry<-table(dat$Country_WRI)
countries@data$Nspecimens<-0
countries@data$Nspecimens<-SpecPerCountry[match(countries@data$name,names(SpecPerCountry))]
countries@data$Nspecimens[which(is.na(countries@data$Nspecimens)==TRUE)]<-0
# total number of individuals from all countries
sum(countries@data$Nspecimens)
# check we haven't missed any...
length(na.omit(dat$Country_WRI))
# total number of countries
length(SpecPerCountry)

# attribute colors to countries based on number of individuals 
class_of_country <- cut(countries@data$Nspecimens,breaks=c(0,1,10,50,100,1000,10000))
my_colors <- brewer.pal(6, "Reds") 
my_colorsCountries <- my_colors[as.numeric(class_of_country)]
# set countries with no data to grey
my_colorsCountries[which(countries@data$Nspecimens==0)]<-"grey80"
#check
#my_colorsCountries[which(countries@data$Nspecimens==0)]
#my_colorsCountries[which(countries@data$Nspecimens==1)]
#my_colorsCountries[which(countries@data$Nspecimens==2)]

# plot
plot.f<-paste0(figFolder,'CountrySpecimens.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
plot(countriesS, col=my_colorsCountries)
dev.off()
system(paste0('open ',plot.f))

#######################################################################
# Figure 5c - plot proportion of species 
#######################################################################

# get cells in each species geographic range
#rangeList<-split(rangeData$WorldID,rangeData$Species)
# get cells in each country
#countryList<-split(countryData$WorldID,countryData$Country)
# split trait data by species
#datList<-split(dat,dat$Species1_BirdLife)
#length(datList) # this includes 10 newly described species and 11 extinct species

# loop over each species, identify the countries where individuals are form 
# and the grid cells that intersect with each sampled country and its breeding geographic range
#Species<-names(datList)
#nSpecies<-length(datList)
#SampledCells<-as.list(rep(NA,nSpecies))
#UnSampledCells<-as.list(rep(NA,nSpecies))
#for(i in 1:nSpecies){
  #get data for focal species
#  focDat<-datList[[i]]
  #get species name
#  focalSpecies<-focDat$Species1_BirdLife[1]
  #get countries where it was collected
#  focalCountries<-unique(na.omit(focDat$Country_WRI))
  #get slot in range list for focal species
#  focalRange<-which(names(rangeList)==focalSpecies)
#  if(length(focalRange)>0){
#    if(length(focalCountries)>0){
      #cells in countries where individuals were collected
#      FocalCountryCells<-unique(unlist(countryList[match(focalCountries,names(countryList))]))
      #cells in species geographic range
#      FocalRangeCells<-rangeList[[focalRange]]
      #intersection of cells in species geographic range and sampled countries
#      SampledCells[[i]]<-intersect(FocalRangeCells,FocalCountryCells)
      #cells in species geographic range not in the sampled countries 
#      UnSampledCells[[i]]<-setdiff(FocalRangeCells,SampledCells[[i]])
#    }else{ # if we have no country info for the species' specimens we just use the whole species geographic range 
#      SampledCells[[i]]<-rangeList[[focalRange]]
#    }
#  }  
#  print(i)
#}
# calculate..... 
# richness of species in each cell that are represented by specimens 
#SampledRichness<-table(na.omit(unlist(SampledCells)))
# richness of species in each cell that are NOT represented by specimens 
#UnSampledRichness<-table(na.omit(unlist(UnSampledCells)))
# total richness
#SamplingRichness<-table(na.omit(c(unlist(SampledCells),unlist(UnSampledCells))))

# table to store richness values for each cell
#RichnessTable<-data.frame(WorldID=as.numeric(names(SamplingRichness)),
#                          TotalRichness=as.numeric(SamplingRichness),
#                          SampledRichness=NA,
#                          UnSampledRichness=NA)
#RichnessTable$SampledRichness[match(as.numeric(names(SampledRichness)),RichnessTable$WorldID)]<-as.numeric(SampledRichness)
#RichnessTable$UnSampledRichness[match(as.numeric(names(UnSampledRichness)),RichnessTable$WorldID)]<-as.numeric(UnSampledRichness)

#write.csv(RichnessTable,file=paste0(SpatialDataFolder,'RichnessTable.csv'))

RichnessTable<-read.csv(paste0(SpatialDataFolder,'RichnessTable.csv'))

# check - sampled richness should be equal to or less than total richnes
plot(RichnessTable$TotalRichness,RichnessTable$SampledRichness)
abline(0,1)
# add species richness to shapefile
gridB@data$TotalRichness<-NA
gridB@data$TotalRichness[match(RichnessTable$WorldID,gridB@data$WorldID)]<-RichnessTable$TotalRichness
# sampled species richness
gridB@data$SampledRichness<-NA
gridB@data$SampledRichness[match(RichnessTable$WorldID,gridB@data$WorldID)]<-RichnessTable$SampledRichness
# proportion of species sampled
gridB@data$PropRichnessSampled<-gridB@data$SampledRichness/gridB@data$TotalRichness

# set breaks and colors
brks <- quantile(gridB@data$PropRichnessSampled,probs=seq(0,1,0.01),na.rm=T)
gridB@data$SamplingCol <- NA
gridB@data$SamplingCol <- findInterval(gridB@data$PropRichnessSampled, brks, all.inside = TRUE)
colors<-c(viridis_pal(option = "D")(100))

# plot map
plot.f<-paste0(figFolder,'SamplingCompleteness.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
gridB2 <- st_as_sf(gridB)
ggplot(gridB2) +
  geom_sf(aes(fill = SamplingCol, color = SamplingCol)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  geom_sf(data = countriesS2, fill = NA,color="black",size = 0.5)+
  theme_void()
dev.off()
system(paste0('open ',plot.f))

# number of species
length(unique(na.omit(dat$Species1_BirdLife)))
# 11020 species, however 11 of these are extinct so not included, hence 11009 species
setdiff(unique(na.omit(dat$Species1_BirdLife)),dat_extant$Species.name)
