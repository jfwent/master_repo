
####################################################################
# Code Files
####################################################################

# # # 1.InstallPackagesAndSetDirectories.R # # #

Description: set directories and install required packages

# # # 2.MakeBLFamilyPhylogeny.R # # #

Description: Code to make family (BirdLife taxonomy) level phylogeny from global BirdTree for use in Figure3.R.

Data required: 'HackettStage1_0001_1000_MCCTreeTargetHeights', 'BirdLife-BirdTree crosswalk.csv', 'AVONET1_BirdLife.csv' 

# # # 3.RepeatabilityAnalysis.R # # #

Description: Code to run repeatability analysis on duplicate measurements - results shown in table S4.

Data required: 'AVONET_Duplicate_Data.csv'  

# # # DrawCircularPhylogeny.R # # #

Description: Functions used to make circular phylogeny in Figure3.R

Data required: none

# # # Figure1.R # # #

Description: Code to generate plot of sampling levels across historical studies in figure 1.

Notes: Positions of numeric labels were adjusted and background colors added separately for aesthetics.

Data required: 'TraitDatabasesHistory.csv' 

# # # Figure3.R # # #

Description: Code to generate circular phylogeny and sampling barplots in figure 3

Notes: Circular phylogeny and barplots were combined after export from R. Bird images, color key and circles indicating family species richness were added separately. 

Data required: 'bl_Specimens_per_family.csv', 'TaxonomySampling.txt', 'Birdlife_Family_Phylogeny.nex'

# # # Figure4.R # # #

Description: Code to generate maps in figure 4

Notes: Maps were combined after export from R. Keys and labels were added separately. 

Data required: 'BehrmannMeterGrid_WGS84_land.shp', 'all_countries.shp', 'AllSpeciesBirdLifeMaps2019.csv', 'CountryBehrmannMeterGrid_WGS84_land.csv', 'AVONET_Raw_Data.csv', 'AVONET_Data_Sources.csv', 'RichnessTable.csv'   

# # # Figure5.R # # #

Description: Code to calculate variance within different taxonomic levels in figure 5

Notes: plots were exported from R and bird images from HBW were added separately.

Data required: 'AVONET_Raw_Data.csv', 'AVONET1_BirdLife.csv'   

# # # Figure6.R # # #

Description: Code to generate maps and boxplots in figure 6

Notes: Maps and boxplots were combined after export from R. Keys and labels were added separately. 

Data required: 'BehrmannMeterGrid_WGS84_land.shp', 'all_countries.shp', 'AllSpeciesBirdLifeMaps2019.csv', 'AVONET1_BirdLife.csv', 'AVONET_extant_species_list', 'MedianTraitValuesByCell.rda'   

# # # FigureS1_S2.R # # #

Description: Code to generate Bland–Altman/Scatter plots in supplementary figure 1 and 2

Data required: 'AVONET_Duplicate_Data.csv'  

# # # FigureS3_S4.R # # #

Description: Code to generate Scatter plots in supplementary figure 3 and 4

Data required: 'AVONET_Duplicate_Data.csv'  

# # # FigureS5.R # # #

Description: Code to generate Bland–Altman/Scatter plots in supplementary figure 5

Data required: 'AVONET_Raw_Data.csv', 'AVONET_Extant_Species_List.csv'   

####################################################################
# Data Files
####################################################################

## TraitData
# AVONET_Data_Sources.csv
# AVONET_Duplicate_Data.csv
# AVONET_Extant_Species_List.csv
# AVONET_Raw_Data.csv
# AVONET1_Birdlife.csv
# bl_Specimens_per_family.csv
# TaxonomySampling.txt
# TraitDatabasesHistory.csv

## PhylogeneticData
# BirdLife-BirdTree crosswalk.csv
# Birdlife_Family_Phylogeny.nex
# HackettStage1_0001_1000_MCCTreeTargetHeights

## SpatialData
# all_countries.shp
# AllSpeciesBirdLifeMaps2019.csv
# BehrmannMeterGrid_WGS84_land.shp
# CountryBehrmannMeterGrid_WGS84_land.csv
# RichnessTable.csv   
# MedianTraitValuesByCell.rda

####################################################################
# Data Files Description
####################################################################

###########################
## ## ## TraitData ## ## ##
###########################

# # # AVONET_Data_Sources.csv # # #

Description: .csv file containing the names of the museums where specimens were measured and their geographic location.

Notes: used in Figure4.R

# # # AVONET_Duplicate_Data.csv # # #

Description: .csv file containing duplicate specimen trait data

Notes: used in 3.RepeatabilityAnalysis.R, FigureS1_S2.R and FigureS3_S4.R

# # # AVONET_Extant_Species_List.csv # # #

Description: .csv file containing the names of extant species of the three taxonomies (BirdLife, eBird and BirdTree) - including order/family/genus information for each species.

Notes: used in FigureS5.R

# # # AVONET_Raw_Data.csv # # #

Description: .csv file containing individual specimen level trait data

Notes: used in Figure4.R and Figure5.R

# # # AVONET1_Birdlife.csv # # #

Description: .csv file containing species level trait averages according to BirdLife taxonomy, as well as niche, trophic level and lifestyle assignments, geographic range size, lat and long.

Notes: used in 2.MakeBLFamilyPhylogeny.R, Figure5.R and Figure6.R

# # # bl_Specimens_per_family.csv # # #

Description: .csv file containing the proportion of species per family (Bird Life taxonomy) with different levels of sampling completeness

Notes: used in Figure3.R

# # # TaxonomySampling.txt # # #

Description: tab delimited .txt file containing the proportion of species in the full dataset with different levels of sampling completeness for the Bird Life, eBird and BirdTree taxonomy. Values are those in Table S1.

Notes: used in Figure3.R

# # # TraitDatabasesHistory.csv # # # 

Description: .csv file containing basic summaries of historical bird trait studies

Notes: used in Figure1.R

###################################
## ## ## Phylogenetic Data ## ## ##
###################################

# # # BirdLife-BirdTree crosswalk.csv # # #

Description: .csv file match species names in BirdLife(Species1) to BirdTree (Species3) taxonomy

Notes: used to generate 'Birdlife_Family_Phylogeny.nex' in '2.MakeBLFamilyPhylogeny.R'

# # # HackettStage1_0001_1000_MCCTreeTargetHeights # # #

Description: Maximum Clade Credibility tree generated using treeAnnotator (https://beast.community/treeannotator) based on the first 1000
trees from across the posterior distribution of phylogenies from BirdTree (https://birdtree.org). Trees contained only species (n=6670) represented by genetic data (i.e. Stage 1) according to the Hackett backbone.

Notes: used to generate 'Birdlife_Family_Phylogeny.nex' in '2.MakeBLFamilyPhylogeny.R'

# # # Birdlife_Family_Phylogeny.nex # # #

Description: family level phylogeny according to BirdLife Taxonomy

Notes: used in Figure3.R Generated using '2.MakeBLFamilyPhylogeny.R'

##############################
## ## ## Spatial Data ## ## ##
##############################

# # # BehrmannMeterGrid_WGS84_land.shp # # #

Description: Behrmann equal area (96 x 96km) grid shapefile

Notes: used in Figure4.R and Figure6.R

# # # all_countries.shp # # #

Description: Country borders shapefile

Notes: used in Figure4.R and Figure6.R

# # # CountryBehrmannMeterGrid_WGS84_land.csv # # #

Description: .csv file containing the grid cell ID's in 'BehrmannMeterGrid_WGS84_land.shp' (WorldID) overlapping with each country in 'all_countries.shp' 

Notes: used in Figure4.R

# # # AllSpeciesBirdLifeMaps2019.csv # # #

Description: .csv file containing the grid cell ID's in 'BehrmannMeterGrid_WGS84_land.shp' (WorldID) where each species is present during the breeding season based on Bird Life species distribution data (Birdlife International, 2019). 

Birdlife International, NatureServe (2019) Bird species distribution maps of the world. Birdlife International, Cambridge, United Kingdom ad NatureServe, Arlington, United States 

We do not have permission to release these data independently but the underlying polygon shapefiles from which the gridded data is derived are freely available on request from BirdLife International. Available here: http://datazone.birdlife.org/species/requestdis

Processed gridded data including proportion of species sampled and median trait values per cell required for mapping in Figures 4 and 6 are provided in 'RichnessTable.csv' and 'MedianTraitValuesByCell.rda'

Notes: used in Figure4.R and Figure6.R

# # # RichnessTable.csv # # #  

Description: .csv file containing the number of species and proportion of species sampled within grid cell ID's in 'BehrmannMeterGrid_WGS84_land.shp' 

Notes: used in Figure4.R

# # # MedianTraitValuesByCell.rda # # # 

Description: .rda file containing the median species trait values within grid cell ID's in 'BehrmannMeterGrid_WGS84_land.shp' 

Notes: used in Figure6.R

##############################
##############################