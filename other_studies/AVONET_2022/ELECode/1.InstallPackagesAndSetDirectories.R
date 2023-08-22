
# set working directory
wd<-''
figFolder<-paste0(wd,'ELEFigures/')
dataFolder<-paste0(wd,'ELEData/')
codeFolder<-paste0(wd,'ELECode/')

PhylogeneticDataFolder<-paste0(dataFolder,'PhylogeneticData/')
TraitDataFolder<-paste0(dataFolder,'TraitData/')
SpatialDataFolder<-paste0(dataFolder,'SpatialData/')

if(!dir.exists(figFolder)) dir.create(figFolder)

# install packages
packages <- c("ape", "phytools", "readxl", "rgdal", "rgeos", "sf", "RColorBrewer", "viridis", "ggplot2", "gridExtra", "dplyr", "lme4", "cowplot",
              "lmerTest","scales","ggExtra","grid","rptR","MuMIn","nlme")
install.packages(setdiff(packages, rownames(installed.packages())))

library(ape)
library(phytools)
library(readxl)
require(rgdal)
library(rgeos)
library(sf)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lme4)
library(cowplot)
library(lmerTest)
library(scales)
library(ggExtra)
library(grid)
library(rptR)
library(MuMIn)
library(nlme)

