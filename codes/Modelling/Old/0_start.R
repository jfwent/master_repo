## prepare R session to be able to start with the model building
# Author: Jon Went, jwent@ethz.ch
# Date: 27.06.2023

# -------- load libraries --------
rm(list=ls())

library(tidyverse)
library(brms)
library(glmmTMB)

# -------- load data -------------

load("data/database/BBS_final_area.rda")

# --------- what more does the start need? -----