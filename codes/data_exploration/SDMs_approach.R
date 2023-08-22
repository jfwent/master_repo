# Meeting Notes and Ideas 02.08.2023 with Lica

# -----
# First only look at abundance and land use change
# try 3 ways to characterize land use change:
# - binary (no change vs change)
# - bins (little, moderate, big)
# - continuous variable

# ----- Approach similar to Rumpf et al., 2019, Nat Comm
# 1. relate historical abundance or presence-absence data with historical land use data
# (here explore other variables such as centroid of longitude, centroid of latitude, historical climatic data,
# and mean elevetion or elevation at centroid)
# make sure to have same resolution everywhere (land use is in 30m*30m)
# use model ensembles (GLM, GAM, RF, CART) with TSS > 0.6
# convert to presence/absence with threshold that maximizes the TSS score
# k-fold cross validation when projecting to both historical presence-absence  and current presence-absence data
  # dataset split into 10 parts, calibrate model with remaining 90 % of data, project onto remaining 10 %
  # repeat this process 10x to produce presence absence data at each location for both historical and current conditions
# select models with TSS >0.6
# then use majority rule to determine presence-absence per location and set as present in a tie
# across all species: 
# calculate the sensitivity, mean specificity and mean TSS for historical observations and historical projections

# 2. extinction and colonization:
# if present in historical data 