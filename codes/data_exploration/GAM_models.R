#----------------
# Build GAMS for all species and years
#----------------


#Two approaches
# 1. only build models for species that were present in the three years leading up to the year of the model
#     and average the abundance across these three years, to account for detectability and fluctuations in populations
#     and then predict in the future to the year from which on the species disappears in three consecutive years 
# 2. 