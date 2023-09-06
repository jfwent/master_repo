# download NA BBS data
# Author: Jon Went, jwent@ethz.ch
# Date: 04.09.2023

# ---- libraries ----

# install.packages("bbsAssistant") not available for this R version

library(rdataretriever)
library(DBI)
library(tidyverse)


#----- 

bbs_db <- dbConnect(RSQLite::SQLite(), 'bbs.sqlite')

surveys <- tbl(bbs_db, "breed_bird_survey_counts") # database connection not possible
sites <- tbl(bbs_db, "breed_bird_survey_routes")

#
rdataretriever::fetch('breed-bird-survey')
