# Calculate the beta diversity, turnover and nestedness using betapart
#######

#--------- set up data -----

rm(list=ls())

# install.packages("betapart")
library(betapart)
# data set up: matrix with presence/absence (1/0) for birds in columns and locations in rows

load("data/Lica/BBS_partition_abundance.rda")
bbs_all <- BBS_partition_abundance; rm(BBS_partition_abundance)

#------ loop through data 2000 to 2019 -----
res <- list()

years <- unique(bbs_all$year)

n_iter <- length(years)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

for(k in seq_along(years)){
  
  init[k] <- Sys.time()
  #---------------------
  
  bbs.now <- subset(bbs_all, year == years[k])
  
  # Get unique segments and bird species
  unique_segments <- unique(bbs.now$partition)
  unique_birds <- unique(bbs.now$animal_jetz)
  
  bbsMat.now <- matrix(NA, nrow = length(unique_segments), ncol = length(unique_birds))
  
  rownames(bbsMat.now) <- unique_segments
  colnames(bbsMat.now) <- unique_birds

  for (i in 1:length(unique_segments)) {
    
    segment <- unique_segments[i]
    
    # Filter the data for the current segment
    segment_data <- bbs.now[bbs.now$partition == segment, ]
    
    # Loop through each bird species
    for (j in 1:length(unique_birds)) {
      bird <- unique_birds[j]
      
      # Check if the bird species exists in the segment data
      if (bird %in% segment_data$animal_jetz) {
        
        # Set presence to 1 if abundance is greater than zero
        max_abundance <- max(segment_data$seg_abundance[segment_data$animal_jetz == bird], na.rm = T)
        
        if (!is.na(max_abundance) && max_abundance > 0) {
          bbsMat.now[i, j] <- 1
          
        }else{
          bbsMat.now[i, j] <- 0
        }
      }
      
    }
  }
  
  res[[k]] <- bbsMat.now
  
  #------ progress bar ----
  
  end[k] <- Sys.time()
  
  setTxtProgressBar(pb, k)
  time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}

#----- create matrix for year 2021 -----

load("data/Lica/BBS_partition_abundance_2021.rda")

unique_segments2021 <- unique(bbs2021$partition)
unique_birds2021 <- unique(bbs2021$animal_jetz)

bbsMat.2021 <- matrix(NA, nrow = length(unique_segments2021), ncol = length(unique_birds2021))

rownames(bbsMat.2021) <- unique_segments2021
colnames(bbsMat.2021) <- unique_birds2021

for (i in 1:length(unique_segments2021)) {
  
  segment <- unique_segments2021[i]
  segment_data <- bbs2021[bbs2021$partition == segment, ]
  
  for (j in 1:length(unique_birds2021)) {
    bird <- unique_birds2021[j]
    
    if (bird %in% segment_data$animal_jetz) {
      max_abundance <- max(segment_data$seg_abundance[segment_data$animal_jetz == bird], na.rm = TRUE)
      
      if (!is.na(max_abundance) && max_abundance > 0) {
        bbsMat.2021[i, j] <- 1
      }else{
        bbsMat.2021[i, j] <- 0
      }
    }
  }
}

