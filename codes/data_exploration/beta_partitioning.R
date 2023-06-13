# Calculate the beta diversity, turnover and nestedness using betapart
#######

#--------- set up data -----

rm(list=ls())

# install.packages("betapart")
library(betapart)

data(bbsData) # 49x569 matrix with presence/absence (1/0) for birds in columns (AOU nr) and state nrs (rows)

load("data/Lica/BBS_partition_abundance_2021.rda")

#------ loop through data -----
res <- list()

years <- unique(bbs2021$year)

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
  
  bbs.now <- subset(bbs2021, year == years[k])
  
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

#-----
res <- list()
years <- unique(bbs2021$year)
n_iter <- length(years)

pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "=")

init <- numeric(n_iter)
end <- numeric(n_iter)

for (k in years) {
  init[k] <- Sys.time()
  bbs.now <- subset(bbs2021, year == k)
  
  unique_segments <- unique(bbs.now$partition)
  unique_birds <- unique(bbs.now$animal_jetz)
  
  bbsMat.now <- matrix(0, nrow = length(unique_segments), ncol = length(unique_birds))
  
  rownames(bbsMat.now) <- unique_segments
  colnames(bbsMat.now) <- unique_birds
  
  for (i in 1:length(unique_segments)) {
    segment <- unique_segments[i]
    segment_data <- bbs.now[bbs.now$partition == segment, ]
    
    for (j in 1:length(unique_birds)) {
      bird <- unique_birds[j]
      
      if (bird %in% segment_data$animal_jetz) {
        max_abundance <- max(segment_data$seg_abundance[segment_data$animal_jetz == bird], na.rm = TRUE)
        
        if (!is.na(max_abundance) && max_abundance > 0) {
          bbsMat.now[i, j] <- 1
        }
      }
    }
  }
  
  res[[as.character(k)]] <- bbsMat.now
  
  end[k] <- Sys.time()
  
  setTxtProgressBar(pb, k)
  time <- round(seconds_to_period(sum(end - init)), 0)
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remaining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time, " // Estimated time remaining:", remaining), "")
}
