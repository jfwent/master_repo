# Calculate the beta diversity, turnover and nestedness using betapart
#######

#--------- set up data -----

rm(list=ls())

load("data/Lica/BBS_partition_abundance.rda")
bbs_all <- BBS_partition_abundance; rm(BBS_partition_abundance)

#------ presence/absence matrix for years 2000 to 2019 -----
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

#----- presence/absence matrix for year 2021 -----

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

res[[21]] <- bbsMat.2021

save(res, file="data/pres_abs_mat_list.rda")

#----- abundance matrix for years 2000-2019 ----
rm(list=ls())
load("data/Lica/BBS_partition_abundance.rda")
bbs_all <- BBS_partition_abundance; rm(BBS_partition_abundance)

res_abund <- list()

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
        sum_abundance <- sum(segment_data$seg_abundance[segment_data$animal_jetz == bird], na.rm = T)
        
        if (!is.na(sum_abundance) && sum_abundance > 0) {
          bbsMat.now[i, j] <- sum_abundance
          
        }else{
          bbsMat.now[i, j] <- 0
        }
      }
      
    }
  }
  
  res_abund[[k]] <- bbsMat.now
  
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

save(res_abund, file="data/abund_mat_list.rda")

#-------- create betapart objects -------

rm(list = ls())
# install.packages("betapart")
library(betapart)
# data set up: matrix with presence/absence (1/0) for species in columns and locations in rows

load("data/pres_abs_mat_list.rda")
load("data/abund_mat_list.rda")

bbs2000 <- res[[1]]
bbs2000[is.na(bbs2000)] = 0
bbs2000_core <- betapart.core(bbs2000)

bbs2001 <- res[[2]]
bbs2001[is.na(bbs2001)] = 0
bbs2001_core <- betapart.core(bbs2001)

bbs2000_abund <- res_abund[[1]]
bbs2000_abund[is.na(bbs2000_abund)] = 0
# 
# # bbs2000_abund_core <- betapart.core.abund(bbs2000_abund) # takes too long to calculate

#----- calculate indices for 2000 ----

bbs2000_multi_sor <- beta.multi(bbs2000_core, index.family = "sorensen")
bbs2000_multi_jac <- beta.multi(bbs2000_core, index.family = "jaccard")

bbs2001_multi_sor <- beta.multi(bbs2001_core, index.family = "sorensen")
bbs2001_multi_jac <- beta.multi(bbs2001_core, index.family = "jaccard")

div_ind_2000_2001 <- bind_rows(bbs2000_multi_sor, bbs2000_multi_jac,
                               bbs2001_multi_sor, bbs2001_multi_jac)

bbs2000_pair_sor <- beta.pair(bbs2000_core, index.family = "sorensen") # creates the distance matrices
bbs2000_pair_jac <- beta.pair(bbs2000_core, index.family = "jaccard") # creates the distance matrices

bbs2001_pair_sor <- beta.pair(bbs2001_core, index.family = "sorensen") # creates the distance matrices
bbs2001_pair_jac <- beta.pair(bbs2001_core, index.family = "jaccard") # creates the distance matrices

bbs_2000_pair_jtu <- (bbs2000_pair_jac$beta.jtu)

# bbs2000_pair_sor, bbs2000_pair_jac, bbs2001_pair_sor, bbs2001_pair_jac

# bbs2000_abund_ind <- beta.multi.abund(bbs2000_abund) # takes too long to calculate

#----- loop over all years (beta diversity) -----
rm(list = ls())

library(betapart)

load("data/pres_abs_mat_list.rda")
PA_mat_list <- res; rm(res)

#initialize years for analysis
years <- 2000:2021

n_iter <- length(years)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

# Create an empty list to store the results
beta_div_list <- list()

for (i in 1:(length(years))) {
  
  init[i] <- Sys.time()
  #---------------------
  
  bbs.now <- PA_mat_list[[i]]
  
  bbs.now[is.na(bbs.now)] <- 0
  
  bbs.now_core <- betapart.core(bbs.now)
  
  bbs.now_sor <- beta.multi(bbs.now_core, index.family = "sorensen")
  bbs.now_jac <- beta.multi(bbs.now_core, index.family = "jaccard")
  
  div_ind.now <- dplyr::bind_rows(bbs.now_sor, bbs.now_jac)
  
  entry_name <- paste0(years[i])
  
  beta_div_list[[entry_name]] <- div_ind.now
  
  #------ progress bar ----
  
  end[i] <- Sys.time()
  
  setTxtProgressBar(pb, i)
  time <- round(lubridate::seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(lubridate::seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}

betadiv_2000 <- beta_div_list[[1]]

save(beta_div_list, file = "data/beta_div_list.rda")

#----- calculate temporal change from 2000 to 2001 ----

#both matrices must contain exactly the make up: same locations and species and same order

sort_cols <- sort(colnames(bbs2000))
bbs2000_sorted <- bbs2000[, sort_cols]

sort_rows <- sort(rownames(bbs2000_sorted))
bbs2000_sorted <- bbs2000_sorted[sort_rows,]

sort_cols <- sort(colnames(bbs2001))
bbs2001_sorted <- bbs2001[, sort_cols]

sort_rows <- sort(rownames(bbs2001_sorted))
bbs2001_sorted <- bbs2001_sorted[sort_rows,]

segments <- intersect(dimnames(bbs2000_sorted)[[1]], dimnames(bbs2001_sorted)[[1]])
species <- intersect(dimnames(bbs2000_sorted)[[2]], dimnames(bbs2001_sorted)[[2]])

bbs2000_subset <- bbs2000_sorted[segments,]
bbs2001_subset <- bbs2001_sorted[segments,]

bbs2000_subset <- bbs2000_subset[, dimnames(bbs2000_subset)[[2]] %in% species]
bbs2001_subset <- bbs2001_subset[, dimnames(bbs2001_subset)[[2]] %in% species]

bbs.t.sor <- beta.temp(bbs2000_subset, bbs2001_subset, index.family="sor")
bbs.t.jac <- beta.temp(bbs2000_subset, bbs2001_subset, index.family="jac")
bbs.t <- bind_cols(bbs.t.sor, bbs.t.jac)

sum(is.na(bbs.t$beta.sim))

#------- loop over all years (temporal beta diversity) ---------
rm(list=ls())

load("data/pres_abs_mat_list.rda")

# Create a vector of years
years <- 2000:2019

n_iter <- length(years)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

# Create an empty list to store the results
results <- list()

# Loop over each year
for (i in 1:(length(years) - 1)) {
  
  init[i] <- Sys.time()
  #---------------------
  
  current_year <- years[i]
  next_year <- years[i + 1]
  
  # Load the data for the current and next year
  bbs_current <- res[[i]]
  bbs_next <- res[[i + 1]]
  
  # Remove NA values from the current year's data
  bbs_current[is.na(bbs_current)] <- 0
  
  # Sort the columns of the current year's data
  sort_cols_current <- sort(colnames(bbs_current))
  bbs_current_sorted <- bbs_current[, sort_cols_current]
  
  # Sort the rows of the current year's data
  sort_rows_current <- sort(rownames(bbs_current_sorted))
  bbs_current_sorted <- bbs_current_sorted[sort_rows_current,]
  
  # Remove NA values from the next year's data
  bbs_next[is.na(bbs_next)] <- 0
  
  # Sort the columns of the next year's data
  sort_cols_next <- sort(colnames(bbs_next))
  bbs_next_sorted <- bbs_next[, sort_cols_next]
  
  # Sort the rows of the next year's data
  sort_rows_next <- sort(rownames(bbs_next_sorted))
  bbs_next_sorted <- bbs_next_sorted[sort_rows_next,]
  
  # Subset the data to common segments and species
  segments <- intersect(dimnames(bbs_current_sorted)[[1]], dimnames(bbs_next_sorted)[[1]])
  species <- intersect(dimnames(bbs_current_sorted)[[2]], dimnames(bbs_next_sorted)[[2]])
  
  bbs_current_subset <- bbs_current_sorted[segments, ]
  bbs_next_subset <- bbs_next_sorted[segments, ]
  
  bbs_current_subset <- bbs_current_subset[, dimnames(bbs_current_subset)[[2]] %in% species]
  bbs_next_subset <- bbs_next_subset[, dimnames(bbs_next_subset)[[2]] %in% species]
  
  # Calculate the metrics for the current and next year
  bbs_t_sor <- beta.temp(bbs_current_subset, bbs_next_subset, index.family = "sor")
  bbs_t_jac <- beta.temp(bbs_current_subset, bbs_next_subset, index.family = "jac")
  
  # Bind the calculated metrics
  bbs_t <- bind_cols(bbs_t_sor, bbs_t_jac)
  
  # Create the new name for the list entry
  entry_name <- paste0(current_year, "_", next_year)
  
  # Store the results in the list
  results[[entry_name]] <- bbs_t
  
  #------ progress bar ----
  
  end[i] <- Sys.time()
  
  setTxtProgressBar(pb, i)
  time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}

# Access the results for a specific year
result_2005_2006 <- results[["2005_2006"]]

beta_diversity <- results
save(beta_diversity, file = "data/beta_diversity_list.rda")

#------ visualize some results -----
rm(list=ls())

load("data/beta_diversity_list.rda")
load("data/pres_abs_mat_list.rda")

bbs.t <- beta_diversity[["2000_2001"]]

# Set up the plot
with(bbs.t, plot(sqrt(beta.sim) ~ sqrt(beta.sne),
                 type = 'n',
                 ylab = expression(sqrt(beta[sim])),
                 xlab = expression(sqrt(beta[sne])),
                 main = paste0("Temp diff beta div 2000-2001")))

# Calculate the density of data points
point_density <- density(sqrt(bbs.t$beta.sne), na.rm = TRUE)

# Set up the transparency based on point density
opacity <- point_density$y / max(point_density$y)

# Add markers with transparency
with(bbs.t, points(sqrt(beta.sne), sqrt(beta.sim), pch = 16, cex = 1, col = alpha("black", opacity)))

#---- loop over years to plot -----
# Create an empty list to store the plots
plot_list <- list()

x_limits <- c(0, max(sapply(beta_diversity, function(x) sqrt(max(x$beta.sne, na.rm = TRUE)))))
y_limits <- c(0, max(sapply(beta_diversity, function(x) sqrt(max(x$beta.sim, na.rm = TRUE)))))

# Iterate over each entry in beta_diversity
for (i in seq_along(beta_diversity)) {
  # Extract the data for the current entry
  bbs.t <- beta_diversity[[i]]
  
  # Set up the plot
  with(bbs.t, plot(sqrt(beta.sim) ~ sqrt(beta.sne),
                   type = 'n',
                   ylab = expression(sqrt(beta[sim])), # value of the turnover component, measured as Simpson dissimilarity
                   xlab = expression(sqrt(beta[sne])), # value of the nestedness component,
                                                      # measured as nestedness-resultant fraction of Sorensen dissimilarity
                   xlim = x_limits,
                   ylim = y_limits,
                   main = paste0("Temp diff beta div ", names(beta_diversity)[i])))
  
  # Calculate the density of data points
  point_density <- density(sqrt(bbs.t$beta.sne), na.rm = TRUE)
  
  # Set up the transparency based on point density
  opacity <- point_density$y / max(point_density$y)
  
  # Add markers with transparency
  with(bbs.t, points(sqrt(beta.sne), sqrt(beta.sim), pch = 16, cex = 1, col = alpha("black", opacity)))
  
  # Store the plot in the plot_list
  plot_list[[i]] <- recordPlot()
}

save(plot_list, file="figures/beta_diversity_plot_list.rda")

#---- create animation to show temporal changes ----
library(ggplot2); library(gganimate)

# Create an empty list to store the data frames
df_list <- list()

# Iterate over each entry in beta_diversity
for (i in seq_along(beta_diversity)) {
  
  # Extract the data for the current entry
  bbs.t <- beta_diversity[[i]]
  
  # Convert data to numeric and remove rows with missing values
  bbs.t$beta.sne <- as.numeric(bbs.t$beta.sne)
  bbs.t$beta.sim <- as.numeric(bbs.t$beta.sim)
  bbs.t <- na.omit(bbs.t)
  
  # Create a data frame for plotting
  df <- data.frame(
    x = sqrt(bbs.t$beta.sne),
    y = sqrt(bbs.t$beta.sim),
    label = rownames(bbs.t),
    year = rep(names(beta_diversity)[i], nrow(bbs.t))
  )
  
  # Remove rows with missing values
  df <- na.omit(df)
  
  # Add the data frame to the list
  df_list[[i]] <- df
}

# Combine the data frames into a single data frame
combined_df <- do.call(rbind, df_list)

# Create the ggplot object
p <- ggplot(combined_df, aes(x = x, y = y, label = label, frame = year)) +
  geom_point() +
  labs(x = expression(sqrt(beta[sne])),
       y = expression(sqrt(beta[sim])),
       title = "Temp diff beta div {closest_state}") +
  theme_minimal() +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("linear")

# Save the animation as a GIF
anim_save("figures/biodiversity_animation.gif", animation = p)

