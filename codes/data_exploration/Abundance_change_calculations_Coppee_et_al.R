
# try calculating the abundance change in the same way that Coppée et al. 2022 caluclated the abundance
load("data/abund_mat_list.rda")

bbs2000 <- res_abund[[1]]
bbs2000[is.na(bbs2000)] <- 0

bbs2019 <- res_abund[[20]]
bbs2019[is.na(bbs2019)] <- 0

#---- function for calculation----
d_abund_calc <- function(abund_year1, abund_year2){
  delta_change <- (abund_year2 - abund_year1)/((abund_year1 + abund_year2)/2)
  return(delta_change)
}

#---- make sure the two matrices have the exact same birds and segments-----
sort_cols <- sort(colnames(bbs2000))
bbs2000_sorted <- bbs2000[, sort_cols]

sort_rows <- sort(rownames(bbs2000_sorted))
bbs2000_sorted <- bbs2000_sorted[sort_rows,]

sort_cols <- sort(colnames(bbs2019))
bbs2019_sorted <- bbs2019[, sort_cols]

sort_rows <- sort(rownames(bbs2019_sorted))
bbs2019_sorted <- bbs2019_sorted[sort_rows,]

segments <- intersect(dimnames(bbs2000_sorted)[[1]], dimnames(bbs2019_sorted)[[1]])
species <- intersect(dimnames(bbs2000_sorted)[[2]], dimnames(bbs2019_sorted)[[2]])

bbs2000_subset <- bbs2000_sorted[segments,]
bbs2019_subset <- bbs2019_sorted[segments,]

bbs2000_subset <- bbs2000_subset[, dimnames(bbs2000_subset)[[2]] %in% species]
bbs2019_subset <- bbs2019_subset[, dimnames(bbs2019_subset)[[2]] %in% species]

rm(segments, sort_cols, sort_rows, species)

#---- calculation ----
delta_abund <- d_abund_calc(bbs2000_subset, bbs2019_subset)

#---- extract extinctions ----
#extinctions defined by authors as delta abundance = -2

# Find indices where the value is -2
indices <- which(delta_abund == -2, arr.ind = TRUE)
seg_names <- rownames(delta_abund)
spec_names <- colnames(delta_abund)

# Extract corresponding locations and species
segment_ind <- indices[, 1]
species_ind <- indices[, 2]

# Get original names based on indices
segments <- seg_names[segment_ind]
species <- spec_names[species_ind]

# Create a new matrix with extracted locations and species
extinctions <- cbind(segments, species)

#--- calculate apparitions ----
# apparitions defined by authors as delta abund = +2

# Find indices where the value is +2
indices <- which(delta_abund == 2, arr.ind = TRUE)
seg_names <- rownames(delta_abund)
spec_names <- colnames(delta_abund)

# Extract corresponding locations and species
segment_ind <- indices[, 1]
species_ind <- indices[, 2]

# Get original names based on indices
segments <- seg_names[segment_ind]
species <- spec_names[species_ind]

# Create a new matrix with extracted locations and species
apparitions <- cbind(segments, species)

#----- loop over years ----
rm(list = ls())

load("data/abund_mat_list.rda")

#Initialize years
years <- 2000:2019

#Prepare containers for results
delta_abund_list <- list()
extinction_list <- list()
apparition_list <- list()

# Loop over the years
for (i in 1:(length(years) - 1)) {
  year1 <- years[i]
  year2 <- years[i + 1]
  
  # Get the matrices for the current years
  abund_year1 <- res_abund[[year1 - 1999]]
  abund_year2 <- res_abund[[year2 - 1999]]
  
  #Make sure the matrices have exactly the same makeup (same segments and bird species in same column and row number)
  sort_cols <- sort(colnames(abund_year1))
  abund_year1_sorted <- abund_year1[, sort_cols]
  
  sort_rows <- sort(rownames(abund_year1_sorted))
  abund_year1_sorted <- abund_year1_sorted[sort_rows,]
  
  sort_cols <- sort(colnames(abund_year2))
  abund_year2_sorted <- abund_year2[, sort_cols]
  
  sort_rows <- sort(rownames(abund_year2_sorted))
  abund_year2_sorted <- abund_year2_sorted[sort_rows,]
  
  segments <- intersect(dimnames(abund_year1_sorted)[[1]], dimnames(abund_year2_sorted)[[1]])
  species <- intersect(dimnames(abund_year1_sorted)[[2]], dimnames(abund_year2_sorted)[[2]])
  
  abund_year1_subset <- abund_year1_sorted[segments,]
  abund_year2_subset <- abund_year2_sorted[segments,]
  
  abund_year1_subset <- abund_year1_subset[, dimnames(abund_year1_subset)[[2]] %in% species]
  abund_year2_subset <- abund_year2_subset[, dimnames(abund_year2_subset)[[2]] %in% species]
  
  # Perform the calculations
  delta_abund <- d_abund_calc(abund_year1_subset, abund_year2_subset)
  
  #Get indices for apparitions
  indices_ex <- which(delta_abund == -2, arr.ind = TRUE)
  
  # Extract corresponding locations and species
  segment_ind_ex <- indices_ex[, 1]
  species_ind_ex <- indices_ex[, 2]
  
  # Get original names based on indices
  segments_ex <- segments[segment_ind_ex]
  species_ex <- species[species_ind_ex]
  
  # Extract extinctions
  extinctions <- cbind(segments_ex, species_ex)
  
  #Get indices for apparitions
  indices_app <- which(delta_abund == 2, arr.ind = TRUE)
  
  # Extract corresponding locations and species
  segment_ind_app <- indices_app[, 1]
  species_ind_app <- indices_app[, 2]
  
  # Get original names based on indices
  segments_app <- segments[segment_ind_app]
  species_app <- species[species_ind_app]
  
  # Extract apparitions
  apparitions <- cbind(segments_app, species_app)
  
  # Store the results in the corresponding lists
  delta_abund_list[[i]] <- delta_abund
  extinction_list[[i]] <- extinctions
  apparition_list[[i]] <- apparitions
  
  # Assign names to the results
  delta_name <- paste0("delta_abund_", year1, "_", year2)
  extinction_name <- paste0("extinction_", year1, "_", year2)
  apparition_name <- paste0("apparition_", year1, "_", year2)
  
  names(delta_abund_list)[i] <- delta_name
  names(extinction_list)[i] <- extinction_name
  names(apparition_list)[i] <- apparition_name
}

# I have the problem now, what do we do with re-apparitions and re-extinctions?
# It could be that in one year a certain abundance was detected
# and then 0 for a year, and then again some abundance for a year

app_2000_2001 <- apparition_list[[1]]
ex_2000_2001 <- extinction_list[[1]]
