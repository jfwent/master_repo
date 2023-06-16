# beta diversity of ecoregions

#---- load data ----
rm(list = ls())
load("data/abund_mat_list.rda")
abund_list <- res_abund; rm(res_abund)

#sort the matrices in my list, so that all have ordered columns and rows
source("codes/functions/sort_matrix.R")
abund_list_sorted <- lapply(abund_list, sort_matrix)

#---- filter for stable community ----
#load first 3 years
abund_2000 <- abund_list_sorted[[1]]
abund_2001 <- abund_list_sorted[[2]]
abund_2002 <- abund_list_sorted[[3]]

#find common species and subset
common_species <- intersect(intersect(colnames(abund_2000), colnames(abund_2001)), colnames(abund_2002))
common_segments <- intersect(intersect(rownames(abund_2000), rownames(abund_2001)), rownames(abund_2002))

abund_2000_sub <- abund_2000[common_segments, common_species]
abund_2000_sub[is.na(abund_2000_sub)] <- 0
abund_2001_sub <- abund_2001[common_segments, common_species]
abund_2001_sub[is.na(abund_2001_sub)] <- 0
abund_2002_sub <- abund_2002[common_segments, common_species]
abund_2002_sub[is.na(abund_2002_sub)] <- 0

#find species with abundance >0 for all three years
overlap_matrix <- (abund_2000_sub > 0) & (abund_2001_sub > 0) & (abund_2002_sub > 0)

stable_species_mat <- matrix(0, nrow = nrow(overlap_matrix), ncol = ncol(overlap_matrix))

for (i in 1:nrow(overlap_matrix)) {
  for (j in 1:ncol(overlap_matrix)) {
    if (overlap_matrix[i, j]) {  # Check if the value is TRUE
      # Extract the corresponding cells from the three previous matrices
      values <- c(abund_2000_sub[i, j], abund_2001_sub[i, j], abund_2002_sub[i, j])
      
      # Compute the mean value and store it in the stable_species_mat
      stable_species_mat[i, j] <- mean(values)
    }
  }
}

colnames(stable_species_mat) <- common_species
rownames(stable_species_mat) <- common_segments

sum(stable_species_mat > 0)

#check rows and columns
# Check if there are any rows where all values are 0
rows_with_all_zeros <- which(rowSums(stable_species_mat) == 0)

# Check if there are any columns where all values are 0
cols_with_all_zeros <- which(colSums(stable_species_mat) == 0)


if (length(rows_with_all_zeros) > 0) {
  print(paste("Rows with all zeros:", toString(rows_with_all_zeros)))
} else {
  print("No rows with all zeros")
}

if (length(cols_with_all_zeros) > 0) {
  print(paste("Columns with all zeros:", toString(cols_with_all_zeros)))
} else {
  print("No columns with all zeros")
}

# filter these rows and columns
stable_species_mat_filtered <- stable_species_mat[-rows_with_all_zeros, -cols_with_all_zeros]

save(stable_species_mat_filtered, file = "data/stable_species_mat.rda")

#----- calculate beta diversity for USA at segment level ----
rm(list = ls())
load("data/stable_species_mat.rda")
bbs_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)

library(betapart)
bbs_abund_core <- betapart.core.abund(bbs_mat)

bbs_abund_bray <- beta.multi.abund(bbs_abund_core)
bbs_abund_ruzicka <- beta.multi.abund(bbs_abund_core, index.family = "ruzicka")

beta_div_abund <- dplyr::bind_cols(bbs_abund_bray, bbs_abund_ruzicka)

PA_bbs_mat <- ifelse(bbs_mat > 0, 1, 0)
PA_core <- betapart.core(PA_bbs_mat)

bbs_sor <- beta.multi(PA_core, index.family = "sorensen")
bbs_jac <- beta.multi(PA_core, index.family = "jaccard")

beta_div_PA <- dplyr::bind_cols(bbs_sor, bbs_jac)

beta_div_segments <- dplyr::bind_cols(beta_div_PA, beta_div_abund)
print(paste0("Segment beta diversity in USA presence based:",
             round(beta_div_segments[1:6],3),
            ". Segment beta diversity in USA abundance based: ",
            round(beta_div_segments[7:12],3)
            )
      )


#----- calculate beta diversity for ecoregions at segment level ----
#load data
rm(list=ls())
load("data/stable_species_mat.rda")
bbs_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)
PA_bbs_mat <- ifelse(bbs_mat > 0, 1, 0)
load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

# extract ecoregions for segments
segments <- rownames(bbs_mat)
library(dplyr); library(betapart); library(lubridate)
ecoregion_df <- land[which(land$segment %in% segments),]
ecoregion_df <- subset(ecoregions, year == 2001)
ecoregion_df <- ecoregions %>% select(ecoregion, cluster_nr, cluster, segment)
ecoregion_names <- unique(ecoregion_df$ecoregion)

n_iter <- length(ecoregion_names)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

init <- numeric(n_iter)
end <- numeric(n_iter)

beta_div_ecoregions <- list()

for (ecoregion in ecoregion_names) {
  
  init[ecoregion] <- Sys.time()
  #---------------------
  
  # Subset the segment names for the current ecoregion
  segments <- ecoregion_df$segment[ecoregion_df$ecoregion == ecoregion]
  
  # Subset the bbs_mat and presence_absence_mat for the current ecoregion
  bbs_mat_subset <- bbs_mat[segments, ]
  presence_absence_mat_subset <- PA_bbs_mat[segments, ]
  
  abund_core <- betapart.core.abund(bbs_mat_subset)
  PA_core <- betapart.core(presence_absence_mat_subset)
  
  # Calculate beta div for PA
  bbs_sor <- beta.multi(PA_core, index.family = "sorensen")
  bbs_jac <- beta.multi(PA_core, index.family = "jaccard")
  
  # Calculate beta div for Abund
  bbs_abund_bray <- beta.multi.abund(abund_core, index.family = "bray")
  bbs_abund_ruzicka <- beta.multi.abund(abund_core, index.family = "ruzicka")
  
  beta_multi <- dplyr::bind_cols(bbs_sor, bbs_jac, bbs_abund_bray, bbs_abund_ruzicka)
  
  # Store the results for the current ecoregion
  beta_div_ecoregions[[ecoregion]] <- beta_multi
  
  #------ progress bar ----
  
  end[ecoregion] <- Sys.time()
  
  setTxtProgressBar(pb, ecoregion)
  time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
  remainining <- round(seconds_to_period(est), 0)
  
  cat(paste(" // Execution time:", time,
            " // Estimated time remaining:", remainining), "")
}

save(beta_div_ecoregions, file = "data/beta_div_ecoregions_list.rda")

#----- build cluster subset -----
rm(list=ls())

library(dplyr); library(betapart); library(lubridate)

load("data/stable_species_mat.rda")
bbs_mat <- stable_species_mat_filtered; rm(stable_species_mat_filtered)
PA_bbs_mat <- ifelse(bbs_mat > 0, 1, 0)
load("data/land_use_clustered.rda")
land <- combined_df; rm(combined_df)

# extract ecoregions for segments
segments <- rownames(bbs_mat)
ecoregion_df <- land[which(land$segment %in% segments),]
ecoregion_df <- subset(ecoregions, year == 2001)
ecoregion_df <- ecoregions %>% select(ecoregion, cluster_nr, cluster, segment)
ecoregion_names <- unique(ecoregion_df$ecoregion)

#----- calculate beta diversity for ecoregions at cluster level ----

#-----