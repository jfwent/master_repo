#----- Reading and preparation of Data from Bird et al., 2020 Cons Biol

#------ load Bird data ----
library(readxl); library(tidyr); library(dplyr)

rm(list = ls())
bird_df <- read_xlsx("other_studies/Bird_2020_Cons_Biol/cobi13486-sup-0004-tables4.xlsx")
str(bird_df)

bird_groups_df <- read_xlsx("other_studies/Bird_2020_Cons_Biol/cobi13486-sup-0003-tables3.xlsx")
str(bird_groups_df)

#----- load our data ----
load("data/Lica/BBS_partition_abundance.rda")
BBS_df <- BBS_partition_abundance; rm(BBS_partition_abundance)

unique_species <- unique(BBS_df$animal_jetz)

load("data/stable_species_mat.rda")
unique_stable_species <- colnames(stable_species_mat_filtered)

#----
# clean data frames from bird
colnames(bird_df) <- gsub(" ", "_", colnames(bird_df)) # get rid of spaces in colnames
bird_df$animal_jetz <- gsub(" ", "_", bird_df$Scientific_name) # get rid of spaces in bird species entries
# bird_df <- separate(bird_df, Scientific_name, c("Genus", "Species"), " ", remove = F)

bird_df <- bird_df %>%
  select(-c("...17")) %>%
  relocate("animal_jetz", .before = "Scientific_name") # rearrange

#same for second data frame
colnames(bird_groups_df) <- gsub(" ", "_", colnames(bird_groups_df))
bird_groups_df$animal_jetz <- gsub(" ", "_", bird_df$Scientific_name)
bird_groups_df <- bird_groups_df %>%
  relocate("animal_jetz", .before = "Scientific_name")

# ---- select columns I want to keep ----

bird_df_sub <- bird_df %>%
  select(-c(1:6,8))

bird_groups_df_sub <- bird_groups_df %>%
  select(-c(1:3,5))

# ---- join BBS data with data frames ---

Bird_full <- bird_df_sub %>%
  left_join(bird_groups_df_sub, by = "animal_jetz")

BBS_bird <- BBS_df %>%
  left_join(Bird_full, by = "animal_jetz")

#----- get unique species -----
# get unique species in Bird data frame
unique_species_bird <- sort(unique(bird_df$animal_jetz))

# find common species
common_species <- intersect(unique_species, unique_species_bird)
common_stable_species <- intersect(unique_stable_species, unique_species_bird)

#---- subset data frames -----
# subset data frames for common species
BBS_sub <- BBS_df[BBS_df$animal_jetz %in% common_species, ]
bird_sub <- bird_df[bird_df$animal_jetz %in% common_species, ]
bird_ecol_traits <- bird_groups_df[bird_groups_df$animal_jetz %in% common_species, ]

BBS_stable_sub <- BBS_df[BBS_df$animal_jetz %in% common_stable_species,]
bird_stable_sub <- bird_df[bird_df$animal_jetz %in% common_stable_species, ]
bird_ecol_traits_stable <- bird_groups_df[bird_groups_df$animal_jetz %in% common_stable_species, ]

#----- join df ----
# join data frames
matches <- match(BBS_sub$animal_jetz, bird_sub$animal_jetz)
desired_columns <- bird_sub[matches, 9:16]
BBS_sub <- cbind(BBS_sub, desired_columns)
matches2 <- match(BBS_sub$animal_jetz, bird_ecol_traits$animal_jetz)
desired_columns2 <- bird_ecol_traits[matches2, 6:21]
BBS_sub <- cbind(BBS_sub, desired_columns2)

matches_stable <- match(BBS_stable_sub$animal_jetz, bird_stable_sub$animal_jetz)
desired_columns_stable <- bird_stable_sub[matches_stable, 9:16]
BBS_stable_sub <- cbind(BBS_stable_sub, desired_columns_stable)

matches_stable2 <- match(BBS_stable_sub$animal_jetz, bird_ecol_traits_stable$animal_jetz)
desired_columns_stable2 <- bird_ecol_traits_stable[matches_stable2, 6:21]
BBS_sub_stable <- cbind(BBS_stable_sub, desired_columns_stable2)

#----- save files ----
save(BBS_bird, file = "data/BBS_Bird_df.rda")
save(BBS_sub_stable, file = "data/BBS_Bird_stable_df.rda")
