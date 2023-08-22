# Population viability modelling, first exploration
#Author: Jon Went, jwent@ethz.ch
# Date: 31.07.2023

# ----- libraries ----

library(popbio)
library(tidyverse)
library(ggplot2)

#----- load data ----

rm(list=ls())

load("data/Lica/BBS_partition_abundance.rda")
load("data/Lica/BBS_partition_abundance_2021.rda")
load("data/land_use_pxsum_clustered.rda")
load("data/Bird_full_df.rda")
eco_txt <- read.table("data/Lica/US_ecoregions.txt", header = T, sep = "") %>%
  select(-FID, -Kilometers) %>%
  rename(segment = partition,
         ecoregion = Ecoregion)

bbs2021 <- bbs2021 %>%
  select(-segment) %>%
  rename(segment = partition)

# --- try to get IUCN status and population trend from bird life data zone ----

# install.packages("rvest")
library(rvest)
library(stringr)

base_url <- "http://datazone.birdlife.org/species/search"

scrape_bird_info <- function(species_name) {
  # Create the full URL with the search query
  
  species_name.str <- str_replace_all(species_name, "_", " ")
  
  search_url <- paste0(base_url, "?q=", URLencode(species_name.str))
  
  # Fetch the webpage
  webpage <- read_html(search_url)
  
  # Extract the IUCN status
  iucn_status <- webpage %>% 
    html_nodes(".tbl_species tr:nth-child(1) .td_common") %>% 
    html_text() %>% 
    trimws()
  
  # Extract the population trend
  population_trend <- webpage %>% 
    html_nodes(".tbl_species tr:nth-child(2) .td_common") %>% 
    html_text() %>% 
    trimws()
  
  # Return the results as a named list
  list(IUCN_Status = iucn_status, Population_Trend = population_trend)
}


# tst <- scrape_bird_info(BBS_df$)
# results_list <- lapply(BBS_df$animal_jetz, scrape_bird_info)


# ----- initial data transformations ----
BBS_df <- BBS_partition_abundance %>%
  rename(segment = partition) %>%
  bind_rows(bbs2021) %>%
  left_join(eco_txt, by = "segment") %>%
  left_join(Bird_full_df, by = "animal_jetz") %>%
  left_join(land_use_pxsum_complete, by = c("ecoregion","segment", "year"))

# BBS_df %>%
#   filter(!is.na(ecoregion)) %>%
#   group_by(segment) %>%
#   fill(cluster_nr, .direction = "down") %>%
#   fill(cluster_nr, .direction = "up") %>%
#   mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
#   ungroup() %>%
#   group_by(year, cluster.tmp, animal_jetz) %>%
#   nest(data = -c(year, cluster.tmp, animal_jetz)) %>%
#   mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance)))

rm(BBS_partition_abundance, bbs2021, Bird_full_df, land_use_pxsum_complete, eco_txt)

BBS_2021 <- BBS_df %>%
  filter(year == "2021")


# ---- Bird in cluster exploration, messy... ----
# to select ecoregion and bird of interest

# try with a couple of birds:
# Petrochelidon_pyrrhonota, Quiscalus_quiscula, Turdus_migratorius, Agelaius_phoeniceus
# Sturnus_vulgaris, Columba_livia

# ---- calculate for all the birds ----

bird.tmp <- BBS_df %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  # group_by(year, cluster.tmp, animal_jetz) %>%
  nest(data = -c(year, animal_jetz, ecoregion)) %>%
  mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance)))

# terrestrial_diurnal_df <- BBS_df %>%
#   filter(Nocturnal != 1 & Freshwater != 1 & Marine != 1) %>%
#   group_by(segment) %>%
#   fill(cluster_nr, .direction = "down") %>%
#   fill(cluster_nr, .direction = "up") %>%
#   mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
#   ungroup() %>%
#   group_by(year, cluster.tmp, animal_jetz) %>%
#   nest(data = -c(year, cluster.tmp, animal_jetz)) %>%
#   mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance)))

# Passeriformes <- BBS_df %>%
#   filter(ORDER == "Passeriformes") %>%
#   group_by(segment) %>%
#   fill(cluster_nr, .direction = "down") %>%
#   fill(cluster_nr, .direction = "up") %>%
#   mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
#   ungroup() %>%
#   group_by(year, cluster.tmp, animal_jetz) %>%
#   nest(data = -c(year, cluster.tmp, animal_jetz)) %>%
#   mutate(tot_abund = map_dbl(data, ~sum(.x$seg_abundance)))

# passeriformes_species <- unique(Passeriformes$animal_jetz)
# terrestrial_diurnal_species <- unique(terrestrial_diurnal_df$animal_jetz)

# bird.tmp %>%
#   select(-data) %>%
#   nest(PVA_data = -c(animal_jetz, cluster_tmp)) %>%
#   filter(!any(tot_abund == 0) & all(lengths(year >= 10)))
#   # mutate(lambda = )

# ---- Accipiter_cooperii ---- 

Accipiter_cooperii <- bird.tmp %>%
  filter(animal_jetz == "Accipiter_cooperii",
         year < 2021) %>%
  mutate(tot_abund.tmp = ifelse(tot_abund == 0, NA, tot_abund)) %>%
  na.omit() %>%
  select(-tot_abund.tmp) %>%
  group_by(ecoregion) %>%
  mutate(n_years = n()) %>%
  filter(n_years >= 10) %>%
  arrange(ecoregion, year) %>%
  filter(n_years >= 10) %>%
  filter(first(tot_abund) != 1)


ggplot(data = Accipiter_cooperii,
       aes(x = year, y = tot_abund, group = ecoregion, color = ecoregion)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Ecoregion") +
  guides() +
  theme_minimal()
  

ggplot(data = Accipiter_cooperii, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. cooperii Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # Increasing

# ----- Columba_livia ----

Columba_livia <- Bird %>%
  filter(animal_jetz == "Columba_livia") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment)) # very high abundance in 2001 at 35_035_3, and 2014 at 33_014_3
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Columba_livia,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Columba_livia, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "C. livia Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # declining

# Bird Life Datazone: Population size decreasing
# IUCN: Leas Concern

# ---- Sturnus_vulgaris ----

Sturnus_vulgaris <- Bird %>%
  filter(animal_jetz == "Sturnus_vulgaris") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment)) %>%
  # filter(ecoregion == "Columbia_Mountains/Northern_Rockies") # very high count in 2003 at 53_002_5, maybe a murmuration? 
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Sturnus_vulgaris,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Sturnus_vulgaris, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "S. vulgaris Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # declining, but maybe that's good because it's invasive?

# Bird Life Datazone: Population size decreasing
# IUCN: Least Concern

# ---- Agelaius_phoeniceus ----

Agelaius_phoeniceus <- Bird %>%
  filter(animal_jetz == "Agelaius_phoeniceus") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment)) %>%
  # filter(ecoregion == "Sonoran_Desert") # very high density in Sonoran Desert, especially on route 14_089 
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Agelaius_phoeniceus,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Agelaius_phoeniceus, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. phoenicus Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # declining

# Bird Life Datazone: Population trend decreasing
# IUCN: Least Concern


# ---- Turdus_migratorius ----

Turdus_migratorius <- Bird %>%
  filter(animal_jetz == "Turdus_migratorius") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment))
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Turdus_migratorius,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Turdus_migratorius, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "T. migratorius Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # declining

# Bird Life Datazone: Population trend stable
# IUCN: Least Concern

# ----- Quiscalus_quiscula -----

Quiscalus_quiscula <- Bird %>%
  filter(animal_jetz == "Quiscalus_quiscula") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # filter(!(seg_abundance > 1000)) %>% # to filter out the
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")
  # mutate(tot_abund = ifelse(tot_abund == 0, NA, tot_abund))

Quiscalus_quiscula %>%
  filter(tot_abund > 1000)

Bird %>%
  filter(animal_jetz == "Quiscalus_quiscula") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  filter(cluster.tmp == "Middle_Atlantic_Coastal_Plain_1") %>%
  arrange(desc(seg_abundance)) %>%
  filter(year == "2001") %>%
  print(n=60) 
# ungroup() %>%
# summarize(tot_abund = sum(seg_abundance))

ggplot(data = Quiscalus_quiscula,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Quiscalus_quiscula, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "Q. quiscula Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # strongly declining

# Bird Life datazone says that the population size is decreasing
# IUCN: Near Threatened

# ---- Petrochelidon_pyrrhonota ----

Petrochelidon_pyrrhonota <- Bird %>%
  filter(animal_jetz == "Petrochelidon_pyrrhonota") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  filter(!(seg_abundance > 1000)) %>% # to filter out the outliers --> one segment has > 1000 individuals but only in 2004 and 2006
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")
# mutate(tot_abund = ifelse(tot_abund == 0, NA, tot_abund))

ggplot(data = Petrochelidon_pyrrhonota, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "P. pyrrhonota Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal()

ggplot(data = Petrochelidon_pyrrhonota,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

Petrochelidon_pyrrhonota %>%
  group_by(year) %>%
  summarize(all_abund = sum(tot_abund)) %>%
  # print(n=30)
  ggplot(aes(x=year, y = all_abund)) +
  geom_smooth() +
  labs(title = "P. pyrrhonota Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # increase from 2000-2014 and then fast decline
# Bird Life datazone says that the population size is increasing
# IUCN: Least Concern

Bird %>%
  filter(animal_jetz == "Petrochelidon_pyrrhonota") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  summarise(tot_abund = sum(seg_abundance), .groups = "drop") %>%
  mutate(tot_abund = ifelse(tot_abund == 0, NA, tot_abund))


# ---- Investigate species that are VU in IUCN ---- 

# ---- Euphagus_carolinus ----
Euphagus_carolinus  <- Bird %>%
  filter(animal_jetz == "Euphagus_carolinus") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment)) # Only three observations, in 2 indiv in 2011, 2 in 2017, 1 in 2019
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Euphagus_carolinus, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. spragueii Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # very small pop size, almost 0, fluctating

# Bird Life Datazone: Population size decreasing
# IUCN: Endangered

# ---- Spizella_wortheni ---- 

Spizella <- Bird %>%
  # filter(animal_jetz == "Spizella_wortheni")
  filter(grepl("Spizella", animal_jetz, ignore.case = TRUE))
  # group_by(segment) %>%
  # fill(cluster_nr, .direction = "down") %>%
  # fill(cluster_nr, .direction = "up") %>%
  # mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  # ungroup() %>%
  # group_by(year, cluster.tmp) %>%
  # arrange(desc(segment))
  # summarise(tot_abund = sum(seg_abundance), .groups = "drop")

unique(Spizella$animal_jetz)

ggplot(data = Agelaius_tricolor,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Agelaius_tricolor, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. spragueii Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # small pop size, fluctating

# Bird Life Datazone: Population size decreasing
# IUCN: Endangered



# ---- Agelaius tricolor ----

Agelaius_tricolor <- Bird %>%
  filter(animal_jetz == "Agelaius_tricolor") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment)) # 4 observations with > 100 individuals observed... especially one with > 300 
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")

ggplot(data = Agelaius_tricolor,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Agelaius_tricolor, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. spragueii Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # small pop size, fluctating

# Bird Life Datazone: Population size decreasing
# IUCN: Endangered

# ---- Anthus spragueii-----

Anthus_spragueii <- Bird %>%
  filter(animal_jetz == "Anthus_spragueii") %>%
  group_by(segment) %>%
  fill(cluster_nr, .direction = "down") %>%
  fill(cluster_nr, .direction = "up") %>%
  mutate(cluster.tmp = paste0(ecoregion, "_", cluster_nr)) %>%
  ungroup() %>%
  group_by(year, cluster.tmp) %>%
  # arrange(desc(segment))
  summarise(tot_abund = sum(seg_abundance), .groups = "drop")
  # 
ggplot(data = Anthus_spragueii,
       aes(x = year, y = tot_abund, group = cluster.tmp, color = cluster.tmp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Population Size Trajectories",
       x = "Time Steps",
       y = "Population Size",
       color = "Cluster") +
  guides(color = FALSE) +
  theme_minimal()

ggplot(data = Anthus_spragueii, aes(x = year, y = tot_abund)) +
  geom_smooth() +  # Add a linear trend line
  labs(title = "A. spragueii Population Size Trend",
       x = "Time Steps",
       y = "Population Size") +
  theme_minimal() # declining, very small pop size, fluctations

# Bird Life Datazone: Population size decreasing
# IUCN: Vulnerable

#------ get data ---- 

# Ecoregions with high bird abundances: Central Corn Belt Plains, Sonoran Desert, Central Great Plains, Ridge and Valley

data.now <- Accipiter_cooperii

bird.now <- data.now %>%
  filter(ecoregion == "North_Central_Hardwood_Forests", 
    # cluster_nr == 1, # select cluster of interest
    year < 2021)

N <- bird.now$tot_abund
# N[N == 0] <- NA # the calculations can't handle 0's, because in R division by 0 = Inf, and log(0) = -Inf
Years <- bird.now$year


# Step 0: Plot the data     ----     
#====================================#


ggplot(data=bird.now, aes(x=year, y=tot_abund)) +
  geom_line(size = 1, color= "#00AFBB") +
  geom_point(color="#00AFBB") +
  ylab("Population size") +
  xlab("Time steps")

# Step 1: Calculate "lambda", "mu" and "sigma" ----
#==========================================#

# Population growth rate (lambda)
lambda <- N[-1]/N[-length(N)] #

# Intrinsic rate of increase
logN <- log(N[-1]/N[-length(N)])  #  this is the log of lambda (population growth rate)

# mean rate of increase
mu <- mean(logN, na.rm = TRUE)  # lower than zero, so the population is declining and will die out

# environmental variation
sigma2 <- var(logN, na.rm = TRUE)   # the higher, the higher the risk of extinction

# Step 2: Project the population ----
#================================#

n0 <- N[1] # accumulated (sum) number of individuals in 2000
T <- 50 # time iterations to project over
runs <- 500 # number of population trajectories
stoch.pop <- matrix(NA, T, runs)
stoch.pop[1,] <- n0 

# set the quasi-extinction threshold
Ne <- 2 # check in the literature

# projections

for (i in 1:runs){
  for (t in 2:T){
    r <- rnorm(n = 1, mean = mu, sd = sqrt(sigma2))
    lambda.now <- exp(r)
    stoch.pop[t,i] <- stoch.pop[(t-1),i] * lambda.now
    if(stoch.pop[t,i] <= Ne) {
      stoch.pop[t,i] <- 0
      i < i+1}
  }
}

# Step 3: Examine the results    ----
#================================#


# Plot population size trajectories
matplot(log(stoch.pop), type="l",
        xlab="log Population size",
        ylab="Time steps")


# Plot population size at the last time step
lastN <- data.frame(pop = stoch.pop[T,])
summary(lastN)
ggplot(lastN, aes(x = pop)) +
  geom_histogram(bins = 40) +
  xlab("Population size after 50 years")


# Mean population size simulations with confidence intervals
pop.mean <- apply(stoch.pop,1,mean, na.rm=T)
log.pop.sd <- apply(log(stoch.pop+0.00001),1,sd,na.rm=T)
ucl <- exp(log(pop.mean)+1.96*log.pop.sd)  # upper confidence interval
lcl <- exp(log(pop.mean)-1.96*log.pop.sd)  # lower confidence interval

dataproj <- data.frame(years=(bird.now$year[1]:(bird.now$year[1] + (T-1))), 
                       pop.mean=pop.mean,
                       low=lcl,
                       up=ucl,
                       N=c(bird.now$tot_abund, rep(NA, T-length(bird.now$tot_abund))))

p <- ggplot(dataproj, aes(x = years)) +
  geom_line(aes(y = log(pop.mean))) +
  geom_ribbon(aes(ymin = log(low), ymax = log(up)), alpha = 0.2)

p + geom_line(aes(y=log(N), colour="red")) +
  ylab("log population size") +
  xlab("Time steps")

# Step 4: Quantify extinction risk  ----
#===================================#

# probability to reach the extinction criteria
Pr.ext <- (sum(lastN <= Ne) / runs)*100  # percentage of trajectories reaching the extinction threshold before the end of the simulation
Pr.ext

# Cumulative extinction probability per time step
ex = extCDF(mu, sigma2, Nc=n0, Ne=Ne)

# Use bootstrap to get confidence intervals
CIext <- countCDFxt(mu, sigma2, nt=T-1, Nc=n0, Ne=Ne, tmax=T, Nboot=500, plot=TRUE)
Prext <- data.frame(years=(bird.now$year[1]:(bird.now$year[1] + (T-1))), 
                    m=CIext$Gbest,
                    low=CIext$Glo,
                    up=CIext$Gup)


# Plot the accumulative extinction risk
ggplot(Prext, aes(x=years)) +
  geom_point(aes(y=m)) +
  geom_line(aes(y=m)) +
  geom_ribbon(aes(ymin=low, ymax=up), alpha=0.2) +
  xlab("Years") + ylab("Quasi-extinction probability")



# Step 5: Estimate time to extinction  ----
#======================================#

# Time to reach extinction for extinct population
maxt<-NULL # empty vector to store results
for(i in 1:runs){
  N = stoch.pop[,i]
  maxt[i] <- max(which(N>0))
}


# Time to reach extinction for pseudo-extinct population
time.ext <- maxt[maxt<T]
summary(time.ext)
median(time.ext)  # median time at extinction for extinct population; the mean produces overestimation of extinction time because of few populations growing fast 

# Plot time to extinction
df <- data.frame(time.ext=time.ext)
ggplot(df, aes(x = time.ext)) +
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept=median(time.ext)), colour="red") +
  xlab("Time to extinction")
