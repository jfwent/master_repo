# BBS data preparation
# Author: Jon Went, jwent@ethz.ch
# date: 1.9.2023

# --- load library ----
library(tidyverse)

# ---- load data ----

load("data/Lica/BBS_partition_abundance.rda")
load("data/Gen_length/Bird_full_df.rda")
load("data/fun_traits/FuncDat.rda")

load("data/Climate/climate_df.rda")
load("data/hfp_t1_t2.rda")
load("data/Land_use/land_use_area_t1_t2.rda")

# ---- prepare and filter for species traits ----

BBS.species.all <- sort(unique(BBS_partition_abundance$animal_jetz)) # 565 species in the data set

get_species_data = function() {
  data_path <- paste('./data/', 'bbs', '_species.csv', sep = "")
  if (file.exists(data_path)) {
    return(read.csv(data_path))
  }else{
    species_table=db_engine(action = 'read', sql_query = 'SELECT * FROM breed_bird_survey_species')
    write.csv(species_table, file = data_path, row.names = FALSE, quote = FALSE)
    #save_provenance(species_table)
    return(species_table)
  }
}

filter_species <- function(df){
  species_table = get_species_data()
  
  is_unidentified = function(names) {
    #Before filtering, account for this one hybrid of 2 subspecies so it's kept
    names[names=='auratus auratus x auratus cafer']='auratus auratus'
    grepl('sp\\.| x |\\/', names)
  }
  
  valid_taxa = species_table %>%
    filter(!is_unidentified(species)) %>%
    filter(aou > 2880) %>%
    filter(aou < 3650 | aou > 3810) %>%
    filter(aou < 3900 | aou > 3910) %>%
    filter(aou < 4160 | aou > 4210) %>%
    filter(aou != 7010)
  
  filter(df, AOU %in% valid_taxa$aou)
}

bbs.filtered <- filter_species(BBS_partition_abundance)
my.birds <- sort(unique(bbs.filtered$animal_jetz))

# length(unique(bbs.filtered$animal_jetz)) # 387 species kept
# length(unique(bbs.filtered$partition)) # 2976 segments

# sp_traits <- FuncDat %>%
#   select(-BodyMass.Value) %>%
#   full_join(Bird_full_df, by = "animal_jetz") %>%
#   filter(animal_jetz %in% BBS.species.all) %>%
#   select(-c(22:36, 40:43, Nocturnal.y, Diet))
# 
# terrestrial_birds <- sp_traits  %>%
#   filter(Nocturnal.x != 1,
#          PelagicSpecialist != 1,
#          Marine != 1,
#          Freshwater != 1)
# 
# nocturnal_birds <- sp_traits %>%
#   filter(Nocturnal.x == 1)
# 
# water_birds <- sp_traits %>%
#   filter((Marine == 1 | Freshwater == 1 | PelagicSpecialist == 1),
#          Nocturnal.x != 1)
# 
# # check the water birds wikipedia page if they really are seabirds, waders, oder otherwise strongly associated with waterbodies
# water_birds <- water_birds %>%
#   mutate(wikipedia_links = paste("https://en.wikipedia.org/wiki/", animal_jetz)) %>%
#   mutate(wikipedia_links = gsub(" ", "", wikipedia_links)) %>%
#   arrange(animal_jetz)
# 
# # for (link in water_birds$wikipedia_links) {
# #   system(paste("open -a 'Google Chrome' ", link))
# # }
# 
# # in my opinion wrongly excluded from the terrestrial birds:
# # Agelaius_phoeniceus, Agelaius tricolor, Anthus rubescens, Cistothorus platensis, Corvus brachyrhynchos, Crotophaga sulcirostris
# # Elanus leucurus, Empidonax alnorum,  Eremophila alpestris, Falco columbarius, Falco femoralis, Falco mexicanus, Falco peregrinus,
# # Falco rusticolus, Falco sparverius, Hirundo rustica, Lanius excubitor, Melanerpes carolinus, Melospiza melodia, Myiarchus cinerascens,
# # Myiarchus crinitus, Myiarchus tuberculifer, Myiarchus tyrannulus, Myiodynastes luteiventris, Oenanthe oenanthe, Pachyramphus aglaiae,
# # Passer domesticus, Petrochelidon pyrrhonota, Phylloscopus borealis, Progne subis, Quiscalus mexicanus, Quiscalus quiscula,
# # Riparia riparia, Sialia sialis, Spizella pallida, Stelgidopteryx serripennis, Sturnus vulgaris, Tachycineta bicolor, Tachycineta thalassina,
# # Tyrannus crassirostris, Tyrannus melancholicus, Tyrannus tyrannus, Vermivora chrysoptera, Zenaida macroura, Zonotrichia atricapilla,
# # Zonotrichia leucophrys
# 
# # unclear if wetland bird and therefore exclude:
# # Contopus cooperi, Empidonax difficilis, Empidonax flaviventris, Empidonax traillii, Empidonax virescens,
# # Euphagus carolinus, Euphagus cyanocephalus, Geothlypis trichas, Icterus graduacauda, Limnothlypis swainsonii, Melospiza lincolnii,
# 
# missing_birds <- c("Agelaius_phoeniceus", "Agelaius_tricolor", "Anthus_rubescens", "Cistothorus_platensis", "Corvus_brachyrhynchos",
#                    "Crotophaga_sulcirostris", "Elanus_leucurus", "Empidonax_alnorum",  "Eremophila_alpestris", "Falco_columbarius",
#                    "Falco_femoralis",
#                    "Falco_mexicanus", "Falco_peregrinus", "Falco_rusticolus", "Falco_sparverius", "Hirundo_rustica", "Lanius_excubitor",
#                    "Melanerpes_carolinus", "Melospiza_melodia", "Myiarchus_cinerascens",
#                    "Myiarchus_crinitus", "Myiarchus_tuberculifer", "Myiarchus_tyrannulus", "Myiodynastes_luteiventris", "Oenanthe_oenanthe",
#                    "Pachyramphus_aglaiae",
#                    "Passer_domesticus", "Petrochelidon_pyrrhonota", "Phylloscopus_borealis", "Progne_subis", "Quiscalus_mexicanus", "Quiscalus_quiscula",
#                    "Riparia_riparia", "Sialia_sialis", "Spizella_pallida", "Stelgidopteryx_serripennis", "Sturnus_vulgaris", "Tachycineta_bicolor",
#                    "Tachycineta_thalassina",
#                    "Tyrannus_crassirostris", "Tyrannus_melancholicus", "Tyrannus_tyrannus", "Vermivora_chrysoptera", "Zenaida_macroura",
#                    "Zonotrichia_atricapilla",
#                    "Zonotrichia_leucophrys")
# 
# unclear_birds <- c("Contopus_cooperi", "Empidonax_difficilis", "Empidonax_flaviventris", "Empidonax_traillii",
#                    "Empidonax_virescens",
#                    "Euphagus_carolinus", "Euphagus_cyanocephalus", "Geothlypis_trichas", "Icterus_graduacauda",
#                    "Limnothlypis_swainsonii", "Melospiza_lincolnii")
# 
# water_birds_checked <- sp_traits %>%
#   filter((Marine == 1 | Freshwater == 1 | PelagicSpecialist == 1) & !(animal_jetz %in% missing_birds),
#          Nocturnal.x != 1)
# 
# my_birds_df <- sp_traits %>%
#   filter(!(animal_jetz %in% water_birds_checked$animal_jetz),
#          Nocturnal.x != 1) %>%
#   arrange(animal_jetz)
# 
# my.birds <- my_birds_df$animal_jetz # I keep 372 species
# 
# rm(terrestrial_birds, water_birds, water_birds_checked,
#    nocturnal_birds, sp_traits, missing_birds, unclear_birds, my_birds_df)

rm(Bird_full_df, FuncDat)
rm(bbs.filtered)

# ---- BBS t1 and t2, conservative detectability ----

BBS.stable.presence.t1 <- BBS_partition_abundance %>%
  filter(year %in% c(2000:2002)) %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund),2),
            abund.geom.mean = exp(mean(log(seg_abund)))) %>%
  na.omit() %>%
  filter(animal_jetz %in% my.birds) %>%
  mutate(year = 2001) %>%
  relocate(year)

BBS.stable.absence.t1 <- BBS_partition_abundance %>%
  filter(year %in% c(2000:2002)) %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund == 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund),2),
            abund.geom.mean = exp(mean(log(seg_abund)))) %>%
  na.omit() %>%
  filter(animal_jetz %in% my.birds) %>%
  mutate(year = 2001) %>%
  relocate(year)

BBS.stable.t1 <- rbind(BBS.stable.absence.t1, BBS.stable.presence.t1)

BBS.stable.presence.t2 <- BBS_partition_abundance %>%
  filter(animal_jetz %in% BBS.stable.t1$animal_jetz,
         year %in% c(2017:2019)) %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund > 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund),2),
            abund.geom.mean = exp(mean(log(seg_abund)))) %>%
  na.omit() %>%
  mutate(year = 2019) %>%
  relocate(year)

BBS.stable.absence.t2 <- BBS_partition_abundance %>%
  filter(animal_jetz %in% BBS.stable.t1$animal_jetz,
         year %in% c(2017:2019)) %>%
  rename(segment = partition) %>%
  group_by(year, segment, animal_jetz) %>%
  summarize(seg_abund = sum(seg_abundance)) %>%
  filter(seg_abund == 0) %>%
  ungroup() %>%
  group_by(segment, animal_jetz) %>%
  mutate(total_entries = n()) %>%
  filter(total_entries == 3) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund),2),
            abund.geom.mean = exp(mean(log(seg_abund)))) %>%
  na.omit() %>%
  mutate(year = 2019) %>%
  relocate(year)

BBS.stable.t2 <- rbind(BBS.stable.absence.t2, BBS.stable.presence.t2)

BBS.stable.full <- rbind(BBS.stable.t1, BBS.stable.t2)

rm(filter_species, get_species_data)

# ---- check the species numbers ----

# length(unique(BBS.stable.absence.t1$animal_jetz)) # 332 species stable absence T1
# length(unique(BBS.stable.presence.t1$animal_jetz)) # 310 species stable presence T1
# length(unique(BBS.stable.absence.t2$animal_jetz)) # 315 species stable absence T2
# length(unique(BBS.stable.presence.t2$animal_jetz)) # 305 species stable presence T2
# length(unique(BBS.stable.full$animal_jetz)) # 333 species stable
# length(unique(BBS.stable.full$segment)) # 2937

rm(BBS.stable.absence.t1, BBS.stable.absence.t2, BBS.stable.presence.t1, BBS.stable.presence.t2)

rm(BBS.species.all)

save(BBS.stable.full, file = "data/BBS_stable_t1_t2.rda")

rm(BBS.stable.t1, BBS.stable.t2)

rm(my.birds)


# ---- filter for complete cases ----

# ---- prepare the bioclim data ---

climate.df <- climate_df %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean"),
         -c(pr.sum.mean, cmi.annual.mean, cmi.diff.mean)); rm(climate_df)

hfp.df <- hfp.full %>%
  # filter(year == 2001) %>%
  select(-c(contains("var"), contains("median")), contains("mean")) %>%
  mutate(hfp.mean = ifelse(is.nan(hfp.mean) == T, NA, hfp.mean)); rm(hfp.full)

land.use.df <- hfp.df %>%
  # filter(year == 2001) %>%
  left_join(land_use_area, by = c("segment", "year")) %>%
  select(-c(ecoregion, tot.area.m2, route)) %>%
  mutate(across(
    .cols = contains("area"),
    .fns = c(
      log = \(x) log(x + 900))
    ,
    .names = "{.col}.{.fn}"
  )) %>%
  select(year, segment, contains("log"), contains("hfp")); rm(land_use_area, hfp.df)

bioclim.df <- climate.df %>%
  left_join(land.use.df, by = c("year", "segment")); rm(climate.df, land.use.df)

dbioclim <- bioclim.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  reframe(
    across(
      .cols = matches('mean') | matches('log'),
      # .fns = \(.) lag(.)-.,
      .fns = ~ ifelse(!is.na(lag(.)), lag(.) - ., NA),
      .names = "delta.{col}"
    )
  ) %>%
  na.omit()


# ---- complete cases

BBS.stable.full.complete <- BBS.stable.full %>%
  left_join(bioclim.df, by = c("year", "segment")) %>%
  na.omit()  %>%
  select(-c(abund.mean, abund.median, abund.var))

# length(unique(BBS.stable.full.complete$segment)) # 1709 segments
# length(unique(BBS.stable.full.complete$animal_jetz)) # 279 bird species


# ---- filter out with min number of occurences -----

# --- min number of occurrences is 6

BBS.stable.full.min6 <- BBS.stable.full.complete %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 6) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min6$animal_jetz)) # 180 species retained
# length(unique(BBS.stable.full.min6$segment)) # 1709 segments retained

# hist(BBS.stable.full.min6$abund.geom.mean)

# ----- min number of occurrences is 10

BBS.stable.full.min10 <- BBS.stable.full.complete %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 10) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min10$animal_jetz)) # 168 species retained
# length(unique(BBS.stable.full.min10$segment)) # 1709 segments

# ---- min number of occurrences is 40

BBS.stable.full.min40 <- BBS.stable.full.complete %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min40$animal_jetz)) # 101 species retained
# length(unique(BBS.stable.full.min40$segment)) # 1709 segments retained

# ---- delta abundances without double 0's for complete cases ------

d.abund.min6 <- BBS.stable.full.min6 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

# length(unique(d.abund.min6$animal_jetz)) # 179 species
# length(unique(d.abund.min6$segment)) # 1492 segments

# hist(d.abund.min6$d.abund)

# d.abund.min6 %>%
#   ggplot2::ggplot(aes(x=d.abund))+
#   geom_density(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
#   ggtitle("Delta_abundance distribution, min(n.occur = 6)") +
#   theme_bw()

d.abund.min10 <- BBS.stable.full.min10 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

# length(unique(d.abund.min10$animal_jetz)) # 168 species
# length(unique(d.abund.min10$segment)) # 1492 segments

d.abund.min40 <- BBS.stable.full.min40 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  select(year, segment, animal_jetz, abund.geom.mean) %>%
  mutate(delta.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(delta.abund)) %>%
  select(-abund.geom.mean) %>%
  left_join(dbioclim, by = "segment") %>%
  na.omit()

# length(unique(d.abund.min40$animal_jetz)) # 101 species
# length(unique(d.abund.min40$segment)) # 1490 segments

# ---- save files -----

save(BBS.stable.full.min6, file = "data/BBS.full.stable.min6.rda")
save(BBS.stable.full.min10, file = "data/BBS.full.stable.min10.rda")
save(BBS.stable.full.min40, file = "data/BBS.full.stable.min40.rda")

save(d.abund.min6, file = "data/d.abund.min6.rda")
save(d.abund.min10, file = "data/d.abund.min10.rda")
save(d.abund.min40, file = "data/d.abund.min40.rda")


# =========== old -----

# ---- filter out min number of occurrences without filtering for complete cases ----
# ---- min number of occurrences is 6

BBS.stable.full.min6 <- BBS.stable.full %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 6) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min6$animal_jetz)) # 233 species retained
# length(unique(BBS.stable.full.min6$segment)) # 2937 segments retained

# hist(BBS.stable.full.min6$abund.geom.mean)

# ----- min number of occurrences is 10

BBS.stable.full.min10 <- BBS.stable.full %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 10) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min10$animal_jetz)) # 215 species retained
# length(unique(BBS.stable.full.min10$segment)) # 2937 segments

# ---- min number of occurrences is 40

BBS.stable.full.min40 <- BBS.stable.full %>%
  group_by(year, animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40) %>%
  ungroup() %>%
  group_by(animal_jetz) %>%
  mutate(n_years = n_distinct(year)) %>%
  filter(n_years != 1) %>%
  select(-c(n_years, tot_entries, tot_occur))

# length(unique(BBS.stable.full.min40$animal_jetz)) # 143 species retained
# length(unique(BBS.stable.full.min40$segment)) # 2937 segments retained

# ---- delta abundances without double 0's ------

# d.abund <- BBS.stable.full %>%
#   arrange(animal_jetz, segment) %>%
#   group_by(animal_jetz, segment) %>%
#   mutate(d.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
#   filter(all(abund.geom.mean !=0),
#          # !is.na(d.abund)
#          )

# hist(d.abund$d.abund) # stark um 0 gepoolt

# d.abund %>%
#   ggplot2::ggplot(aes(x=d.abund))+
#   geom_density(fill = "#69b3a2", color="#e9ecef", alpha=0.8)

# length(unique(d.abund$animal_jetz)) #261 species

d.abund.min6 <- BBS.stable.full.min6 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  mutate(d.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(d.abund))

length(unique(d.abund.min6$animal_jetz)) # 230 species
# length(unique(d.abund.min6$segment)) # 2380 segments

# hist(d.abund.min6$d.abund)

# d.abund.min6 %>%
#   ggplot2::ggplot(aes(x=d.abund))+
#   geom_density(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
#   ggtitle("Delta_abundance distribution, min(n.occur = 6)") +
#   theme_bw()

# length(unique(d.abund.min6$animal_jetz)) # 223 species

d.abund.min10 <- BBS.stable.full.min10 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  mutate(d.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(d.abund))

# length(unique(d.abund.min10$animal_jetz)) # 215 species
# length(unique(d.abund.min10$segment)) # 2380 segments

# hist(d.abund.min10$d.abund)

# d.abund.min10 %>%
#   ggplot2::ggplot(aes(x=d.abund))+
#   geom_density(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
#   ggtitle("Delta_abundance distribution, min(n.occur = 10)") +
#   theme_bw()

# length(unique(d.abund.min10$animal_jetz)) # 206 species

d.abund.min40 <- BBS.stable.full.min40 %>%
  arrange(animal_jetz, segment) %>%
  group_by(animal_jetz, segment) %>%
  mutate(d.abund = lag(abund.geom.mean) - abund.geom.mean) %>%
  filter(all(abund.geom.mean !=0),
         !is.na(d.abund))

length(unique(d.abund.min40$animal_jetz)) # 143 species
length(unique(d.abund.min40$segment)) # 2379 segments

# hist(d.abund.min40$d.abund)


# --- check full data set ----

d.bioclim <- bioclim.df %>%
  arrange(segment) %>%
  group_by(segment) %>%
  mutate(across(
    .cols = c(contains("mean") | contains("log")),
    .fns = \(x) lag(x) - x,
    .names = {"d----{col}"}
  )) %>%
  na.omit()

d.abund.min40.full <- d.abund.min40 %>% left_join(d.bioclim, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 40)

# length(unique(d.abund.min40.full$segment)) # 1489
# length(unique(d.abund.min40.full$animal_jetz)) # 74


d.abund.min10.full <- d.abund.min10 %>% left_join(d.bioclim, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 10)

# length(unique(d.abund.min10.full$segment)) # 1492
# length(unique(d.abund.min10.full$animal_jetz)) # 131

d.abund.min6.full <- d.abund.min6 %>% left_join(d.bioclim, by = "segment") %>%
  na.omit() %>%
  group_by(animal_jetz) %>%
  mutate(tot_entries = n(),
         tot_occur = sum(abund.geom.mean > 0)) %>%
  filter(tot_occur >= 6)

# length(unique(d.abund.min6.full$segment)) # 1492
# length(unique(d.abund.min6.full$animal_jetz)) # 150



# ---- BBS t1 and t2 -----

# BBS.t1 <- BBS_partition_abundance %>%
#   filter(animal_jetz %in% my.birds,
#          year %in% c(2000:2002)) %>%
#   rename(segment = partition,
#          seg_abund = seg_abundance) %>%
#   select(year, segment, animal_jetz, seg_abund) %>%
#   group_by(segment, animal_jetz) %>%
#   summarize(abund.mean = round(mean(seg_abund),2),
#             abund.median = round(median(seg_abund),2),
#             abund.var = round(var(seg_abund), 2),
#             abund.geom.mean = exp(mean(log(seg_abund)))) %>%
#   mutate(year = 2001) %>%
#   relocate(year)
# 
# BBS.t2 <- BBS_partition_abundance %>%
#   filter(animal_jetz %in% BBS.t1$animal_jetz,
#          year %in% c(2017:2019)) %>%
#   rename(segment = partition,
#          seg_abund = seg_abundance) %>%
#   select(year, segment, animal_jetz, seg_abund) %>%
#   group_by(segment, animal_jetz) %>%
#   summarize(abund.mean = round(mean(seg_abund),2),
#             abund.median = round(median(seg_abund),2),
#             abund.var = round(var(seg_abund), 2),
#             abund.geom.mean = exp(mean(log(seg_abund)))) %>%
#   mutate(year = 2019) %>%
#   relocate(year)
# 
# BBS.full <- rbind(BBS.t1, BBS.t2)
# 
# rm(BBS.t1, BBS.t2)
# 
# save(BBS.full, file = "data/BBS_t1_t2.rda")
