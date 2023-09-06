# BBS data preparation
# Author: Jon Went, jwent@ethz.ch
# date: 1.9.2023

# --- load library ----
library(tidyverse)

# ---- load data ----

load("data/Lica/BBS_partition_abundance.rda")
load("data/Gen_length/Bird_full_df.rda")
load("data/fun_traits/FuncDat.rda")

#---- prepare and filter for species traits ----

BBS.species.all <- unique(BBS_partition_abundance$animal_jetz)

sp_traits <- FuncDat %>%
  select(-BodyMass.Value) %>%
  full_join(Bird_full_df, by = "animal_jetz") %>%
  filter(animal_jetz %in% BBS.species.all) %>%
  select(-c(22:36, 40:43, Nocturnal.y, Diet))

terrestrial_birds <- sp_traits  %>%
  filter(Nocturnal.x != 1,
         PelagicSpecialist != 1,
         Marine != 1,
         Freshwater != 1)

nocturnal_birds <- sp_traits %>%
  filter(Nocturnal.x == 1)

water_birds <- sp_traits %>%
  filter((Marine == 1 | Freshwater == 1 | PelagicSpecialist == 1),
         Nocturnal.x != 1)

# check the water birds wikipedia page if they really are seabirds, waders, oder otherwise strongly associated with waterbodies
water_birds <- water_birds %>%
  mutate(wikipedia_links = paste("https://en.wikipedia.org/wiki/", animal_jetz)) %>%
  mutate(wikipedia_links = gsub(" ", "", wikipedia_links)) %>%
  arrange(animal_jetz)

# for (link in water_birds$wikipedia_links) {
#   system(paste("open -a 'Google Chrome' ", link))
# }

# in my opinion wrongly excluded from the terrestrial birds:
# Agelaius_phoeniceus, Agelaius tricolor, Anthus rubescens, Cistothorus platensis, Corvus brachyrhynchos, Crotophaga sulcirostris
# Elanus leucurus, Empidonax alnorum,  Eremophila alpestris, Falco columbarius, Falco femoralis, Falco mexicanus, Falco peregrinus,
# Falco rusticolus, Falco sparverius, Hirundo rustica, Lanius excubitor, Melanerpes carolinus, Melospiza melodia, Myiarchus cinerascens,
# Myiarchus crinitus, Myiarchus tuberculifer, Myiarchus tyrannulus, Myiodynastes luteiventris, Oenanthe oenanthe, Pachyramphus aglaiae,
# Passer domesticus, Petrochelidon pyrrhonota, Phylloscopus borealis, Progne subis, Quiscalus mexicanus, Quiscalus quiscula,
# Riparia riparia, Sialia sialis, Spizella pallida, Stelgidopteryx serripennis, Sturnus vulgaris, Tachycineta bicolor, Tachycineta thalassina,
# Tyrannus crassirostris, Tyrannus melancholicus, Tyrannus tyrannus, Vermivora chrysoptera, Zenaida macroura, Zonotrichia atricapilla,
# Zonotrichia leucophrys

# unclear if wetland bird and therefore exclude:
# Contopus cooperi, Empidonax difficilis, Empidonax flaviventris, Empidonax traillii, Empidonax virescens,
# Euphagus carolinus, Euphagus cyanocephalus, Geothlypis trichas, Icterus graduacauda, Limnothlypis swainsonii, Melospiza lincolnii,

missing_birds <- c("Agelaius_phoeniceus", "Agelaius_tricolor", "Anthus_rubescens", "Cistothorus_platensis", "Corvus_brachyrhynchos",
                   "Crotophaga_sulcirostris", "Elanus_leucurus", "Empidonax_alnorum",  "Eremophila_alpestris", "Falco_columbarius",
                   "Falco_femoralis",
                   "Falco_mexicanus", "Falco_peregrinus", "Falco_rusticolus", "Falco_sparverius", "Hirundo_rustica", "Lanius_excubitor",
                   "Melanerpes_carolinus", "Melospiza_melodia", "Myiarchus_cinerascens",
                   "Myiarchus_crinitus", "Myiarchus_tuberculifer", "Myiarchus_tyrannulus", "Myiodynastes_luteiventris", "Oenanthe_oenanthe",
                   "Pachyramphus_aglaiae",
                   "Passer_domesticus", "Petrochelidon_pyrrhonota", "Phylloscopus_borealis", "Progne_subis", "Quiscalus_mexicanus", "Quiscalus_quiscula",
                   "Riparia_riparia", "Sialia_sialis", "Spizella_pallida", "Stelgidopteryx_serripennis", "Sturnus_vulgaris", "Tachycineta_bicolor",
                   "Tachycineta_thalassina",
                   "Tyrannus_crassirostris", "Tyrannus_melancholicus", "Tyrannus_tyrannus", "Vermivora_chrysoptera", "Zenaida_macroura",
                   "Zonotrichia_atricapilla",
                   "Zonotrichia_leucophrys")

unclear_birds <- c("Contopus_cooperi", "Empidonax_difficilis", "Empidonax_flaviventris", "Empidonax_traillii",
                   "Empidonax_virescens",
                   "Euphagus_carolinus", "Euphagus_cyanocephalus", "Geothlypis_trichas", "Icterus_graduacauda",
                   "Limnothlypis_swainsonii", "Melospiza_lincolnii")

water_birds_checked <- sp_traits %>%
  filter((Marine == 1 | Freshwater == 1 | PelagicSpecialist == 1) & !(animal_jetz %in% missing_birds),
         Nocturnal.x != 1)

my_birds_df <- sp_traits %>%
  filter(!(animal_jetz %in% water_birds_checked$animal_jetz),
         Nocturnal.x != 1) %>%
  arrange(animal_jetz)

my.birds <- my_birds_df$animal_jetz

rm(terrestrial_birds, water_birds, water_birds_checked,
   nocturnal_birds, sp_traits, missing_birds, unclear_birds, my_birds_df)

rm(Bird_full_df, FuncDat)

# ---- BBS t1 and t2 -----

BBS.t1 <- BBS_partition_abundance %>%
  filter(animal_jetz %in% my.birds,
         year %in% c(2000:2002)) %>%
  rename(segment = partition,
         seg_abund = seg_abundance) %>%
  select(year, segment, animal_jetz, seg_abund) %>%
  group_by(segment, animal_jetz) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund), 2)) %>%
  mutate(year = 2001) %>%
  relocate(year)

BBS.t2 <- BBS_partition_abundance %>%
  filter(animal_jetz %in% BBS.t1$animal_jetz,
         year %in% c(2017:2019)) %>%
  rename(segment = partition,
         seg_abund = seg_abundance) %>%
  select(year, segment, animal_jetz, seg_abund) %>%
  group_by(segment, animal_jetz) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund), 2)) %>%
  mutate(year = 2019) %>%
  relocate(year)

BBS.full <- rbind(BBS.t1, BBS.t2)

rm(BBS.t1, BBS.t2)

save(BBS.full, file = "data/BBS_t1_t2.rda")

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
            abund.var = round(var(seg_abund),2)) %>%
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
            abund.var = round(var(seg_abund),2)) %>%
  na.omit() %>%
  filter(animal_jetz %in% my.birds) %>%
  mutate(year = 2001) %>%
  relocate(year)

BBS.stable.t1 <- rbind(BBS.stable.absence.t1, BBS.stable.presence.t1)

BBS.stable.t2.tmp <- BBS_partition_abundance %>%
  filter(animal_jetz %in% BBS.stable.t1$animal_jetz,
         year %in% c(2017:2019)) %>%
  rename(segment = partition,
         seg_abund = seg_abundance) %>%
  select(year, segment, animal_jetz, seg_abund) %>%
  group_by(segment, animal_jetz) %>%
  summarize(abund.mean = round(mean(seg_abund),2),
            abund.median = round(median(seg_abund),2),
            abund.var = round(var(seg_abund), 2)) %>%
  filter(animal_jetz %in% my.birds) %>%
  mutate(year = 2019) %>%
  relocate(year)

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
            abund.var = round(var(seg_abund),2)) %>%
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
            abund.var = round(var(seg_abund),2)) %>%
  na.omit() %>%
  mutate(year = 2019) %>%
  relocate(year)

BBS.stable.t2 <- rbind(BBS.stable.absence.t2, BBS.stable.presence.t2)

BBS.stable.full <- rbind(BBS.stable.t1, BBS.stable.t2)

# my.birds.stable.absence.t1 <- unique(BBS.stable.absence.t1$animal_jetz) # 320 species
my.birds.stable.presence.t1 <- unique(BBS.stable.presence.t1$animal_jetz) # 297 species
my.birds.stable.absence.t2 <- unique(BBS.stable.absence.t2$animal_jetz) # 320 species
my.birds.stable.presence.t2 <- unique(BBS.stable.presence.t2$animal_jetz) # 291 species
my.birds.stable.full <- unique(BBS.stable.full$animal_jetz) # 320 species

rm(BBS.stable.absence.t1, BBS.stable.absence.t2, BBS.stable.presence.t1, BBS.stable.presence.t2)
rm(BBS.stable.t1, BBS.stable.t2)

rm(my.birds,my.birds.stable.presence.t1,my.birds.stable.absence.t2,my.birds.stable.presence.t2,my.birds.stable.full,my.birds.stable.absence.t1)
rm(BBS.species.all)

save(BBS.stable.full, file = "data/BBS_stable_t1_t2.rda")

# ---- check the species numbers ----

# BBS.species.all: 565
# all species present in the BBS dataset from 2000-2019

# my.birds: 372
#all terrestrial and diurnal species that are in the BBS dataset

# BBS.species.t1 <- unique(BBS.t1$animal_jetz)
# 351
# all terrestrial and diurnal species from 2000-2002 in the BBS dataset

# BBS.species.stable.t1 <- unique(BBS.stable.t1$animal_jetz)
# 297
# all terrestrial and diurnal species continuously observed (abundance > 0) at a BBS segment from 2000-2002

# rm(BBS.species.stable.t1, BBS.species.t1)
# rm(BBS.species.all, my.birds)
