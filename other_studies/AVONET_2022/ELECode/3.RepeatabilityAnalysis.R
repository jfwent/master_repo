####################################################################
# load data
####################################################################
original_data <-read.csv(paste0(TraitDataFolder,'AVONET_Duplicate_Data.csv'),h=T)

####################################################################
# data processing and cleaning
####################################################################
duplicates <- original_data[c(1:2,5,8:14,17)]
duplicates$Family1 <- as.factor(duplicates$Family1)
duplicates$Specimen.number <- as.factor(duplicates$Specimen.number)

####################################################################
# repeatability analysis
####################################################################
#With BirdLife family included
Complete_Beak_Culmen_BL <- rptGaussian(Beak.Length_Culmen ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                        nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Nares_BL <- rptGaussian(Beak.Length_Nares ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                       nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Width_BL <- rptGaussian(Beak.Width ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                       nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Depth_BL <- rptGaussian(Beak.Depth ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                       nboot = 1000, npermut = 500, ratio = T)
Complete_Tarsus_Length_BL <- rptGaussian(Tarsus.Length ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                          nboot = 1000, npermut = 500, ratio = T)
Complete_Kipps_Distance_BL <- rptGaussian(Kipps.Distance ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                           nboot = 1000, npermut = 500, ratio = T)
Complete_Wing_Length_BL <- rptGaussian(Wing.Length ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                        nboot = 1000, npermut = 500, ratio = T)
Complete_Tail_Length_BL <- rptGaussian(Tail.Length ~ Family1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                        nboot = 1000, npermut = 500, ratio = T)

#BirdLife family excluded
Complete_Beak_Culmen <- rptGaussian(Beak.Length_Culmen ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                    nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Nares <- rptGaussian(Beak.Length_Nares ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                   nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Width <- rptGaussian(Beak.Width ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                   nboot = 1000, npermut = 500, ratio = T)
Complete_Beak_Depth <- rptGaussian(Beak.Depth ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                   nboot = 1000, npermut = 500, ratio = T)
Complete_Tarsus_Length <- rptGaussian(Tarsus.Length ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                      nboot = 1000, npermut = 500, ratio = T)
Complete_Kipps_Distance <- rptGaussian(Kipps.Distance ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                       nboot = 1000, npermut = 500, ratio = T)
Complete_Wing_Length <- rptGaussian(Wing.Length ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                    nboot = 1000, npermut = 500, ratio = T)
Complete_Tail_Length <- rptGaussian(Tail.Length ~ 1 + (1|Specimen.number), grname = "Specimen.number", data = duplicates, 
                                    nboot = 1000, npermut = 500, ratio = T)

Complete_boot <- cbind(Complete_Beak_Culmen_BL$R_boot, Complete_Beak_Nares_BL$R_boot, 
                       Complete_Beak_Width_BL$R_boot, Complete_Beak_Depth_BL$R_boot,
                       Complete_Tarsus_Length_BL$R_boot, Complete_Kipps_Distance_BL$R_boot,
                       Complete_Wing_Length_BL$R_boot,Complete_Tail_Length_BL$R_boot,
                       Complete_Beak_Culmen$R_boot, Complete_Beak_Nares$R_boot, 
                       Complete_Beak_Width$R_boot, Complete_Beak_Depth$R_boot,
                       Complete_Tarsus_Length$R_boot, Complete_Kipps_Distance$R_boot, 
                       Complete_Wing_Length$R_boot,Complete_Tail_Length$R_boot)

names(Complete_boot)[1] <- "Beak Culmen_family"
names(Complete_boot)[2] <- "Beak Nares_family"
names(Complete_boot)[3] <- "Beak Width_family"
names(Complete_boot)[4] <- "Beak Depth_family"
names(Complete_boot)[5] <- "Tarsus Length_family"
names(Complete_boot)[6] <- "Kipps_family"
names(Complete_boot)[7] <- "Wing Length_family"
names(Complete_boot)[8] <- "Tail Length_family"
names(Complete_boot)[9] <- "Beak Culmen_1"
names(Complete_boot)[10] <- "Beak Nares_1"
names(Complete_boot)[11] <- "Beak Width_1"
names(Complete_boot)[12] <- "Beak Depth_1"
names(Complete_boot)[13] <- "Tarsus Length_1"
names(Complete_boot)[14] <- "Kipps_1"
names(Complete_boot)[15] <- "Wing Length_1"
names(Complete_boot)[16] <- "Tail Length_1"

#write.csv(Complete_boot,"Repeatability_bootstraps.csv")

####################################################################
# Effect of trait on repeatability score
####################################################################
#BirdLife family included
Complete_boot_BL <- Complete_boot[c(1:8)]
Bootstraps_BL <- data.frame(stack(Complete_boot_BL[1:8]))
colnames(Bootstraps_BL)[1] <- "Repeatability_score"
colnames(Bootstraps_BL)[2] <- "Trait"

model_family <- glm(Repeatability_score ~ Trait, data=Bootstraps_BL)
model_family.aov <- aov(model_family)
summary(model_family.aov)

#BirdLife family excluded
Complete_boot_1 <- Complete_boot[c(9:16)]
Bootstraps_1 <- data.frame(stack(Complete_boot_1[1:8]))
colnames(Bootstraps_1)[1] <- "Repeatability_score"
colnames(Bootstraps_1)[2] <- "Trait"

model_1 <- glm(Repeatability_score ~ Trait, data=Bootstraps_1)
model_1.aov <- aov(model_1)
summary(model_1.aov)
