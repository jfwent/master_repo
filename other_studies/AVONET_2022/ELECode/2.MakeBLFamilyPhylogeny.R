
phy<-read.nexus(paste0(PhylogeneticDataFolder,'HackettStage1_0001_1000_MCCTreeTargetHeights.nex')) # species level phylogeny according to BirdTree taxonomy
crossWalk<-read.csv(paste0(PhylogeneticDataFolder,'BirdLife-BirdTree crosswalk.csv')) # taxonomy crosswalk
AVONET1<-read.csv(paste0(TraitDataFolder,'AVONET1_BirdLife.csv')) # BirdLife species and families 

###############################################################################################
###############################################################################################

crossWalk$Species3<-gsub(" ","_",crossWalk$Species3) # add underscore to species names in the crosswalk to match those in the phylogeny
BLspecies<-crossWalk$Species1[match(phy$tip.label,crossWalk$Species3)] # find the BL species that match the BirdTree species in the phylogeny
BLFamily<-AVONET1$Family1[match(BLspecies,AVONET1$Species1)] # find the familes of these BL species

# sample a single BirdTree species from each BL family
tip.labelByFamily<-split(phy$tip.label,BLFamily)
sampleFunction<-function(x){return(x[1])}
familyReps<-sapply(tip.labelByFamily,sampleFunction)
familyNames<-names(tip.labelByFamily)
familyData<-data.frame(Family=familyNames,Species=familyReps)
# use these family representatives to extract a family level tree
dropTips<-setdiff(phy$tip.label,familyReps)
familyPhy<-drop.tip(phy,dropTips)
# replace BirdTree species names in the tree with BL family names
familyPhyTips<-familyPhy
familyPhyTips$tip.label<-familyData$Family[match(familyPhy$tip.label,familyData$Species)]
# Elachuridae is not represented in Stage 1 BirdTree
# insert this family as sister to Bombycillidae (see Alström et al 2014 Biol Letters 10 (3): 20131067)
focEdge<-which(familyPhyTips$edge[,2]==which(familyPhyTips$tip.label=="Bombycillidae"))
focNode<-familyPhyTips$edge[focEdge,1]
focEdgeLength<-familyPhyTips$edge.length[focEdge]
familyPhyTipsUpdate<-bind.tip(tree=familyPhyTips,tip.label="Elachuridae",edge.length=focEdgeLength,where=focNode)
# ladderise tree to make pretty plot
familyPhyTipsL<-ladderize(familyPhyTipsUpdate)
# save
write.nexus(familyPhyTipsL,file=paste0(PhylogeneticDataFolder,'Birdlife_Family_Phylogeny.nex'))





