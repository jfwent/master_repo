
####################################################################
# Figure 3 plot Birdlife tree and sampling across families and taxonomies
####################################################################

familyData<-read.csv(paste0(TraitDataFolder,'bl_Specimens_per_family.csv')) # level of sampling per family
familyPhyTipsL<-read.nexus(paste0(PhylogeneticDataFolder,'Birdlife_Family_Phylogeny.nex')) # family level phylogeny

source(paste0(codeFolder,'DrawCircularPhylogeny.R'))

###############################################################################################
###############################################################################################

#check for mismatches in tree family tips labels and family names in tables
setdiff(familyPhyTipsL$tip.label,familyData$familyIDS)
setdiff(familyData$familyIDS,familyPhyTipsL$tip.label)
#none....

# sort family dataframe to match tips labels
familyData<-familyData[match(familyPhyTipsL$tip.label,familyData$familyIDS),]

# assign traits to plot
traits1<-familyData$Portion_of_family_with_at_least_one_full_specimen
names(traits1)<-familyData$Birdlife.Family
traits2<-familyData$Portion_of_family_with_at_least_four_specimens
names(traits2)<-familyData$Birdlife.Family
traits3<-familyData$Portion_of_family_with_at_least_four_main_traits_measured_per_species
names(traits3)<-familyData$Birdlife.Family

# plot tree

plot.f<-paste0(figFolder,'Birdlife_Phylogeny.pdf')
pdf(plot.f,w=8,h=8)

plotTree.wMultipleBars(familyPhyTipsL, xT1=traits1, xT2=traits2, xT3=traits3,   
                       scale=0.2, widthT1=2.8,widthT2=3.7,widthT3=4.25,
                       ThresholdT1=75,ThresholdT2=75,ThresholdT3=75,
                       method="plotTree",type="fan", tip.labels=TRUE,tip.label.Offset=65,buffer=44,
                       alphaVal=100,colT1="gold",colT2="red4",colT3="dodgerblue4",border=FALSE)

dev.off()
system(paste0('open ',plot.f))



####################################################################
# plot sampling coverage for each taxonomy
####################################################################

# dataframe containing species numbers and levels of sampling 
data<-read.delim(paste0(TraitDataFolder,'TaxonomySampling.txt'),header=T)

# convert sampling levels to proportions
data[,3:6]<-data[,3:6]/data[,2]

# make plot
plot.f<-paste0(figFolder,'SpeciesLevelSamplingBarplots.pdf')
pdf(plot.f, width=5, height=2)	

# set layout
par(mfrow=c(1,3))
par(oma=c(2,2,2,1))
par(mar=c(2,2,1,1))

# plot proportion of species with at least 1 complete specimen
plot(0,0,xlim=c(0,3),ylim=c(0,1),type="n",bty="l",xaxt="n",yaxt="n")
axis(1,at=seq(0.5,2.5,1),labels=c("BirdLife","eBird","BirdTree"),tck=-0.01,padj=-2,cex.axis=0.82)
axis(2,at=seq(0,1,0.25),labels=seq(0,1,0.25),tck=-0.01,hadj=0.75,las=2)
mtext("Proportion of species",side=2,line=2.5,cex=0.75)
mtext(expression("">="1 complete set"),side=3,line=0,cex=0.75)

yval<-data[1,5]
polygon(c(0,1,1,0),c(0,0,yval,yval),col="gold",border="black")
yval<-data[2,5]
polygon(c(1,2,2,1),c(0,0,yval,yval),col="gold",border="black")
yval<-data[3,5]
polygon(c(2,3,3,2),c(0,0,yval,yval),col="gold",border="black")

# plot proportion of species with at least 4 specimens
plot(0,0,xlim=c(0,3),ylim=c(0,1),type="n",bty="l",xaxt="n",yaxt="n")
axis(1,at=seq(0.5,2.5,1),labels=c("BirdLife","eBird","BirdTree"),tck=-0.01,padj=-2,cex.axis=0.82)
axis(2,at=seq(0,1,0.25),labels=seq(0,1,0.25),tck=-0.01,hadj=0.75,las=2)
mtext(expression("">="4 individuals"),side=3,line=0,cex=0.75)

yval<-data[1,4]
polygon(c(0,1,1,0),c(0,0,yval,yval),col="red4",border="black")
yval<-data[2,4]
polygon(c(1,2,2,1),c(0,0,yval,yval),col="red4",border="black")
yval<-data[3,4]
polygon(c(2,3,3,2),c(0,0,yval,yval),col="red4",border="black")

# plot proportion of species with at least 4 complete specimens

plot(0,0,xlim=c(0,3),ylim=c(0,1),type="n",bty="l",xaxt="n",yaxt="n")
axis(1,at=seq(0.5,2.5,1),labels=c("BirdLife","eBird","BirdTree"),tck=-0.01,padj=-2,cex.axis=0.82)
axis(2,at=seq(0,1,0.25),labels=seq(0,1,0.25),tck=-0.01,hadj=0.75,las=2)
mtext(expression("">="4 complete sets"),side=3,line=0,cex=0.75)

yval<-data[1,6]
polygon(c(0,1,1,0),c(0,0,yval,yval),col="dodgerblue4",border="black")
yval<-data[2,6]
polygon(c(1,2,2,1),c(0,0,yval,yval),col="dodgerblue4",border="black")
yval<-data[3,6]
polygon(c(2,3,3,2),c(0,0,yval,yval),col="dodgerblue4",border="black")

dev.off()
system(paste0('open ',plot.f))


