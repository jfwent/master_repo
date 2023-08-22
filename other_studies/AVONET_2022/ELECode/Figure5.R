####################################################################
# Figure 5 trait variance across taxonomic levels, families and species 
####################################################################

####################################################################
# load data
####################################################################

# species level trait averages birdlife taxonomy
datBL<-read.csv(paste0(TraitDataFolder,'AVONET1_BirdLife.csv'))
#trait data
dat<-read.csv(paste0(TraitDataFolder,'AVONET_Raw_Data.csv'))
#transfer order and family assignments to raw data
dat$Order1<-datBL$Order1[match(dat$Species1_BirdLife,datBL$Species1)]
dat$Family1<-datBL$Family1[match(dat$Species1_BirdLife,datBL$Species1)]
#keep just the columns we are using
datBL<-datBL[,match(c("Beak.Length_Culmen","Order1","Family1","Species1"),names(datBL))]
dat<-dat[,match(c("Beak.Length_Culmen","Order1","Family1","Species1_BirdLife"),names(dat))]
#remove any rows with missing trait data
datBL<-na.omit(datBL)
dat<-na.omit(dat)

#############################################################v
# find trait variance within species
#############################################################v
#split specimen bill data by species
BLlist<-split(log10(dat$Beak.Length_Culmen),dat$Species1_BirdLife)
#remove NAs
BLlist<-lapply(BLlist, function(x) x[!is.na(x)])
#lets just keep species with 5 or more specimens
N<-sapply(BLlist,length)
Threshold<-5
BLlist<-BLlist[N>=Threshold]
#variance within species
withinVar<-sapply(BLlist,var,na.rm=T)

#############################################################v
# find trait variance among species within families
#############################################################v
#split species bill data by family
BLlistFam<-split(log10(datBL$Beak.Length_Culmen),datBL$Family1)
#remove NAs
BLlistFam<-lapply(BLlistFam, function(x) x[!is.na(x)])
BLlist<-lapply(BLlist, function(x) x[!is.na(x)])
#lets just keep families with 5 or more species
NFam<-sapply(BLlistFam,length)
Threshold<-5
BLlistFam<-BLlistFam[NFam>=Threshold]
withinVarFam<-sapply(BLlistFam,var,na.rm=T)

#############################################################v
# variance components analysis
#############################################################v

dat$Order1<-as.factor(dat$Order1)
dat$Family1<-as.factor(as.character(dat$Family1))
dat$Species1_BirdLife<-as.factor(as.character(dat$Species1_BirdLife))

m <- lme(log10(Beak.Length_Culmen) ~ 1, 
         random = ~ 1|Order1/Family1/Species1_BirdLife, data=dat,
         method="REML", control=(msMaxIter=10000000))

Var<-rev(na.omit(as.numeric(VarCorr(m)[,"Variance"])))
v <- varcomp(m, TRUE, TRUE)
v<-c(0,v) 
v<-round(rev(diff(v)),3)*100

#############################################################v
# make figure
#############################################################v

plot.f<-paste0(figFolder,'Figure5.pdf')
pdf(plot.f, width=6.8, height=2.5)	

par(mfrow=c(4,1))
par(oma=c(2,0,0,0))
par(mar=c(1,1,1,1))
mat<-matrix(nrow=2,ncol=4)
mat[1:2,1]<-c(4,3)
mat[1:2,2]<-c(2,1)
mat[1,3:4]<-5
mat[2,3:4]<-6
layout(mat)

# plot variance by taxonomic level

label<-c("Specimen","Species","Family","Order")
for(i in 1:4){
  x<-0+seq(-0.15,0.15,0.00001)
  y<-dnorm(x, mean = 0, sd = Var[i], log = FALSE)
  plot(x,y,type="l",xaxt="n",yaxt="n",xlim=c(-0.15,0.15))
  if(i==1){
    axis(1,at=seq(-0.15,0.15,0.05),labels=round(seq(-0.15,0.15,0.05),2),tck=-0.01,padj=-1.5)
    mtext("Beak length",side=1,line=1.25,cex=0.75)
  }
  if(i==3){
    axis(1,at=seq(-0.15,0.15,0.05),labels=round(seq(-0.15,0.15,0.05),2),tck=-0.01,padj=-1.5)
    mtext("Beak length",side=1,line=1.25,cex=0.75)
  }
  if(i==2 | i==4){
    axis(1,at=seq(-0.15,0.15,0.05),labels=rep("",length(seq(-0.15,0.15,0.05))),tck=-0.01,padj=-1.5)
    
  }
  text(-0.1,max(y)+((max(y)-min(y))/20),label[i],pos=1)
  text(-0.1,max(y)-((max(y)-min(y))/10),paste0(" (",v[i],"%)"),pos=1)
}

# plot rank order of families and species by their variance
par(mar=c(1,4,1,1))

plot(sort(withinVarFam),pch=21,xaxt="n",yaxt="n",xlab="",ylab="",bty="l")
axis(2,at=seq(0,0.06,0.015),labels=c("0.000","0.015","0.030","0.045","0.060"),tck=-0.025,las=2,hadj=0.75)
mtext("Variance",side=2,line=3,cex=0.75)
length(unlist(BLlistFam)) # number of species (from families with at least 5 species)
length(withinVarFam) # number of families
mtext("Among species (n = 10817)\nwithin families (n = 156)",side=3,line=-1.5,cex=0.75,at=45)

plot(sort(withinVar),xaxt="n",yaxt="n",xlab="",ylab="",bty="l",pch=".",ylim=c(0,0.025))
axis(2,at=seq(0,0.025,0.005),labels=c("0.000","0.005","0.010","0.015","0.020","0.025"),tck=-0.025,las=2,hadj=0.75)
mtext("Rank",side=1,line=0.5,cex=0.75)
mtext("Variance",side=2,line=3,cex=0.75)
length(unlist(BLlist)) # number of specimens (from species with at least 5 specimens)
length(withinVar) # number of species
mtext("Among specimens (n = 61954)\nwithin species (n = 7072)",side=3,line=-1.5,cex=0.75,at=2200)

dev.off()
system(paste0('open ',plot.f))



