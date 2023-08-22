
####################################################################
# Figure 1 plot sample sizes of previous bird trait studies
####################################################################

plotTable <- read.csv(paste0(TraitDataFolder,'TraitDatabasesHistory.csv'))
plotTable$ltySpecies<-1
plotTable$ltySpecimen<-1
plotTable$ltySpecimen[which(plotTable$Reference=="Dunn et al. 2001 Evolution")]<-2

plot.f=paste0(figFolder,'Figure1.pdf')
pdf(plot.f,w=3.5,h=3.5)

# separate Chira and pigot studies so they can be distinguished on the timeline
plotTable$Year[which(plotTable$Reference=="Pigot et al 2020 Nature Ecol Evol")]<-2020.5

#stretch out recent years to aid visualisation
plotTable$Year2<-plotTable$Year
plotTable$Year2[which(plotTable$Year>=2015)]<-2015+5*(plotTable$Year2[which(plotTable$Year>=2015)]-2015)

# set symbol sizes proportional to number of traits measured
plotTable$NumMorphTraits2<-plotTable$NumMorphTraits
plotTable$NumMorphTraits2[plotTable$NumMorphTraits2>15]<-15
plotTable$SymbolSize<-plotTable$NumMorphTraits2/5

# make plot
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(2,2.5,0.25,1))
possYears<-min(plotTable$Year2):max(plotTable$Year2)
plot(1,1,xlim=c(min(possYears),max(possYears)),ylim=c(-10000,13000),log="x",yaxt="n",ylab="",xaxt="n",xlab="",bty="n")

# add horizntal guide lines 
yvals<-seq(0,12000,1000)
for(i in 1:length(yvals)){
  lines(c(min(possYears)-5,max(possYears)),c(yvals[i],yvals[i]),lwd=0.25,col="grey90")
  lines(c(min(possYears)-5,max(possYears)),-c(yvals[i],yvals[i]),lwd=0.25,col="grey90")
}
lines(c(min(possYears)-5,max(possYears)),c(0,0),lwd=1,col="black")

# add vertical lines showing species and specimen sample sizes
for(x in 1:nrow(plotTable)){
  
  if(plotTable$openSource[x]==0){
    lines(c(plotTable$Year2[x],plotTable$Year2[x]),(c(0,plotTable$NumSpecies[x])),col="grey50",lty=plotTable$ltySpecies[x])
    lines(c(plotTable$Year2[x],plotTable$Year2[x]),-(c(0,plotTable$NumSpecimen[x]))/10,col="grey50",lty=plotTable$ltySpecimen[x])
  }
  if(plotTable$openSource[x]==1){
    lines(c(plotTable$Year2[x],plotTable$Year2[x]),(c(0,plotTable$NumSpecies[x])),col="black",lty=plotTable$ltySpecies[x])
    lines(c(plotTable$Year2[x],plotTable$Year2[x]),-(c(0,plotTable$NumSpecimen[x]))/10,col="black",lty=plotTable$ltySpecimen[x])
  }
}
for(x in 1:nrow(plotTable)){
  
  if(plotTable$openSource[x]==0){
    points(plotTable$Year2[x],(plotTable$NumSpecies[x]),pch=21,bg="white",col="grey50",cex=plotTable$SymbolSize[x])
    points(plotTable$Year2[x],-(plotTable$NumSpecimen[x])/10,pch=21,bg="grey50",col="grey50",cex=0.5)
    #text(plotTable$Year2[x]+1,plotTable$NumSpecies[x],x,pos=2,cex=0.5,co="grey50")
  }
  if(plotTable$openSource[x]==1){
    points(plotTable$Year2[x],(plotTable$NumSpecies[x]),pch=21,bg="white",col="black",cex=plotTable$SymbolSize[x])
    points(plotTable$Year2[x],-(plotTable$NumSpecimen[x])/10,pch=21,bg="black",col="black",cex=0.5)
    #text(plotTable$Year2[x]+1,plotTable$NumSpecies[x],x,pos=2,cex=0.5,co="black")
  }
}

# add study names
studyNames<-c("1. Hartman 1961",
              "2. Ricklefs 1977",
              "3. James 1982",
              "4. Miles et al 1987",
              "5. Leisler & Winkler 1991",
              "6. Dunn et al 2001",
              "7. Korner-Nievergelt & Leisler 2004",
              "8. Ricklefs 2004",
              "9. Dawideit et al 2009",
              "10. Claramunt 2010",
              "11. Kennedy et al 2016",
              "12. Miller et al 2017",
              "13. Ricklefs 2017",
              "14. Cooney et al 2017",
              "15. Zanata et al 2018",
              "16. Phillips et al 2018",
              "17. Chira et al 2018",
              "18. Rodrigues et al 2019",
              "19. Chira et al 2020",
              "20. Pigot et al 2020")
cols<-c("grey60","black")[plotTable$openSource+1]
yVals<-c(rev(seq(3500,10500,length.out=10)),seq(-2500,-9500,length.out=10))
for(i in 1:length(studyNames)){text(1957,yVals[i],studyNames[i],pos=4,col=cols[i],cex=0.5)}

# add axes and labels
axis(1,at=seq(1960,2045,5),labels=rep("",length(seq(1960,2045,5))),cex.axis=0.5,padj=-4,tck=-0.01)

axis(1,at=seq(1960,2015,5),labels=seq(1960,2015,5),cex.axis=0.5,padj=-4,tck=-0.01)
axis(1,at=seq(2020,2045,5),labels=seq(2016,2021,1),cex.axis=0.5,padj=-4,tck=-0.01)

axis(2,at=seq(0,12000,1000),labels=c("       0"," 1000"," 2000"," 3000"," 4000"," 5000"," 6000"," 7000"," 8000"," 9000","10,000  ","11,000  ","12,000  "),cex.axis=0.5,hadj=0.3,las=2,tck=-0.01)
axis(2,at=seq(0,-10000,-1000),labels=c("","10,000  ","20,000  ","30,000  ","40,000  ",
                                       "50,000  ","60,000  ","70,000  ","80,000  ","90,000  ",
                                       "100,000     "),cex.axis=0.5,hadj=0.3,las=2,tck=-0.01)

mtext('Year',side=1,outer=F,line=0.5,las=1,cex=0.75)
mtext('# species',side=2,outer=F,line=1.7,cex=0.75,at=6000)
mtext('# specimens',side=2,outer=F,line=1.7,cex=0.75,at=-5000)

dev.off()
system(paste0('open ',plot.f))





