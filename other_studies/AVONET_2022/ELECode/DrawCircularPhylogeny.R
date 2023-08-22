# function to generate circular phylogeny with three rings adapted from phytools plotTree function

plotTree.wMultipleBars<-function (tree, xT1,xT2,xT3,
                                  scale = NULL, widthT1=2,widthT2=1,widthT3=0.5, 
                                  ThresholdT1=4,ThresholdT2=4,ThresholdT3=4,
                                  type = "phylogram",method = "plotTree",  tip.labels = FALSE,
                                  tip.label.Offset=20,buffer=10,
                                  alphaVal=50,colT1 = "grey",colT2 = "grey",colT3 = "grey", border = NULL) {
  
  d <- scale * (max(xT1) - min(0, min(xT1))) * 3 + buffer
  H <- nodeHeights(tree)
  lims <- c(-max(H) - d, max(H) + d)
  sw <- 0
  fg <- par()$fg
  if(tip.labels==FALSE){
    capture.output(plotTree(tree,type = "fan", ftype = "off",col="grey80",xlim = lims, ylim = lims, add = FALSE))
  }else{
    capture.output(plotTree(tree,type = "fan", ftype = "reg",fsize=0.4,offset=tip.label.Offset,col="grey80",xlim = lims, ylim = lims, add = FALSE))
  }
  par(fg = fg)
  
  obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  maxXScale<-max(xT1)* scale
  
  ThresholdT1 <- ThresholdT1 * scale
  ThresholdT2 <- ThresholdT2 * scale
  ThresholdT3 <- ThresholdT3 * scale
  
  xT1 <- xT1 * scale
  xT2 <- xT2 * scale
  xT3 <- xT3 * scale
  
  wT1 <- widthT1
  wT2 <- widthT2
  wT3 <- widthT3
  
  colT1 <- rep(colT1, ceiling(Ntip(tree)/length(colT1)))[1:Ntip(tree)]
  names(colT1) <- tree$tip.label
  colT2 <- rep(colT2, ceiling(Ntip(tree)/length(colT2)))[1:Ntip(tree)]
  names(colT2) <- tree$tip.label
  colT3 <- rep(colT3, ceiling(Ntip(tree)/length(colT3)))[1:Ntip(tree)]
  names(colT3) <- tree$tip.label
  
  #############################################################
  # trait 1
  #############################################################
  
  h <- max(nodeHeights(familyPhyTipsL))
  sw <- strwidth("l")
  for (i in 1:length(xT1)) {
    theta <- atan(obj$yy[i]/obj$xx[i])
    if (obj$xx[i] > 0){
      s <- 1
    }else{ 
      s <- -1
    }  
    dx <- s * h * cos(theta) + s * cos(theta) * sw
    dy <- s * h * sin(theta) + s * sin(theta) * sw
    
    x1 <- dx + (wT1/2) * cos(pi/2 - theta) - s * min(0,min(xT1)) * cos(theta)#top left
    x2 <- dx - (wT1/2) * cos(pi/2 - theta) - s * min(0,min(xT1)) * cos(theta)#bottom left
    
    y1 <- dy - (wT1/2) * sin(pi/2 - theta) - s * min(0,min(xT1)) * sin(theta)#top left
    y2 <- dy + (wT1/2) * sin(pi/2 - theta) - s * min(0,min(xT1)) * sin(theta)#bottom left
    
    x3 <- s * xT1[i] * cos(theta) + x2 #bottom right
    x4 <- s * xT1[i] * cos(theta) + x1 #top right
    
    y3 <- s * xT1[i] * sin(theta) + y2 #bottom right
    y4 <- s * xT1[i] * sin(theta) + y1 #top right
    
    x3Max <- s * max(xT1) * cos(theta) + x2 #bottom right
    x4Max <- s * max(xT1) * cos(theta) + x1 #top right
    
    y3Max <- s * max(xT1) * sin(theta) + y2 #bottom right
    y4Max <- s * max(xT1) * sin(theta) + y1 #top right
    
    if(xT1[i]>=ThresholdT1){
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = colT1[i], border = FALSE)
    }else{
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = makeTransparent(colT1[i],alpha=alphaVal), border = FALSE)
    } 
  }
  
  #############################################################
  # trait 2
  #############################################################
  
  for (i in 1:length(xT1)) {
    theta <- atan(obj$yy[i]/obj$xx[i])
    if (obj$xx[i] > 0){
      s <- 1
    }else{ 
      s <- -1
    }  
    dx <- s * (h+maxXScale+1) * cos(theta) + s * cos(theta) * sw
    dy <- s * (h+maxXScale+1) * sin(theta) + s * sin(theta) * sw
    
    x1 <- dx + (wT2/2) * cos(pi/2 - theta) - s * min(0,min(xT2)) * cos(theta)#top left
    x2 <- dx - (wT2/2) * cos(pi/2 - theta) - s * min(0,min(xT2)) * cos(theta)#bottom left
    
    y1 <- dy - (wT2/2) * sin(pi/2 - theta) - s * min(0,min(xT2)) * sin(theta)#top left
    y2 <- dy + (wT2/2) * sin(pi/2 - theta) - s * min(0,min(xT2)) * sin(theta)#bottom left
    
    x3 <- s * xT2[i] * cos(theta) + x2 #bottom right
    x4 <- s * xT2[i] * cos(theta) + x1 #top right
    
    y3 <- s * xT2[i] * sin(theta) + y2 #bottom right
    y4 <- s * xT2[i] * sin(theta) + y1 #top right
    
    x3Max <- s * max(xT2) * cos(theta) + x2 #bottom right
    x4Max <- s * max(xT2) * cos(theta) + x1 #top right
    
    y3Max <- s * max(xT2) * sin(theta) + y2 #bottom right
    y4Max <- s * max(xT2) * sin(theta) + y1 #top right
    
    if(xT2[i]>=ThresholdT2){
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = colT2[i], border = FALSE)
    }else{
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = makeTransparent(colT2[i],alpha=alphaVal), border = FALSE)
    } 
  }
  
  #############################################################
  # trait 3
  #############################################################
  
  for (i in 1:length(xT1)) {
    theta <- atan(obj$yy[i]/obj$xx[i])
    if (obj$xx[i] > 0){
      s <- 1
    }else{ 
      s <- -1
    }  
    dx <- s * (h+maxXScale+maxXScale+2) * cos(theta) + s * cos(theta) * sw
    dy <- s * (h+maxXScale+maxXScale+2) * sin(theta) + s * sin(theta) * sw
    
    x1 <- dx + (wT3/2) * cos(pi/2 - theta) - s * min(0,min(xT3)) * cos(theta)#top left
    x2 <- dx - (wT3/2) * cos(pi/2 - theta) - s * min(0,min(xT3)) * cos(theta)#bottom left
    
    y1 <- dy - (wT3/2) * sin(pi/2 - theta) - s * min(0,min(xT3)) * sin(theta)#top left
    y2 <- dy + (wT3/2) * sin(pi/2 - theta) - s * min(0,min(xT3)) * sin(theta)#bottom left
    
    x3 <- s * xT3[i] * cos(theta) + x2 #bottom right
    x4 <- s * xT3[i] * cos(theta) + x1 #top right
    
    y3 <- s * xT3[i] * sin(theta) + y2 #bottom right
    y4 <- s * xT3[i] * sin(theta) + y1 #top right
    
    x3Max <- s * max(xT3) * cos(theta) + x2 #bottom right
    x4Max <- s * max(xT3) * cos(theta) + x1 #top right
    
    y3Max <- s * max(xT3) * sin(theta) + y2 #bottom right
    y4Max <- s * max(xT3) * sin(theta) + y1 #top right
    
    if(xT3[i]>=ThresholdT3){
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = colT3[i], border = FALSE)
    }else{
      polygon(c(x1, x2, x3Max, x4Max), c(y1, y2, y3Max, y4Max),col = makeTransparent("grey80",alpha=100), border = FALSE)
      polygon(c(x1, x2, x3, x4), c(y1, y2, y3, y4),col = makeTransparent(colT3[i],alpha=alphaVal), border = FALSE)
    } 
  }
  invisible(obj)
}

# function to set transparency of bars in function plotTree.wMultipleBars

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

