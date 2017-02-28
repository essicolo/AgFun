CateNelson <- function(x,y, xcn, ycn, TNpos="ul",
                       xlab="Test value", ylab="Heigth",
                       anovm=TRUE, anovm.span=0.1, anovm.degree=2,
                       dens.plot=TRUE,
                       colors="clarity", pch.cex=1.2,
                       pchType = c(25,24,21,22),
                       pch.per.quadrant = TRUE,
                       filled.by="white",
                       draw.ellipse = "both", level.ellipse = 0.95,
                       grid=NULL)
{

  # Theory
  
  # TN | FP
  # -------
  # FN | TP

  # Spécificity = TN/(TN+FP): probability that a high yield is well balanced
  # Sensitivity = TP/(TP+FN): probability a low yield is misbalanced
  # Positive predictive value PPV = TP/(TP+FP): probability that a misbalanced diagnosis end up as low yielder
  # Negative predictive value NPV = TN/(TN+FN): probability that a balanced diagnosis end up as high yielder

  # x, y: x- and y-axes, numerical vectors
  # xcn, ycn: x and y delimiters, numerical scalars
  # xlab, ylab: x and y axis labels, character scalars
  # anovm: plot anovm? boolean
  # anovm.span, anovm.degree: smoothness of the anovm
  # dens.plot: plot high and low yield densities? boolean
  # colors: color scheme, clarity, greys, Dark2, pastel1, shootit, nature
  # pch.cex: point size
  # pchType: point types respectively for FN, FP, TN and TP
  # pch.per.quadrant: different pch types per quadrant? boolean
  # filled.by: color to fill the points
  # draw.ellipse: should ellipses be plotted? none: no ellipses, err: error, dev: deviation, both: error and deviation
  # level.elliose: confidence level for ellipses
  # grid: plot grid of points

  if(colors == "clarity") {
    colors <- c("#f8766d", "#7cae00", "#c77cff", "#00bfc4", "grey15")
  } else if(colors == "greys") {
    colors <- c("grey66", "white", "grey66", "white", "black")
  } else if(colors == "Dark2") {
    colors <-c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","grey15")
  } else if(colors == "pastel1") {
    colors <-c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4","grey15")
  } else if(colors == "shootit") {
    colors <-c("#a36610", "#f56b3d", "#f5e53d", "#f2f1e9","grey15")
  } else if(colors == "nature") {
    colors <-c("#4985e7", "#9b98a9", "#fcf960", "#777125","grey15")
  }


  palette(colors)

  cnData<-as.data.frame(cbind(x,y))
  cnData.sort<-cnData[order(cnData[,1], decreasing = FALSE),]


  css3<-(sum(cnData.sort[,2]))^2/length(cnData.sort[,2])
  css1<-vector()
  css2<-css1
  css<-css1

  for (i in 1:nrow(cnData.sort)) {
    css1[i]<-(sum(cnData.sort[1:i,2]))^2/length(cnData.sort[1:i,2])
    css2[i]<-(sum(cnData.sort[i:nrow(cnData.sort),2]))^2/length(cnData.sort[i:nrow(cnData.sort),2])
    css[i]<-css1[i]+css2[i]-css3
  }

  tss<-sum(cnData.sort[,2]^2)-sum(cnData.sort[,2])^2/nrow(cnData.sort) #sum(css)
  R.squared<-css/tss

  # TYPE
  cnType <- vector()
  for (i in 1:nrow(cnData)) {
    if(any(is.na(cnData[i, ]))) {
      cnType[i] <- NA
    } else if (TNpos == "ul") {
      if (cnData[i,1]>=xcn & cnData[i,2]>ycn) cnType[i]<-"FP"
      if (cnData[i,1]>xcn & cnData[i,2]<=ycn) cnType[i]<-"TP"
      if (cnData[i,1]<=xcn & cnData[i,2]<ycn) cnType[i]<-"FN"
      if (cnData[i,1]<xcn & cnData[i,2]>=ycn) cnType[i]<-"TN"
    } else if(TNpos =="ur") {
      if (cnData[i,1]>=xcn & cnData[i,2]>ycn) cnType[i]<-"TN"
      if (cnData[i,1]>xcn & cnData[i,2]<=ycn) cnType[i]<-"FN"
      if (cnData[i,1]<=xcn & cnData[i,2]<ycn) cnType[i]<-"TP"
      if (cnData[i,1]<xcn & cnData[i,2]>=ycn) cnType[i]<-"FP"
    } else if(TNpos =="ll") {
      if (cnData[i,1]>=xcn & cnData[i,2]>ycn) cnType[i]<-"TP"
      if (cnData[i,1]>xcn & cnData[i,2]<=ycn) cnType[i]<-"FP"
      if (cnData[i,1]<=xcn & cnData[i,2]<ycn) cnType[i]<-"TN"
      if (cnData[i,1]<xcn & cnData[i,2]>=ycn) cnType[i]<-"FN"
    } else if(TNpos =="lr") {
      if (cnData[i,1]>=xcn & cnData[i,2]>ycn) cnType[i]<-"FN"
      if (cnData[i,1]>xcn & cnData[i,2]<=ycn) cnType[i]<-"TN"
      if (cnData[i,1]<=xcn & cnData[i,2]<ycn) cnType[i]<-"FP"
      if (cnData[i,1]<xcn & cnData[i,2]>=ycn) cnType[i]<-"TP"
    }
  }


  cnType<-as.factor(cnType)

  if (pch.per.quadrant) {
    pchFac <- cnType
  } else {
    pchFac <- filled.by
  }


  cnCountTOT<-nrow(cnData)
  cnCountFP<-nrow(cnData[cnType == "FP",])
  cnCountTP<-nrow(cnData[cnType == "TP",])
  cnCountFN<-nrow(cnData[cnType == "FN",])
  cnCountTN<-nrow(cnData[cnType == "TN",])

  cnTable<-data.frame(Balanced=c(cnCountTN,cnCountFN),
                      Imbalanced=c(cnCountFP,cnCountTP))
  rownames(cnTable)<-c("High yield","Low yield")

  sensitivity <- cnCountTP/(cnCountTP+cnCountFN)
  specificity <- cnCountTN/(cnCountTN+cnCountFP)
  ppv <- cnCountTP/(cnCountTP+cnCountFP)
  npv <- cnCountTN/(cnCountFN+cnCountTN)
  prevalence <- (cnCountTP+cnCountFP)/(cnCountTP+cnCountFP+cnCountFN+cnCountTN)
  accuracy <- (cnCountTP+cnCountTN)/(cnCountTP+cnCountFP+cnCountFN+cnCountTN)

  posLL<- c( min(x) + (xcn-min(x))/2 , min(y) + (ycn-min(y))/2 )
  posUL<- c( min(x) + (xcn-min(x))/2 , max(y) - (max(y)-ycn)/2 )
  posLR<- c( max(x) - (max(x)-xcn)/2 , min(y) + (ycn-min(y))/2 )
  posUR<- c( max(x) - (max(x)-xcn)/2 , max(y) - (max(y)-ycn)/2 )

  n_plots <- anovm + dens.plot
  if (n_plots == 0) {
    layout(matrix(c(1,2,3,4),2,2), widths=c(10,1), height=c(1,10))
  } else if (n_plots == 1) {
    layout(matrix(c(1,2,5,3,4,6),3,2), widths=c(20,1), height=c(1,13,7))
  } else if (n_plots == 2) {
    layout(matrix(c(1,2,5,6,3,4,7,8),4,2), widths=c(20,1), height=c(1,10,5,5))
  }

  par(mar=c(0,4.2,0,0))
  plot(cnData[,1],cnData[,2], type="n", xaxt='n', yaxt='n', xlab=NA, ylab=NA)
  rect(par("usr")[1], par("usr")[3], xcn, par("usr")[4], col =colors[1]) # bleu
  rect(xcn, par("usr")[3], par("usr")[2], par("usr")[4], col =colors[2]) # vert

  if (TNpos == "ul" | TNpos == "ll") {
    text(x=posLL[1], y=par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,
         paste("NPV = ", round(npv,2), sep=""))
    text(x=posLR[1], y=par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,
         paste("PPV = ", round(ppv,2), sep=""))
  } else if(TNpos =="ur" | TNpos =="lr") {
    text(x=posLR[1], y=par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,
         paste("NPV = ", round(npv,2), sep=""))
    text(x=posLL[1], y=par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,
         paste("PPV = ", round(ppv,2), sep=""))
  }

  par(mar=c(4,4.2,0,0))
  plot(cnData[,1],cnData[,2], xlab=xlab, ylab=ylab, type="n")
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey90") # fond
  grid(col="grey90", lty=1)

  # Ellipses
  if (draw.ellipse != "none") {
    require(ellipse)
    ellData <- cnData
    if (draw.ellipse == "dev" | draw.ellipse == "both") {
      ellDev<-ellipse(cor(ellData),
                      scale=c(sd(ellData[,1]),sd(ellData[,2])),
                      centre=apply(ellData, 2, "mean"),
                      level=0.95, npoints=100)
      polygon(ellDev, col=rgb(1, 1, 1,0.5), border=NA)
    }
    if (draw.ellipse == "err" | draw.ellipse == "both") {
      ellErr<-ellipse(cor(ellData),
                      scale=c(sd(ellData[,1])/sqrt(nrow(ellData)),
                              sd(ellData[,2])/sqrt(nrow(ellData))),
                      centre=apply(ellData, 2, "mean"),
                      level=level.ellipse, npoints=100)
      polygon(ellErr, col=rgb(0.3, 0.3, 0.3, 0.5), border=NA)
    }


  }

  abline(v=xcn,h=ycn, lty=2, lwd=1)

  # inside the main CN plot
  if (TNpos == "ul") {
    text(posLL[1],posLL[2],"FN", cex=4, col="gray55")
    text(posUL[1],posUL[2],"TN", cex=4, col="gray55")
    text(posLR[1],posLR[2],"TP", cex=4, col="gray55")
    text(posUR[1],posUR[2],"FP", cex=4, col="gray55")
    points(grid, pch=4, col="grey30", cex=0.05)
    points(cnData[,1],cnData[,2], pch=pchType[pchFac], bg=filled.by, cex=pch.cex, col="grey30")
    text(min(x),min(y),paste("nb FN =",cnCountFN), pos=4)
    text(max(x),min(y),paste("nb TP =",cnCountTP), pos=2)
    text(min(x),max(y),paste("nb TN =",cnCountTN), pos=4)
    text(max(x),max(y),paste("nb FP =",cnCountFP), pos=2)
  } else if(TNpos =="ur") {
    text(posLL[1],posLL[2],"TP", cex=4, col="gray55")
    text(posUL[1],posUL[2],"FP", cex=4, col="gray55")
    text(posLR[1],posLR[2],"FN", cex=4, col="gray55")
    text(posUR[1],posUR[2],"TN", cex=4, col="gray55")
    points(grid, pch=4, col="grey30", cex=0.05)
    points(cnData[,1],cnData[,2], pch=pchType[pchFac], bg=filled.by, cex=pch.cex)
    text(min(x),min(y),paste("nb TP =",cnCountTP), pos=4)
    text(max(x),min(y),paste("nb FN =",cnCountFN), pos=2)
    text(min(x),max(y),paste("nb FP =",cnCountFP), pos=4)
    text(max(x),max(y),paste("nb TN =",cnCountTN), pos=2)
  } else if(TNpos =="ll") {
    text(posLL[1],posLL[2],"TN", cex=4, col="gray55")
    text(posUL[1],posUL[2],"FN", cex=4, col="gray55")
    text(posLR[1],posLR[2],"FP", cex=4, col="gray55")
    text(posUR[1],posUR[2],"TP", cex=4, col="gray55")
    points(grid, pch=4, col="grey30", cex=0.05)
    points(cnData[,1],cnData[,2], pch=pchType[pchFac], bg=filled.by, cex=pch.cex)
    text(min(x),min(y),paste("nb TN =",cnCountTN), pos=4)
    text(max(x),min(y),paste("nb FP =",cnCountFP), pos=2)
    text(min(x),max(y),paste("nb FN =",cnCountFN), pos=4)
    text(max(x),max(y),paste("nb TP =",cnCountTP), pos=2)
  } else if(TNpos =="lr") {
    text(posLL[1],posLL[2],"FP", cex=4, col="gray55")
    text(posUL[1],posUL[2],"TP", cex=4, col="gray55")
    text(posLR[1],posLR[2],"TN", cex=4, col="gray55")
    text(posUR[1],posUR[2],"FN", cex=4, col="gray55")
    points(grid, pch=4, col="grey30", cex=0.05)
    points(cnData[,1],cnData[,2], pch=pchType[pchFac], bg=filled.by, cex=pch.cex)
    text(min(x),min(y),paste("nb FP =",cnCountFP), pos=4)
    text(max(x),min(y),paste("nb TN =",cnCountTN), pos=2)
    text(min(x),max(y),paste("nb TP =",cnCountTP), pos=4)
    text(max(x),max(y),paste("nb FN =",cnCountFN), pos=2)
  }
  axis(side=1, at=round(xcn,2), cex.axis=0.8, font=2, padj=-1)
  axis(side=2, at=ycn, cex.axis=0.8, font=2, padj=1)

  par(mar=c(0,0,0,0))
  plot(x=c(0,1), y=c(0,1), type="n", xaxt='n', yaxt='n', xlab=NA, ylab=NA)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey30") # fond# grey 30
  #text(x=0.5, y=0.5, paste("Prev.=\n", round(prevalence,2), sep=""), col="white")
  text(x=0.5, y=0.5, paste("Acc.=\n", round(accuracy,2), sep=""), col="white")

  par(mar=c(4,0,0,0))
  plot(cnData[,1],cnData[,2], type="n", xaxt='n', yaxt='n', xlab=NA, ylab=NA)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], ycn, col =colors[3]) # rouge
  rect(par("usr")[1], ycn, par("usr")[2], par("usr")[4], col =colors[4]) # mauve

  if (TNpos =="ul" | TNpos =="ur") {
    text(x=par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, y=posUR[2],
         paste("Specificity = ", round(specificity, 2), sep=""), srt=90)
    text(x=par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, y=posLR[2],
         paste("Sensitivity = ", round(sensitivity, 2), sep=""), srt=90)
  } else if(TNpos =="ll" | TNpos =="lr") {
    text(x=par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, y=posUR[2],
         paste("Sensitivity = ", round(sensitivity, 2), sep=""), srt=90)
    text(x=par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, y=posLR[2],
         paste("Specificity = ", round(specificity, 2), sep=""), srt=90)
  }

  if (anovm) {
    par(mar=c(4,4.2,2,0))
    plot(cnData.sort[,1],R.squared, #main="Cate-Nelson ANOVM method",
         xlab=xlab, ylab="R²", type="n")
    #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey90") # fond
    grid(col="grey90", lty=1)
    abline(v=xcn, lty=2, lwd=1)
    points(cnData.sort[,1],R.squared, pch=21, bg="white")
    lines(loess.smooth(cnData.sort[,1],R.squared, degree=anovm.degree, span=anovm.span), lwd=3)
    axis(side=1, at=round(xcn,2), cex.axis=0.8, font=2, padj=-1)
  }

  if (dens.plot) {

    if (TNpos == "ul" | TNpos == "ur") {
      dens.h <- density(cnData[cnType == "TN" | cnType == "FP",1], na.rm=TRUE)
      dens.l <- density(cnData[cnType == "FN" | cnType == "TP",1], na.rm=TRUE)
    } else if(TNpos =="ll" | TNpos == "lr") {
      dens.h <- density(cnData[cnType == "FN" | cnType == "TP",1], na.rm=TRUE)
      dens.l <- density(cnData[cnType == "TN" | cnType == "FP",1], na.rm=TRUE)
    }

    par(mar=c(4,4.2,2,0))
    plot(c(dens.h, dens.l), type="n",
         ylab="Density", xlab=xlab, xlim=c(min(cnData[,1]),max(cnData[,1])))
    #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey90") # fond
    grid(col="grey90", lty=1)
    abline(v=xcn, lty=2)
    polygon(dens.h, col=rgb(113/256, 113/256, 113/256, 0.75), border=NA)
    polygon(dens.l, col=rgb(41/256, 41/256, 41/256, 0.75), border=NA)
    lines(dens.h)
    lines(dens.l)
    text(x=dens.l$x[which(dens.l$y==max(dens.l$y))],
         y=max(dens.l$y)/2, labels="LR", col="white")
    text(x=dens.h$x[which(dens.h$y==max(dens.h$y))],
         y=max(dens.h$y)/2, labels="HR", col="white")
    axis(side=1, at=round(xcn,2), cex.axis=0.8, font=2, padj=-1)
  }

  out<-list()
  out$table<-list(cnTable,
                  paste("sensitivity =", round(sensitivity,2)),
                  paste("specificity =", round(specificity,2)),
                  paste("positive predictive value =", round(ppv,2)),
                  paste("negative predictive value =", round(npv,2)),
                  paste("prevalence =", round(prevalence,2)),
                  paste("accuracy =", round(accuracy,2)))
  out$type<-cnType
  out$sensitivity <- sensitivity
  out$specificity <- specificity
  out$ppv <- ppv
  out$npv <- npv
  out$prevalence <- prevalence
  out$accuracy <- accuracy

  return(out)
}
