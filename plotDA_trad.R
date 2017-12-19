# Fonction pour plot
plot_lda = function(score, loading=NULL, group=NULL, ell_dev=TRUE, ell_err=TRUE, points=TRUE,
                    scale_load = 1, level=0.95, legend=TRUE, label=TRUE,
                    transparency=0.35, xlim=NULL, ylim=NULL, cex_lab=0.6) {
  # score: matrice à deux colonnes des scores de l'ADL
  # loading: matrice deux colonnes correspondant aux axes projettés
  # group: un vecteur définissant l'appartenance des scores à un groupe
  # ell_dev: projetter les ellipses de confiance de la déviation (sur toutes les observations)
  # ell_err: projetter les ellipses de confiance de l'a déviation'erreur (sur la moyenne)
  # level: niveau de confiance pour les ellipses
  # scale_load: échelle des loadings
  
  if(!is.null(group)) {
    group = factor(group)
    palette(rainbow(length(levels(group)), alpha=transparency))
  }
  
  if(!is.null(xlim) & !is.null(ylim)) {
    plot(score, type='n', xlim=xlim, ylim=ylim)
  } else if (!is.null(xlim)) {
    plot(score, type='n', xlim=xlim)
  } else if (!is.null(ylim)) {
    plot(score, type='n', ylim=ylim)
  } else {
    plot(score, type='n')
  }
  grid()
  abline(v=0,h=0, lty=2)
  
  # ellipse dev (observations)
  if(!is.null(group)) {
    if (ell_dev | ell_err) library(ellipse)
    if (ell_dev) {
      for (i in 1:length(levels(group))) {
        score_gr = score[group == levels(group)[i],]
        if(!is.null(nrow(score_gr))) { # if only one instance, skip
          ell_dev_p<-ellipse(cor(score_gr),
                             scale=c(sd(score_gr[,1]), sd(score_gr[,2])),
                             centre=apply(score_gr, 2, "mean"),
                             level=level, npoints=100)
          polygon(ell_dev_p, col=palette()[i], border=NA)
        }
        
      }    
    }
  }
  
  # points
  if(!is.null(group)) {
    if (points) {
      points(score, col=group, pch=16)
    }
  } else {
    points(score, col='grey15', pch=16)
  }
  
  # ellipse err (moyenne)
  if(!is.null(group)) {
    if (ell_err) {
      for (i in 1:length(levels(group))) {
        score_gr = score[group == levels(group)[i],]
        if(!is.null(nrow(score_gr))) { # if only one instance, skip
          ell_err_p<-ellipse(cor(score_gr),
                             scale=c(sd(score_gr[,1])/sqrt(nrow(score_gr)),
                                     sd(score_gr[,2])/sqrt(nrow(score_gr))),
                             centre=apply(score_gr, 2, "mean"),
                             level=level, npoints=100)
          if (ell_err & ell_dev) polygon(ell_err_p, col='white')
          if (ell_err & !ell_dev) polygon(ell_err_p, col=palette()[i])
        }
      }    
    }
  }
  
  #arrows
  if (!is.null(loading)) {
    for (i in 1:nrow(loading)) {
      segments(x0=0, y0=0, x1=loading[i,1]*scale_load, y1=loading[i,2]*scale_load)
      text(x=loading[i,1]*scale_load, y=loading[i,2]*scale_load,
           labels = row.names(loading)[i])
    }
  }
  
  # étiquettes
  if(!is.null(group)) {
    if(label) {
      for (i in 1:length(levels(group))) {
        score_gr = score[group == levels(group)[i],]
        if(!is.null(nrow(score_gr))) { # if only one instance, skip
          centre_gr = apply(score_gr, 2, "mean")
          text(x=centre_gr[1], y=centre_gr[2], labels=levels(group)[i], cex=cex_lab)
        } else {
          text(x=score_gr[1], y=score_gr[2], labels=levels(group)[i], cex=cex_lab)
        }
      } 
    }
  }
  
  # légende
  if(!is.null(group)) {
    if(legend) {
      legend('topright', legend=levels(group), pch = 16, col=palette())
    }
  }
  
  # reset the palette to default
  palette("default") 
}