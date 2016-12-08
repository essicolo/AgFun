# Methods:
# - Beaufils (1973)
# - Jones (1981)
# - Elwali and Gascho (1984)
# - Beverly ??? (not implemented yet)


DRIS <- function(obs, ref.hy, ref.ly, method="Beaufils") {
  
  # TEST TO ASSURE THAT IF METHOD == BEAUFILS, THEN ref.ly HAS TO BE DEFINED
  # ne fonctionne pas
  
  # CREER DES NOMS DE RATIO
  ratioName <- matrix(nrow=ncol(obs), ncol=ncol(obs))
  for (n in 1:nrow(ratioName)) {
    for (p in 1:ncol(ratioName)) {
      ratioName[n,p] <- paste(colnames(obs)[n],"/",colnames(obs)[p], sep="")
    }
  }
  
  # CREER DES MATRICES VIDES AVEC LES NOMS DE RATIO EN ENTETE
  # Observations
  ratioValue.obs <- matrix(ncol=length(ratioName), nrow=nrow(obs))
  colnames(ratioValue.obs) <- 1:ncol(ratioValue.obs)
  counter <- 1
  for (n in 1:nrow(ratioName)) {
    for (p in 1:ncol(ratioName)) {
      colnames(ratioValue.obs)[counter] <- ratioName[n,p]
      counter <- counter+1
    }
  }
  
  # Reference (hy)
  ratioValue.ref.hy <- matrix(ncol=length(ratioName), nrow=nrow(ref.hy))
  colnames(ratioValue.ref.hy) <- 1:ncol(ratioValue.ref.hy)
  counter <- 1
  for (n in 1:nrow(ratioName)) {
    for (p in 1:ncol(ratioName)) {
      colnames(ratioValue.ref.hy)[counter] <- ratioName[n,p]
      counter <- counter+1
    }
  }
  
  # Reference (ly)
  ratioValue.ref.ly <- matrix(ncol=length(ratioName), nrow=nrow(ref.ly))
  colnames(ratioValue.ref.ly) <- 1:ncol(ratioValue.ref.ly)
  counter <- 1
  for (n in 1:nrow(ratioName)) {
    for (p in 1:ncol(ratioName)) {
      colnames(ratioValue.ref.ly)[counter] <- ratioName[n,p]
      counter <- counter+1
    }
  }
  
  # CREER UNE MATRICE INDIQUANT AVEC QUELLE COLONNE CALCULER LES RATIOS
  ratioInd <- matrix(ncol=2, nrow=ncol(ratioValue.obs))
  counter <- 1
  for (n in 1:nrow(ratioName)) {
    for (p in 1:ncol(ratioName)) {
      ratioInd[counter,1] <- n
      ratioInd[counter,2] <- p
      counter <- counter+1
    }
  }
  
  # ENLEVER LES RATIOS A/A
  ratioValue.obs <- ratioValue.obs[,which(ratioInd[,1] != ratioInd[,2])]
  ratioValue.ref.hy <- ratioValue.ref.hy[,which(ratioInd[,1] != ratioInd[,2])]
  ratioValue.ref.ly <- ratioValue.ref.ly[,which(ratioInd[,1] != ratioInd[,2])]
  ratioInd <- ratioInd[which(ratioInd[,1] != ratioInd[,2]),]
  
  # REMPLIR LES MATRICES VIDES DE CALCULS DE RATIO
  # Observations
  for (n in 1:nrow(ratioValue.obs)) {
    for (p in 1:ncol(ratioValue.obs)) {
      ratioValue.obs[n,p] <- obs[n,ratioInd[p,1]]/obs[n,ratioInd[p,2]]
    }
  }
  
  # Reference (hy)
  for (n in 1:nrow(ratioValue.ref.hy)) {
    for (p in 1:ncol(ratioValue.ref.hy)) {
      ratioValue.ref.hy[n,p] <- ref.hy[n,ratioInd[p,1]]/ref.hy[n,ratioInd[p,2]]
    }
  }
  
  # Reference (ly)
  for (n in 1:nrow(ratioValue.ref.ly)) {
    for (p in 1:ncol(ratioValue.ref.ly)) {
      ratioValue.ref.ly[n,p] <- ref.ly[n,ratioInd[p,1]]/ref.ly[n,ratioInd[p,2]]
    }
  }
  
  ratioMean.ref.hy <- apply(ratioValue.ref.hy, 2, "mean") # attention, la moyenne d'un ratio n'est pas égale à l'inverse de la moyenne des ratios inverses
  ratioSD.ref.hy <- apply(ratioValue.ref.hy, 2, "sd") 
  ratioSD.ref.ly <- apply(ratioValue.ref.ly, 2, "sd")
  CV.ref.hy <- ratioSD.ref.hy/ratioMean.ref.hy
    
  
  # FONCTIONS
  f <- matrix(ncol=ncol(ratioValue.obs), nrow=nrow(ratioValue.obs)) # matrice vide des foncions DRIS
  if (method == "Beaufils") {
    for (p in 1:ncol(ratioValue.obs)) {
      # index of the inverse ratio
      inverse <- which(ratioInd[,1]==ratioInd[p,2] & ratioInd[,2]==ratioInd[p,1])
      
      # ratio between variance of hy and ly for the inverse (I) and current (P) ratios
      varRatioI <- ratioSD.ref.hy[inverse]^2 / ratioSD.ref.ly[inverse]^2
      varRatioP <- ratioSD.ref.hy[p]^2 / ratioSD.ref.ly[p]^2
      
      # if the variance of the inverse is higher, the function of ratio P is null
      if (varRatioI > varRatioP) {
        f[,p] <- rep(0, times=length(f[,p]))
      } else {
        for (n in 1:nrow(ratioValue.obs)) {
          
          if(ratioValue.obs[n,p] > ratioMean.ref.hy[p]) {
            f[n,p] <- (ratioValue.obs[n,p]/ratioMean.ref.hy[p]-1)/CV.ref.hy[p]
          } else {
            f[n,p] <- (1-ratioMean.ref.hy[p]/ratioValue.obs[n,p])/CV.ref.hy[p]
          }
        }
      }
    }
  } else if (method == "Jones") {
    for (p in 1:ncol(ratioValue.obs)) {
      inverse <- which(ratioInd[,1] == ratioInd[p,2] & ratioInd[,2] == ratioInd[p,1])
      varRatioI <- ratioSD.ref.hy[inverse]^2 / ratioSD.ref.ly[inverse]^2
      varRatioP <- ratioSD.ref.hy[p]^2 / ratioSD.ref.ly[p]^2
      
      if (varRatioI > varRatioP) {
        f[,p] <- rep(0, times = length(f[,p]))
      } else {
        for (n in 1:nrow(ratioValue.obs)) {
          f[n,p] <- (ratioValue.obs[n,p] - ratioMean.ref.hy[p])/CV.ref.hy[p]
        }
      }
    }
  } else if (method == "Elwali-Gascho") {
    for (p in 1:ncol(ratioValue.obs)) {
      inverse <- which(ratioInd[,1]==ratioInd[p,2] & ratioInd[,2]==ratioInd[p,1])
      varRatioI <- ratioSD.ref.hy[inverse]^2/ratioSD.ref.ly[inverse]^2
      varRatioP <- ratioSD.ref.hy[p]^2/ratioSD.ref.ly[p]^2
      
      if (varRatioI > varRatioP) {
        f[,p] <- rep(0, times=length(f[,p]))
      } else {
        for (n in 1:nrow(ratioValue.obs)) {
          
          if(ratioValue.obs[n,p] < ratioMean.ref.hy[p]-CV.ref.hy[p]) {
            f[n,p] <- (ratioValue.obs[n,p]/ratioMean.ref.hy[p]-1)/CV.ref.hy[p]
          } else if(ratioValue.obs[n,p] > ratioMean.ref.hy[p]+CV.ref.hy[p]) {
            f[n,p] <- (1-ratioMean.ref.hy[p]/ratioValue.obs[n,p])/CV.ref.hy[p]
          } else {
            f[n,p] <- 0
          }
        }
      }
    }
  } else if (method == "Beverly") {
    for (p in 1:ncol(ratioValue.obs)) {
      # index of the inverse ratio
      inverse <- which(ratioInd[,1]==ratioInd[p,2] & ratioInd[,2]==ratioInd[p,1])
      
      # ratio between variance of hy and ly for the inverse (I) and current (P) ratios
      varRatioI <- ratioSD.ref.hy[inverse]^2 / ratioSD.ref.ly[inverse]^2
      varRatioP <- ratioSD.ref.hy[p]^2 / ratioSD.ref.ly[p]^2
      
      # if the variance of the inverse is higher, the function of ratio P is null
      if (varRatioI > varRatioP) {
        f[,p] <- rep(0, times=length(f[,p]))
      } else {
        for (n in 1:nrow(ratioValue.obs)) {
          f[n,p] <- abs((log(ratioValue.obs[n,p]) - log(ratioMean.ref.hy[p]))) / ratioSD.ref.hy[p]
        }
      }
    }
  } 
  
  # INDICES DRIS
  Index <- obs
  for (n in 1:nrow(ratioValue.obs)) {
    for (p in 1:ncol(obs)) {
      Index[n,p] <- sum(f[n,ratioInd[,1]==p])/length(f[n,ratioInd[,1]==p]) - 
                    sum(f[n,ratioInd[,2]==p])/length(f[n,ratioInd[,2]==p])
    }
  }
  #rowSums(Index)
  
  IBN <- rowSums(abs(Index))/ncol(obs)
  
  
  out <- list()
  out$Index <- Index
  out$IBN <- IBN
  return(out)
}

