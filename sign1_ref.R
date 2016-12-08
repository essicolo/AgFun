sign1_ref = function (mv, Y, Ycut, qcrit = seq(0.70, 0.99, 0.01), niter=20)
{
  library(MASS)
  hy <- mv[Y >= Ycut, ]
  p <- ncol(hy)
  n <- nrow(hy)
  p1 <- min(p - 1, n - 1)
  mv.dist.matrix <- list()
  convergence <- matrix(nrow = niter, ncol = length(qcrit))
  quadCount <- data.frame(matrix(ncol = length(qcrit), nrow = 4))
  rownames(quadCount) <- c("TN", "FN", "TP", "FP")
  colnames(quadCount) <- qcrit
  const <- sqrt(qchisq(qcrit, p1))
  accuracy <- c()
  mv.dist <- matrix(nrow = nrow(mv), ncol = length(qcrit))
  
  for (i in 1:length(qcrit)) {
    mv.dist_i <- matrix(ncol = niter, nrow = nrow(mv))
    mv.dist_i[, 1] <- sqrt(mahalanobis(mv, center=apply(hy, 2, median), cov=cov(hy)))
    for (j in 2:niter) {
      #print(paste("i =", i, ";", "j =", j))
      thy <- mv[Y>=Ycut & mv.dist_i[, j-1] < const[i], ]
      tryInv <- try(inv.COV <- ginv(cov(thy)), silent = TRUE)
      if(class(tryInv) == "try-error") {
        convergence[j, i] <- NA
        mv.dist_i[, j] <- NA
      } else {
        mv.dist_i[, j] <- sqrt(mahalanobis(mv, center=apply(thy, 2, median), 
                                                     cov=tryInv, inverted = TRUE))
        convergence[j, i] <- sum(sqrt((mv.dist_i[, j] - mv.dist_i[, j-1])^2))
      }
    }
    mv.dist[, i] <- mv.dist_i[, niter]
    quadCount[1, i] <- sum(mv.dist[, i] < const[i] & Y >= Ycut) # TN
    quadCount[2, i] <- sum(mv.dist[, i] <= const[i] & Y < Ycut) # FN
    quadCount[3, i] <- sum(mv.dist[, i] > const[i] & Y <= Ycut) # TP
    quadCount[4, i] <- sum(mv.dist[, i] >= const[i] & Y > Ycut) # FP
    accuracy[i] <- (quadCount[1, i] + quadCount[3, i]) / length(Y)
  }
  
  list(mv.dist = mv.dist, quadCount = quadCount,
       const = const, accuracy = accuracy, convergence = convergence)
}