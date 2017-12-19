biclass <- function(x, y, xl, yl, limit.form=1) {
  classification <- vector(length=length(x))
  if (limit.form==1) {
    classification[which(x<xl & y>=yl)] <- "TN"
    classification[which(x<=xl & y<yl)] <- "FN"
    classification[which(x>xl & y<=yl)] <- "TP"
    classification[which(x>=xl & y>yl)] <- "FP"
  } else if (limit.form==2) {
    classification[which(x<=xl & y>yl)] <- "TN"
    classification[which(x<xl & y<=yl)] <- "FN"
    classification[which(x>=xl & y<yl)] <- "TP"
    classification[which(x>xl & y>=yl)] <- "FP"
  }
  classification[which(x==xl & y==yl)] <- "TN" # on the exact point of the center of the cross
  lTN <- length(classification[classification=="TN"])
  lFN <- length(classification[classification=="FN"])
  lTP <- length(classification[classification=="TP"])
  lFP <- length(classification[classification=="FP"])
  sensitivity <- lTP/(lTP+lFN)
  specificity <- lTN/(lFP+lTN)
  PPV <- lTP/(lTP+lFP)
  NPV <- lTN/(lTN+lFN)
  accuracy <- (lTN+lTP)/(lTN+lTP+lFN+lFP)
  df <- c(xl, yl, lTN, lFN, lTP, lFP, sensitivity, specificity, PPV, NPV, accuracy)
  out <- list(classification=classification, df=df)
  return(out)
}
