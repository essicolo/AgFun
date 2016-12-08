compci = function(comp=NULL, bal=NULL, sbp, level=0.95, nbsim=1e5) {
  library(compositions)
  psi = gsi.buildilrBase(t(sbp))
  if(is.null(comp) & is.null(bal)) {
    return(print("Either comp or bal should be defined"))
  } else if(!is.null(comp) & !is.null(bal)) {
    return(print("Both comp and bal are defined. Define one OR the other"))
  } else if(!is.null(comp) & is.null(bal)) {
    bal = ilr(comp, V=psi)
  }
  tt = apply(bal, 2, function(X) t.test(X, conf.level = 1-(1-level)/2))
  bal_sim = list()
  ilr_ci = matrix(ncol=ncol(bal), nrow=3)
  for (i in seq_along(tt)) {
    bal_sim[[i]] = runif(nbsim, tt[[i]]$conf.int[1], tt[[i]]$conf.int[2])
    ilr_ci[1, i] = tt[[i]]$conf.int[1]
    ilr_ci[2, i] = mean(bal[, i])
    ilr_ci[3, i] = tt[[i]]$conf.int[2]
  }
  bal_sim = data.frame(matrix(unlist(bal_sim), ncol=length(tt), byrow=FALSE))
  comp_sim = ilrInv(bal_sim, V=psi)
  comp_range = apply(unclass(comp_sim), 2, range)
  comp_range = rbind(comp_range, comp_range[2, ])
  comp_range[2, ] = mean(comp_sim)
  rownames(ilr_ci) = c("ll", "mean", "ul")
  rownames(comp_range) = c("ll", "mean", "ul")
  return(list(ilr_ci=ilr_ci, comp_range=comp_range))
}
