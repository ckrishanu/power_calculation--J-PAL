sampsi.mean <- function (muA, muB, kappa = 1, sd, alpha = 0.05, beta = 0.20)
{
  (nB=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
  nA=kappa*nB
  return_list <-c(ceiling(nA),ceiling(nB))
  return(return_list)
}

samp.clus <- function (nA, nB, rho, obs_clus)
{
  deff<-1+(obs_clus-1)*rho
  nA.clus<-nA*deff
  nB.clus<-nA*deff
  num.clus<-ceiling((nA.clus+nB.clus)/obs_clus)
  return_list_val <- c(nA, nB, rho, obs_clus, ceiling(nA.clus), ceiling(nB.clus), num.clus)
  return_list_name <- c("n1 uncorrected", "n2 uncorrected", "interclass correlation", 
                        "Average obs. per cluster", "n1 corrected", "n2 corrected", 
                        "minimum number of clusters")
  return_list <- as.data.frame(cbind(return_list_name, return_list_val))
  return(return_list)
  
}
