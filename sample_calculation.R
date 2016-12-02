# ------------------------------------------------------------------------------------
##################################### Meta ##########################################
# Power Calculations and simulations
# Collaborators: Krishanu Chakraborty
# 27th November 2016
# Based on Slawa Rokicki's blog : R for public health 
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Version : 1.0.0
# Last edited by : Krishanu Chakraborty
# ------------------------------------------------------------------------------------

sampsi.mean <- function (muA, muB, kappa = 1, sd, alpha = 0.05, beta = 0.20)
{
  (nB <<- (1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
  nA <<- kappa*nB
  
  # The following steps are to align the output as the sampsi command shows in STATA
  
  return_list_name <<- c("Alpha Two sided", "Power", "mA", "mB","sd1", "sd2", "nA", "nB")
  return_list_val <<-c(alpha, (1- beta), muA, muB, sd, sd, ceiling(nA),ceiling(nB)) 
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val)) 
  return(return_list)
}

samp.clus <- function (nA, nB, rho, obs_clus)
{
  
  deff <- 1+(obs_clus-1)*rho
  nA.clus <- nA*signif(deff, digits = 6)
  nB.clus <- nA*signif(deff, digits = 6)
  num.clus <- (nA.clus+nB.clus)/obs_clus
  
  # The following steps are to align the output as the sampsclus command shows in STATA
  
  return_list_val <- c(ceiling(nA), ceiling(nB), rho, obs_clus, ceiling(nA.clus), ceiling(nB.clus), num.clus)
  return_list_name <- c("n1 uncorrected", "n2 uncorrected", "interclass correlation", 
                        "Average obs. per cluster", "n1 corrected", "n2 corrected", 
                        "minimum number of clusters")
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val))
  return(return_list)
}
