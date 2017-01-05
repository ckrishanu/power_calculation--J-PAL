##################################### Meta ##########################################
# Power Calculations and simulations
# Collaborators: Krishanu Chakraborty
# 4th January 2017
# Based on Slawa Rokicki's blog : R for public health 
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Version : 1.1.0
# Last edited by : Krishanu Chakraborty

#################################### Sample Size using mean #########################

sampsi.mean <- function (muA, muB, kappa = 1, sd, alpha = 0.05, beta = 0.20)
{
  
  muA <- signif(muA, digits = 6)
  muB <- signif(muB, digits = 6)
  sd <- signif(sd, digits = 6)
  (nB <<- (1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
  nA <<- kappa*nB
  
  # The following steps are to align the output as the sampsi command shows in STATA
  
  return_list_name <<- c("Alpha Two sided", "Power", "mA", "mB","sd1", "sd2", "nA", "nB")
  return_list_val <<-c(alpha, (1- beta), muA, muB, sd, sd, ceiling(nA),ceiling(nB)) 
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val)) 
  return(return_list)
}

################################ Clustered sample size #################################

samp.clus <- function (nA, nB, rho, obs_clus)
{
  nA <- ceiling(nA)
  nB <- ceiling(nB)
  rho <- signif(rho, digits= 8)
  deff <- 1+(obs_clus-1)*rho
  nA.clus <- as.numeric(nA)*signif(deff, digits = 8)
  nB.clus <- as.numeric(nB)*signif(deff, digits = 8)
  num.clus <- (nA.clus+nB.clus)/obs_clus
  num.clus <- ceiling(num.clus)
  
  # The following steps are to align the output as the sampsclus command shows in STATA
  
  return_list_val <- c(ceiling(nA), ceiling(nB), rho, obs_clus, ceiling(nA.clus), ceiling(nB.clus), num.clus)
  return_list_name <- c("n1 uncorrected", "n2 uncorrected", "interclass correlation", 
                        "Average obs. per cluster", "n1 corrected", "n2 corrected", 
                        "Minimum number of clusters")
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val))
  return(return_list)
}

