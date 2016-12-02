# ------------------------------------------------------------------------------------
  ##################################### Meta ##########################################
# Power Calculations and simulations
# Contributors: Krishanu Chakraborty
# 27th November 2016
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Version of code : 1.0.0
# R version : R version 3.3.2 (Sincere Pumpkin Patch)
# Last edited by : Krishanu Chakraborty
# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
#################################### Introduction ###################################

# Why are power calculations important?

# Power influences many design aspects, including what research questions to pursue, how
# many treatment arms to employ, and even more fundamentally, whether or not to proceed
# with a potential research project.  For example, it may be that a remedial education
# program boosts tests scores by 20 percent when comparing treatment and control groups,
# but due to limited power, the RCT is unable to detect this true effect (with 95%
#                                                                         confidence). However, we can estimate whether a given design is likely to be able to
# detect a reasonable effect size ex ante, allowing us to properly manage partner
# expectations and make the most of limited research resources.
# 
# This exercise will cover two ways of calculating power for a RCT - (1) the conventional
# parametric method, and (2) a non-parametric "simulation" method.
# 
# Questions to consider before running power calculations
# 
# What is the main specification (e.g. regression) we plan to run? 
# [It doesn't have to be fully baked, but the more "baked" it is, the more precise we can make your power
#  estimates.]
# 
# What do we expect to be the mean of the outcome in the control group?
#  
# How about the standard deviation (SD) of the outcome in control group? [If your
# outcome is binary, we can estimate this without baseline data by assuming this
# Bernoulli random variable takes a binomial distribution.]
# 
# What sample sizes are feasible?
# 
# What effect sizes could the intervention reasonably cause?
#  
# What is the smallest, cost-effective effect size that we are interested in? [We often
# arrive at a reasonable answer to this question through discussions with partner 
# organizations and literature reviews.]
# 
# Let's start with a simple parametric example
# load packages
# Please install these packages if you do't have them.
# install.packages(c("gdata","foreign", "Hmisc", "samplesize", "compute.es", "foreach", "ICC", 
# "rjava", "xlsx", "ri", "stargazer", "clusterSEs", "broom", "dplyr", "randomizr"))
# -----------------------------------------------------------------------------

library(gdata)
library(foreign)
library(Hmisc)
library(samplesize)
library(compute.es)
library(foreach)
library(ICC)
library(rJava)
library(xlsx)
library(ri)
library(stargazer)
library(clusterSEs)
library(broom)
library(dplyr)
library(randomizr)


# Please ignore all warnings in loading the packages

# ---------------------------------------------------------------------------------
############################### EXAMPLE 1: Basic Parametric Example ##############
# ---------------------------------------------------------------------------------

# Let's download the zip file containing the Stata do file and the dataset 
# and unzip it to the working directory.
getwd()
setwd("D:/R/J-PAL Power Tutorial") # set working directory that you want to set

download.file("https://www.povertyactionlab.org/sites/default/files/resources/Power-calculations-stata.zip"
              , dest="dataset.zip", mode="wb")

unzip ("dataset.zip", exdir = getwd())

# Let's load the Balsakhi dataset. We'll use this dataset to estimate the control mean.

mydata <- read.dta("D:/R/J-PAL Power Tutorial/Power Calculations_2016/baroda_0102_1obs.dta") 

# view data 

my_data_table <- data.frame(mydata)
View(my_data_table)

# For all parametric power calculations, we'll assume a conventional 95% confidence
# interval and 80% power.

# What do we expect the mean of the outcome to be in the control group?

# Note: Since power calculations are usually done prior to a study, we often use
# baseline/pilot data on the study population, or government statistics for a comparable
# population, to get an approximate for this outcome in the control group.

my_sd <- sd((subset(my_data_table$post_totnorm, my_data_table$bal == 0)), na.rm = TRUE)
my_sd
my_control <- mean((subset(my_data_table$post_totnorm, my_data_table$bal == 0)), na.rm = TRUE)
my_control

# Let's say, based on other studies, that we expect an effect size of a tenth of a
# standard deviation. Now let's calculate the sample size given that we know the likely
# effect size.

my_effect <- my_sd/10
my_treat <- my_effect + my_control

# treat in the STATA exercise is equivelent to my_treat in this R exercise
# control is the STATA exercise is equivalent to my_control in this R exercise
# Let us check the variables that we have

my_treat
my_control
my_effect
my_sd

# Calculate the sample size.
# We can take a look at sample_calculation.R now to understand the fucntion


source("sample_calculation.R")
sampsi.mean(my_control,my_treat,sd = my_sd)

# Say, instead, we knew the sample size and wanted to calculate the Minimum Detectable
# Effect Size (MDE).You can  manually calculate the effect size

sample_n <<- nA+nB
my_mde <- (0.842+1.96)*sqrt(1/0.25)*sqrt(1/(nA+nB))*my_sd
my_mde

# The effect size should be .1151. Verify you get this result before proceeding. Let's
# compare this to the likely effect size that went into the first sample size
# calculation:

my_effect
  
# As this shows, it doesn't matter which we start with - sample size or effect size.
# Some other questions to answer before calculating power:
#   - Will this study be cluster-randomized?
#   - Will our main specification include controls (i.e. lagged dependent variables)?
#   - Do we expect only part of the treatment group to take-up the intervention? If so, are
#     we interested in estimating the local average treatment effect?
#     We'll address each of these questions one at a time to see how they affect power

# ----------------------------------------------------------------------------------------------
########################  EXAMPLE 2: Building Intuition #######################################
# ----------------------------------------------------------------------------------------------

# Now, let's get a better intuition on how a larger or smaller sample size affects our
# power to pick up an effect.
# 
# Say our anticipated effect size is smaller than originally thought; how much larger
# would we need to make the sample in order to still pick up an effect?
# 
# Let's try an effect size that is half as large.

new_my_effect <- my_effect/2
new_my_treat <- new_my_effect + my_control
source("sample_calculation.R")
sampsi.mean(my_control,new_my_treat,sd = my_sd)

# Observation: The minimum sample required is four times as large.

# Let's try an effect size that is a third of the original.

new_my_effect <- my_effect/3
new_my_treat <- new_my_effect + my_control
source("sample_calculation.R")
sampsi.mean(my_control,new_my_treat,sd = my_sd)

# Observation: The minimum sample required is now nine times as large.

# Remember: If our MDE decreases by a factor of X, the required sample size increases by
# the square of X!

# You can verify this from the other side i.e. look at
# the impact on the MDE of increasing your sample size by a factor of X. Say X is 4:


new_sample <- 4*(3142) # Global n of the STATA exercise
new_my_mde <- (0.842+1.96)*sqrt(1/0.25)*sqrt(1/(new_sample))*my_sd
new_my_mde/my_mde

# ---------------------------------------------------------------------------------------------
#####################  EXAMPLE 3: Parametric Power Calculation with Controls ##################
# ---------------------------------------------------------------------------------------------
 
# Now, say we plan to control for baseline covariates in our main specification.  The
# inclusion of these controls will improve our power, since they explain some of the
# variance in our outcome. For example, including prior test scores on the right-hand
# side as controls when our left-hand side variable is test scores during the study
# period, can improve power if prior test scores predict future test scores. Economists
# will often control for a lagged dependent variable (recorded prior to randomization) if
# it makes a first order improvement in power.

# To see how potential controls affect power, we would ideally have access to a sample
# data set (e.g. historical or pilot data).  With these data, we would want to regress
# Y_i (the outcome) on X_i (the controls) to evaluate how much variance is explained by
# the set of covariates we plan to include. With access to historical data, for example,
# this would involve regressing last year's test scores (Y_i = Y_t-1) on test scores from
# the year before (X_i = Y_t-2).
# 
# From this regression, we are interested in the residual standard deviation of the
# outcome variables, or the variance of the outcome that is NOT explained by controls.
# This residual SD becomes the new SD we include in our parametric power calculations.

# Part 1. We have pilot/historical data.

# Using Balsakhi data, this would be:


my.data.frame <- as.data.frame(subset(my_data_table, my_data_table$bal == 0), na.rm = TRUE)
lm1 <- lm(my.data.frame$post_totnorm ~ my.data.frame$pre_totnorm, data = my.data.frame,
          na.action = na.exclude)
summary(lm1)

predicted_lm1 <- predict(lm1)
summary(lm1)
res_my_sd <- sd(residuals(lm1), na.rm = TRUE)

augment(lm1, lm1$)

# If we knew the effect size and wanted to know the sample size needed.
# The result is different from STATA because the res_my_sd value is not matching.
# I guess this is because of the inherent calcualation types

source("sample_calculation.R")
sampsi.mean(my_control,my_treat,sd = res_my_sd)
sampsi.mean(my_control,my_treat, sd = 0.82548809) # hard-coded

# Questions:
#   
#   1. What percent of the variance of study period test scores is explained by test scores
#      at baseline? (Hint: Look at the R^2 statistic of the regression.)
# 
#   2. How does this affect our sample size (compared to not including controls)?
# 
#   3. How about our MDE?

# Part 2. We do not have pilot/historical data for our study population, but plan to
# include controls.

# We must first guess the percentage of variance in outcome variables that we expect to
# be explained by controls. (This can be done by looking at other datasets with similar
#                            outcomes, etc.)
# 
# Say our controls explain 49% of the outcome. We can then calculate the residual
# standard deviation as follows.

new_res_my_sd <- sqrt(.51*my_sd^2)
sampsi.mean(my_control,my_treat,sd = new_res_my_sd)

# We can also trace out an MDE "range" using various assumptions for how much of the
# variance of the outcome is explained by covariates:

z<-seq.int(10,50,length.out = 5)
for (x in z)
{
  new_x<-(1-x/100)
  new_res_my_sd <- sqrt(new_x*my_sd^2)
  print(sampsi.mean(my_control,my_treat,sd = new_res_my_sd))
  cat("the percentage of variance explained by covariates = ", x, "\n")
}

# -------------------------------------------------------------------------------------------------------------
###########################  EXAMPLE 4: Parametric Power Calculation for Cluster-RCTs #########################
# -------------------------------------------------------------------------------------------------------------


# Many designs randomize at the group level instead of at the individual level. For such
# designs, we need to adjust our power calculations so that they incorporate the fact
# that individuals within the same group may be subject to similar shocks, and thereby
# have correlated outcomes. Duflo et al. presents a modified parametric approach, which
# takes into account the intra-cluster correlation (ICC) that arises from randomization
# at the group level.
# 
# We can think of cluster-RCTs as follows:
#   
#    - When ICC = 0, then our N is effectively the number of individuals in the study.
# 
#    - When ICC = 1, then our N is effectively just the number of clusters.
# 
#    - Usually the ICC lies somewhere between 0 and 1, requiring that we adjust our power
#      calculations to account for this.
# 
# Below we adjust Stata's power estimates based on Duflo et al.'s model.
# 
# Note: This model assumes that all clusters are of the same size and have the same
# number of individuals.  It's usually okay if this is violated in reality, but you would
# not want to use these adjustments if groups are dramatically different in size (e.g.
# group one has 10 individuals, group two has 1,000 individuals.) More on this model is
# explained in Duflo et al.'s article  "Using Randomization in Development Economics
# Research: A Toolkit." (http://economics.mit.edu/files/806)

# Part 1. Calculating MDE

# First, let's calculate the intra-cluster correlation (ICC) which measures how
#     correlated the error terms of individuals in the same cluster are.

my_icc_vec <- ICCest(my.data.frame$divid,my.data.frame$post_totnorm)
my_rho <- as.numeric(my_icc_vec[1])

# Now, let's specify the number of individuals in each cluster...

m <- as.numeric(53)

# and the number of clusters (as documented in the Balsaki experiment).

j <- as.numeric(193)

# Let's assume 95% confidence intervals and 80% power...

t <- as.numeric(1.96 + .842) 

# and 50% of the study population is assigned to treatment and 50% to control.

P <- as.numeric(.5) 

# Now, we have Duflo et al.'s power adjustment:

my_mde <- as.numeric(t*sqrt(1/(P*(1-P)*j))*sqrt(my_rho + (1-my_rho)/m)*my_sd) 
my_mde_ldv <-as.numeric(t*sqrt(1/(P*(1-P)*j))*sqrt(my_rho + (1-my_rho)/m)*res_my_sd) 

# And lastly, the total N of our study and the number who are treated, respectively.

n <- as.numeric(j*m)
treated <- as.numeric(n*P)

matrix_data <- c(.05, 0.8, my_rho,j, m, n, treated, my_control, my_sd, my_mde,
                 .05, 0.8, my_rho,j, m, n, treated, my_control, my_sd, my_mde_ldv)
matrix_name <- list(c("No_controls", "Control_LDV"),c("Signif", "Power", "ICC", "Clusters", "Cluster_size", "N", "Treated",
                      "Cntrl_mn", "Cntrl_SD", "MDE"))
mde_clus <- matrix(data = matrix_data, nrow =2, ncol = 10, byrow = 2, dimnames = matrix_name  )
mde_clus

#  Part 2. Calculating Sample Size

# In stata,sampclus command can be used in combination with the sampsi command used above
# to incorporate clustering into our sample size calculations. A more parsimonious
# alternative is the clustersampsi command. However, note that clustersampsi rounds the
# group means to the tenth place after the decimal, making for a less precise
# calculation.

# sampsi and sampclus are emulated in sample_calucation.R

# adjust for the number of individuals per cluster.

new_my_treat2 <- my_control + my_mde
sampsi.mean(my_control,new_my_treat2,sd = my_sd)
samp.clus(nA, nB, my_rho, m)

# If we include the lagged dependent variable as a control we have

my_treat_ldv <- my_control + my_mde_ldv
sampsi.mean(my_control,my_treat_ldv,sd = res_my_sd )
samp.clus(nA, nB, my_rho, m)

# Note that again it doesn't matter if we start with the MDE, or with the sample size.
# 
# Questions:
#  
#    1. Why do we have to adjust power for clustering when running a cluster-RCT?
#  
#    2. Assuming ICC>0, does adding a new cluster of 5 individuals or adding 5 individuals
#       to already-existing clusters give us more power to detect effects?

# -----------------------------------------------------------------------------------------
############ EXAMPLE 5: Parametric Power Calculation with Partial Take-up #################
# -----------------------------------------------------------------------------------------

# In randomized designs, it is common that there is partial take-up of the intervention.
# For example, in the Oregon Health Insurance Experiment (http://nber.org/oregon/), the offer to apply for health
# insurance was associated with only a 25 percentage point increase in take-up of health
# insurance. When take-up is not 100 percent, researchers are often interested in the
# answers to second stage questions, such as what the average effect of becoming insured
# is on health care utilization.

# Below, we provide code that take into account that only some 
# individuals in the treatment group take up the intervention.

eff_tu <- .9 - .1

# Now we calculate the adjusted MDE by multiplying the unadjusted effect size times the
# effective take-up rate

adjusted_mde <- ((0.842+1.96)*sqrt(1/0.25)*sqrt(1/n)*my_sd)*eff_tu
adjusted_mde

# If we want to estimate sample size, we follow a similar procedure:

my_treat_adjusted <- my_control + adjusted_mde
sampsi.mean(my_control, my_treat_adjusted, sd = my_sd)

# Note that unlike with the MDE, partial take-up affects sample size quadratically; we
# divide estimates of sample size by the square of the effective take-up rate!
#   
# Note: For more on partial take-up and how it affects estimation of the power to detect
# local average treatment effects, please see the aforementioned article by Duflo et al.

# ------------------------------------------------------------------------------------
####################### EXAMPLE 6: Non-parametric Power Simulations ##################
# ------------------------------------------------------------------------------------

# Non-parametric power simulations do better than parametric power calculations when we
# have access to good data (historical, baseline, or pilot) on our study population. From
# these data, we can simulate a fake dataset that assumes the treatment has no effect and
# then see what effects we are powered to detect, by looking at the simulated 95%
# confidence interval around our null effect. We should expect that any effect greater
# than this confidence interval would be detected by our study.
# 
# In particular, power simulations do not require the assumption that the sampling
# distribution of your Beta coefficient(s) of interest takes a normal distribution in
# your (finite) sample. You may be particularly worried about this assumption (of
# parametric power calculations) if your sample is very small.
# 
# To do non-parametric power simulations we need to create a (reasonable) "fake" or
# simulated dataset. For example, if you have baseline data for the 3 months prior to a
# 12 month trial, then a reasonable way to expand this dataset would be to simply
# randomly draw days with replacement until you have 365 days in your dataset. Similarly,
# you could use historical data from the two years prior to the study to estimate the
# confidence interval around a null effect in the past year, with data on your outcome
# variable from two years ago serving as controls.
# 
# Let's do an example using Balsakhi data to make this procedure more clear.

# For this example, we expand study data for the control group of the Balsakhi experiment
# by 2, since this is similar to what would be available from historical data.

testdataframe <- subset(my_data_table, my_data_table$bal == 0)

# View(testdataframe)

testdataframe1 <- testdataframe
testdataframe1$studentid <- (-1)*testdataframe1$studentid
testdataframe_merged <- rbind.data.frame(testdataframe1, testdataframe)
keeps <- c("studentid", "pre_tot", "mid_tot", "post_tot", "divid", "pre_totnorm", 
           "mid_totnorm", "post_totnorm")
data_frame_excel <- data.frame(testdataframe_merged[,keeps, drop= FALSE])
data_frame_excel_sorted<- data_frame_excel[order(data_frame_excel$studentid),]
nrow(testdataframe_merged)
summary.data.frame(data_frame_excel_sorted)
nrow(data_frame_excel_sorted)

write.csv(data_frame_excel_sorted, "bal_power_data_r.csv", sep = ",", 
          row.names = FALSE, quote =FALSE, na= "NA") #giving a warning. Can be ignored

# Step 2. Write randomization code as you plan to randomize.
# Step 3. Write simulation code.
# 
# Your randomization code should take into account how you plan to randomize, including
# your plan to stratify, etc.

# This power simulation program does the following:
#   
# (a) specifies main regression equation,
# 
# (b) simulates treatment assignment and runs main regressions 1000 times,
# 
# (c) summarizes the results from these regressions into an easy-to-read matrix.
# 
# To reduce the run-time, we have set this program to run over 100 iterations,
# but if you are truly calculating power via simulation, you should set this at 1000.

alpha <- 0.05                                   # Standard significance level
sims <- 100                                      # Number of simulations to conduct

#initialize a matrix to collect results

colnames <- c("pvalue" , "tstat", "control_mean", "control_sd", "beta", "se", "ci_low", "ci_high")
results <- matrix(nrow = sims, ncol = 8)
colnames(results) <- colnames


# Part 1. Basic MDE (clustering on divid because data is from C-RCT)  
# loop to conduct experiments "sims" times over#

  for (i in 1:sims)
{
    Data <- as.data.frame(data_frame_excel_sorted, na.rm = TRUE)
    
    # We do a clustered random assignment using divid as the cluster
    
    clust_var <- with(Data, Data$divid)
    
    #Assign treatment
    
    Data$treatment <- cluster_ra(clust_var = clust_var)
    View(Data)
    
    # Do analysis (Simple regression)
    fit.sim <- lm(Data$post_totnorm ~ Data$treatment, data = Data, na.action = na.exclude)      
    # call for clsutered SE  by divid
    source("Clustered_SE.R")
    output <- super.cluster.fun(fit.sim, Data$divid)
    print(output)
  }


# Part 2. Controlling for LDV
# loop to conduct experiments "sims" times over#

for (i in 1:sims)
{
  Data.ldv <- as.data.frame(data_frame_excel_sorted, na.rm = TRUE)
  
  # We do a clustered random assignment using divid as the cluster
  
  clust_var <- with(Data.ldv, Data.ldv$divid)
  
  #Assign treatment
  
  Data.ldv$treatment <- cluster_ra(clust_var = clust_var)
  
  # Do analysis (Simple regression)
  fit.sim.ldv <- lm(Data$post_totnorm ~ Data$pre_totnorm, data = Data.ldv, na.action = na.exclude)      
  # call for clsutered SE  by divid
  source("Clustered_SE.R")
  output <- super.cluster.fun(fit.sim.ldv, Data.ldv$divid)
  print(output)
}


# Since the Balsakhi dataset has many observations, we find that our simulated power estimates of MDE are very similar to those found through parametric estimates. To see
# this, compare Example 4 (parts 1 & 2) to Example 6 (parts 1 & 2).
# 
# As you can see, it is much easier to cluster or control for covariates using this method; you simply run the regression specification that you intend to use at the
# analysis stage.
# 
# Similarly, if you wanted to estimate the (local) average treatment effect in the presence of partial take-up of the intervention, then you can simply make basic
# assumptions on the effective take-up rate, and run your two-stage least squares regression in Stata.
# 
# Generally speaking, parametric power calculations are great for back-of-the-envelope calculations of power, while non-parametric simulations provide more precise estimates
# of power when you have good baseline data.
# 
# Questions:
#   
#   1. How is "non-parametric" power different from "parametric" power?
# 
# 2. How do our parametric and non-parametric estimates of power compare? (Hint: Compare Example 4 to Example 6.)
# 
# 3. When would you want to run parametric power calculations?
# 
# 4. Non-parametric simulations?
# 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   
#   7. Answer Key
# 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   
#   Ex. 3, part 1:
#   
#   1. 49%
# 
# 2. Including controls cuts sample size almost in half.
# 
# 3. Reduces MDE to a lesser extent (~28%).
# 
# Ex. 4, part 2:
#   
#   1. Assignment is only random at the cluster level; thus we must cluster our standard errors in our main specification. To this end, our power calculations must also take
# this into account, since, by clustering in our main specification, we will lose all precision gained from intra-cluster correlation in outcomes.
# 
# 2. Adding a new cluster of 5 individuals, since in the limiting case of ICC=1, adding 5 individuals to previous clusters would have no effect on power, while adding 5
# individuals in a new cluster would increase our effective N by 1.
# 
# Ex. 6:
#   
#   1. Non-parametric power simulations do not require the assumption that beta coefficients take a normal distribution (which follows from the central limit theorem), while
# parametric power calculations do.
# 
# 2. Our power simulations in Example 6 find that we are powered to detect an ~.20 SD increase in test scores without controls and a ~.14 SD increase in test scores when
# including a lagged dependent variable. Using parametric methods, Example 4 illustrates these are .19 SD and .13 SD, respectively. Thus, we find very similar results using
# each method.
# 
# 3. We would want to run parametric power calculations when we want a quick, rough estimate of power, or when we do not have access to high quality baseline data.
# 
# 4. We would want to run non-parametric simulations when we have access to high-quality baseline data and have reason to believe parametric assumptions may be violated. For
# example, we would prefer to run simulations if our randomization is complex (i.e. multiple randomizations), have a small sample size, or are running complex
# specifications.



#end
