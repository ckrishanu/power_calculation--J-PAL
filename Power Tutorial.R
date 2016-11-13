#load packages
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


#---------------------------------------------------------------------------------
############################### EXAMPLE 1: Basic Parametric Example ##############
#---------------------------------------------------------------------------------

#Let's download the zip file containing the Stata do file and the dataset 
#and unzip it to your working directory.

setwd("D:/R/J-PAL Power Tutorial") #set working directory

download.file("https://www.povertyactionlab.org/sites/default/files/resources/Power-calculations-stata.zip"
              , dest="dataset.zip", mode="wb")
unzip ("dataset.zip", exdir = getwd())

# Let's load the Balsakhi dataset. We'll use this dataset to estimate the control mean.

mydata<-read.dta("D:/R/J-PAL Power Tutorial/Power Calculations_2016/baroda_0102_1obs.dta") 

#view data 

my_data_table<-data.frame(mydata)
View(my_data_table)

#For all parametric power calculations, we'll assume a conventional 95% confidence
#interval and 80% power.

#What do we expect the mean of the outcome to be in the control group?

#Note: Since power calculations are usually done prior to a study, we often use
#baseline/pilot data on the study population, or government statistics for a comparable
#population, to get an approximate for this outcome in the control group.

my_sd <- sd((subset(my_data_table$post_totnorm, my_data_table$bal == 0)), na.rm = TRUE)
my_sd
my_control <- mean((subset(my_data_table$post_totnorm, my_data_table$bal == 0)), na.rm = TRUE)
my_control

#Let's say, based on other studies, that we expect an effect size of a tenth of a
#standard deviation. Now let's calculate the sample size given that we know the likely
#effect size.

my_effect <- my_sd/10
my_treat <- my_effect + my_control

#treat is equivelent to my_treat
#control is equivalent to my_control
my_treat
my_control
my_effect
my_sd

#Calculate the sample size. This will be a more improved function
source("sample_calculation.R")
sampsi.mean(my_control,my_treat,sd = my_sd)
ceiling(nA)
ceiling(nB)

#Say, instead, we knew the sample size and wanted to calculate the Minimum Detectable
#Effect Size (MDE).You can  manually calculate the effect size

my_mde <- (0.842+1.96)*sqrt(1/0.25)*sqrt(1/(nA+nB))*my_sd
my_mde

#The effect size should be .1151. Verify you get this result before proceeding. Let's
#compare this to the likely effect size that went into the first sample size
#calculation:

my_effect
  
#As this shows, it doesn't matter which we start with - sample size or effect size.
#Some other questions to answer before calculating power:
#- Will this study be cluster-randomized?
#- Will our main specification include controls (i.e. lagged dependent variables)?
#- Do we expect only part of the treatment group to take-up the intervention? If so, are
#we interested in estimating the local average treatment effect?
#We'll address each of these questions one at a time to see how they affect power

########################  EXAMPLE 2: Building Intuition #######################################

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
  

new_sample <- 4*(nA+nB)
new_my_mde <- (0.842+1.96)*sqrt(1/0.25)*sqrt(1/(new_sample))*my_sd
new_my_mde/my_mde

#####################  EXAMPLE 3: Parametric Power Calculation with Controls ##############################

# Now, say we plan to control for baseline covariates in our main specification.  The
# inclusion of these controls will improve our power, since they explain some of the
# variance in our outcome. For example, including prior test scores on the right-hand
# side as controls when our left-hand side variable is test scores during the study
# period, can improve power if prior test scores predict future test scores. Economists
# will often control for a lagged dependent variable (recorded prior to randomization) if
# it makes a first order improvement in power.
# 
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


my.data.frame <- (subset(my_data_table, my_data_table$bal == 0))
lm1 <- lm(my.data.frame$post_totnorm~my.data.frame$pre_totnorm)
res_my_sd <- sigma(lm1)
res_my_sd

# If we knew the effect size and wanted to know the sample size needed.
#The result is different from STATA. Need to check

source("sample_calculation.R")
sampsi.mean(my_control,my_treat,sd = res_my_sd)

# Questions:
#   
#   1. What percent of the variance of study period test scores is explained by test scores
# at baseline? (Hint: Look at the R^2 statistic of the regression.)
# 
# 2. How does this affect our sample size (compared to not including controls)?
# 
# 3. How about our MDE?

# Part 2. We do not have pilot/historical data for our study population, but plan to
#include controls.

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
z
foreach(z)
{
  new_z<-(1-z/100)
  new_res_my_sd <- sqrt(new_z*my_sd^2)
  sampsi.mean(my_control,my_treat,sd = new_res_my_sd) #have to put a cbind
}

###########################  EXAMPLE 4: Parametric Power Calculation for Cluster-RCTs #########################

# Many designs randomize at the group level instead of at the individual level. For such
# designs, we need to adjust our power calculations so that they incorporate the fact
# that individuals within the same group may be subject to similar shocks, and thereby
# have correlated outcomes. Duflo et al. presents a modified parametric approach, which
# takes into account the intra-cluster correlation (ICC) that arises from randomization
# at the group level.
# 
# We can think of cluster-RCTs as follows:
#   
#   - When ICC = 0, then our N is effectively the number of individuals in the study.
# 
# - When ICC = 1, then our N is effectively just the number of clusters.
# 
# - Usually the ICC lies somewhere between 0 and 1, requiring that we adjust our power
# calculations to account for this.
# 
# Below we adjust Stata's power estimates based on Duflo et al.'s model.
# 
# Note: This model assumes that all clusters are of the same size and have the same
# number of individuals.  It's usually okay if this is violated in reality, but you would
# not want to use these adjustments if groups are dramatically different in size (e.g.
# group one has 10 individuals, group two has 1,000 individuals.) More on this model is
# explained in Duflo et al.'s article  "Using Randomization in Development Economics
# Research: A Toolkit."

#Part 1. Calculating MDE

# First, let's calculate the intra-cluster correlation (ICC) which measures how
#     correlated the error terms of individuals in the same cluster are.

my_icc_vec<-ICCest(my.data.frame$divid,my.data.frame$post_totnorm)
my_rho<-as.numeric(my_icc_vec[1])
is.numeric(my_rho)

#Now, let's specify the number of individuals in each cluster...

m<-as.numeric(53)

#and the number of clusters (as documented in the Balsaki experiment).

j<-as.numeric(193)

#Let's assume 95% confidence intervals and 80% power...

t =as.numeric(1.96 + .842) 

#and 50% of the study population is assigned to treatment and 50% to control.

P <-as.numeric(.5) 

#Now, we have Duflo et al.'s power adjustment:

my_mde <- as.numeric(t*sqrt(1/(P*(1-P)*j))*sqrt(my_rho + (1-my_rho)/m)*my_sd) 
my_mde_ldv <-as.numeric(t*sqrt(1/(P*(1-P)*j))*sqrt(my_rho + (1-my_rho)/m)*res_my_sd) 

#And lastly, the total N of our study and the number who are treated, respectively.

n <- as.numeric(j*m)
treated <- as.numeric(n*P)

matrix_data <- c(.05, 0.8, my_rho,j, m, n, treated, my_control, my_sd, my_mde,
                 .05, 0.8, my_rho,j, m, n, treated, my_control, my_sd, my_mde_ldv)
matrix_name <- list(c("No_controls", "Control_LDV"),c("Signif", "Power", "ICC", "Clusters", "Cluster_size", "N", "Treated",
                      "Cntrl_mn", "Cntrl_SD", "MDE"))
mde_clus <- matrix(data = matrix_data, nrow =2, ncol = 10, byrow = 2, dimnames = matrix_name  )
mde_clus

#  Part 2. Calculating Sample Size

#adjust for the number of individuals per cluster.

new_my_treat2 <- my_control + my_mde
sampsi.mean_object <- sampsi.mean(my_control,new_my_treat2,sd = my_sd)
samp.clus(sampsi.mean_object[[1]], sampsi.mean_object[[2]], my_rho, m)

#If we include the lagged dependent variable as a control we have

my_treat_ldv <- my_control + my_mde_ldv
sampsi.mean_object.ldv <- sampsi.mean(my_control,my_treat_ldv,sd = res_my_sd )
samp.clus(sampsi.mean_object.ldv[[1]], sampsi.mean_object.ldv[[2]], my_rho, m)

# Note that again it doesn't matter if we start with the MDE, or with the sample size.
# 
# Questions:
#  
# 1. Why do we have to adjust power for clustering when running a cluster-RCT?
#  
# 2. Assuming ICC>0, does adding a new cluster of 5 individuals or adding 5 individuals
#    to already-existing clusters give us more power to detect effects?

############ EXAMPLE 5: Parametric Power Calculation with Partial Take-up #################

# In randomized designs, it is common that there is partial take-up of the intervention.
# For example, in the Oregon Health Insurance Experiment, the offer to apply for health
# insurance was associated with only a 25 percentage point increase in take-up of health
# insurance. When take-up is not 100 percent, researchers are often interested in the
# answers to second stage questions, such as what the average effect of becoming insured
# is on health care utilization.

# Below, we provide code that take into account that only some
#     individuals in the treatment group take up the intervention.

eff_tu <- .9 - .1

#Now we calculate the adjusted MDE by multiplying the unadjusted effect size times the
# effective take-up rate

adjusted_mde <- ((0.842+1.96)*sqrt(1/0.25)*sqrt(1/n)*my_sd)*eff_tu
adjusted_mde

# If we want to estimate sample size, we follow a similar procedure:

my_treat_adjusted <- my_control + adjusted_mde
sampsi.mean(my_control, my_treat_adjusted, sd= my_sd)

# Note that unlike with the MDE, partial take-up affects sample size quadratically; we
# divide estimates of sample size by the square of the effective take-up rate!
#   
# Note: For more on partial take-up and how it affects estimation of the power to detect
# local average treatment effects, please see the aforementioned article by Duflo et al.

####################### EXAMPLE 6: Non-parametric Power Simulations ##################

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
#                                                                              parametric power calculations) if your sample is very small.
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
#View(testdataframe)
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
          row.names = FALSE, quote =FALSE, na= "NA") #giving a warning.

#Step 2. Write randomization code as you plan to randomize.
# 
# Your randomization code should take into account how you plan to randomize, including
# your plan to stratify, etc. For simplicity, we just randomize by grade-year in this
# example and do not stratify (although the actual RCT did stratify).

randomization <- function(testdataframe){
  anyDuplicated(testdataframe$studentid)
  set.seed(20110402)
  testdataframe <- testdataframe[order(testdataframe$divid),]
  testdataframe$random <- runif(nrow(testdataframe))
  testdataframe <- testdataframe[order(testdataframe$random),]
  testdataframe$index <- 1:nrow(testdataframe)
  testdataframe$treatment <- as.numeric(testdataframe$index <= nrow(testdataframe) /2)
  keeps <- c("divid", "treatment" )
  data_frame_rand <- data.frame(testdataframe[,keeps, drop= FALSE])
  write.csv(data_frame_rand, "rand.csv", sep = ",", 
            row.names = FALSE, quote =FALSE, na= "NA") #giving a warning.
  
}

####incomplete


#end

