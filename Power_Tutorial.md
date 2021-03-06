---
output:
  pdf_document: default
  html_document: default
---
Power Calculations and Simulations
================

Introduction
============

Why are power calculations important?
-------------------------------------

Power influences many design aspects, including what research questions to pursue, how many treatment arms to employ, and even more fundamentally, whether or not to proceed with a potential research project. For example, it may be that a remedial education program boosts tests scores by 20 percent when comparing treatment and control groups, but due to limited power, the RCT is unable to detect this true effect (with 95% confidence). However, we can estimate whether a given design is likely to be able to detect a reasonable effect size ex ante, allowing us to properly manage partner expectations and make the most of limited research resources.

This exercise will cover two ways of calculating power for a RCT :

1.  The conventional parametric method, and
2.  A non-parametric "simulation" method.

### Questions to consider before running power calculations

-   What is the main specification (e.g. regression) we plan to run? (It doesn't have to be fully baked, but the more "baked" it is, the more precise we can make your power estimates.)

-   What do we expect to be the mean of the outcome in the control group?

-   How about the standard deviation (SD) of the outcome in control group? (If your outcome is binary, we can estimate this without baseline data by assuming this Bernoulli random variable takes a binomial distribution.)

-   What sample sizes are feasible?

-   What effect sizes could the intervention reasonably cause?

-   What is the smallest, cost-effective effect size that we are interested in? (We often arrive at a reasonable answer to this question through discussions with partner organizations and literature reviews.)

Please install these packages if you don't have them.

``` r
install.packages(c("haven", "ICC", "randomizr"))
```

Once you installed the packages, load them to get started.

``` r
library(haven)
library(ICC)
library(randomizr)
```

Let's start with a simple parametric example.

Example 1. Basic Parametric Example
===================================

Before we get started make sure you've set your directory properly. You can check your directory with `getwd` and if you need to change it you can set it with `setwd`.

``` r
getwd()
setwd("D:/R/J-PAL Power Tutorial") 
```

Now let's download the data we'll be using for this tutorial. `download.file` will automatically download the required file from the specified URL.

``` r
download.file("https://www.povertyactionlab.org/sites/default/files/resources/Power-calculations-stata.zip", dest = "dataset.zip", mode = "wb")
```

The contents of `dataset.zip` will be extracted into working directory. Then we can unzip the data-set in the same folder.

``` r
unzip("dataset.zip", exdir = getwd())
```

Let's load the Balsakhi data-set. We'll use this data-set to estimate the control mean.

``` r
balsakhi <- read_dta("Power Calculations_2016/baroda_0102_1obs.dta")
```

Let us view the data that we have just loaded

``` r
View(balsakhi)
```

*For all parametric power calculations, we'll assume a conventional 95% confidence interval and 80% power.*

What do we expect the mean and standard deviation of the outcome to be in the control group?

``` r
control_mean <- mean(subset(balsakhi$post_totnorm, balsakhi$bal == 0), na.rm = TRUE)
control_sd <- sd(subset(balsakhi$post_totnorm, balsakhi$bal == 0), na.rm = TRUE)
```

Before proceeding lets check the values of `control_sd` and `control_mean`

``` r
control_sd
```

    ## [1] 1.15142

``` r
control_mean
```

    ## [1] 0.4288781

**Note: Since power calculations are usually done prior to a study, we often use baseline/pilot data on the study population, or government statistics for a comparable population, to get an approximate for this outcome in the control group.**

Let's say, based on other studies, that we expect an effect size of a tenth of a standard deviation. Now let's calculate the sample size given that we know the likely effect size.

``` r
expected_effect <- control_sd / 10
treated_mean <- expected_effect + control_mean
```

`treat` in the STATA exercise is equivalent to `treated_mean` in this R exercise. `control` is the STATA exercise is equivalent to `control_mean` in this R exercise.

Let us check the variables that we have

``` r
treated_mean
```

    ## [1] 0.5440201

``` r
control_mean
```

    ## [1] 0.4288781

``` r
expected_effect
```

    ## [1] 0.115142

``` r
control_sd
```

    ## [1] 1.15142

We can take a look at `sample_calculation.R` now to understand the function `sampsi.mean`. The function calculates the sample sizes for two-sample test based on means and standard deviations of the two samples.

``` r
sampsi.mean <- function (muA, muB, kappa = 1, sd, alpha = 0.05, beta = 0.20) {
  nB <- (1 + 1 / kappa) * (sd * (qnorm(1 - alpha / 2) + qnorm(1 - beta)) / (muA - muB)) ^ 2
  nA <- kappa * nB
  
  # The following steps are to align the output as the sampsi command shows in STATA
  
  variables <- c("Alpha Two sided", "Power", "mA", "mB","sd1", "sd2", "nA", "nB")
  values <- list(alpha, (1 - beta), muA, muB, sd, sd, ceiling(nA), ceiling(nB)) 
  ret <- setNames(values, variables)
  return(ret)
}
```

Now, we can calculate the sample sizes.

``` r
source("D:/R/J-PAL Power Tutorial/sample_calculation.R")
sampsi = sampsi.mean(control_mean, treated_mean, sd = control_sd)
sampsi
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 1.15142
    ## 
    ## $sd2
    ## [1] 1.15142
    ## 
    ## $nA
    ## [1] 1570
    ## 
    ## $nB
    ## [1] 1570

Say, instead, we knew the sample size and wanted to calculate the Minimum Detectable Effect Size (MDE). You can manually calculate the effect size

``` r
sample_n <- sampsi$nA + sampsi$nB
mde <- (0.842 + 1.96) * sqrt(1 / 0.25) * sqrt(1 / sample_n) * control_sd
mde
```

    ## [1] 0.1151508

The effect size should be .1151. Verify you get this result before proceeding. Let's compare this to the likely effect size that went into the first sample size calculation:

``` r
expected_effect
```

    ## [1] 0.115142

As this shows, it doesn't matter which we start with - sample size or effect size.

### Some other questions to answer before calculating power:

-   Will this study be cluster-randomized?

-   Will our main specification include controls (i.e. lagged dependent variables)?

-   Do we expect only part of the treatment group to take-up the intervention? If so, are we interested in estimating the local average treatment effect?

We'll address each of these questions one at a time to see how they affect power.

Example 2. Building Intuition
=============================

Now, let us get a better intuition on how a larger or smaller sample size affects our power to pick up an effect.

Say our anticipated effect size is smaller than originally thought; how much larger would we need to make the sample in order to still pick up an effect?

Let's try an effect size that is half as large.

``` r
smaller_expected_effect <- expected_effect / 2
smaller_treated_mean <- smaller_expected_effect + control_mean
sampsi.mean(control_mean, smaller_treated_mean, sd = control_sd)
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.4864491
    ## 
    ## $sd1
    ## [1] 1.15142
    ## 
    ## $sd2
    ## [1] 1.15142
    ## 
    ## $nA
    ## [1] 6280
    ## 
    ## $nB
    ## [1] 6280

*Observation: The minimum sample required is four times as large.*

Let's try an effect size that is a third of the original.

``` r
smaller_expected_effect <- expected_effect / 3
smaller_treated_mean <- smaller_expected_effect + control_mean
sampsi.mean(control_mean, smaller_treated_mean, sd = control_sd)
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.4672588
    ## 
    ## $sd1
    ## [1] 1.15142
    ## 
    ## $sd2
    ## [1] 1.15142
    ## 
    ## $nA
    ## [1] 14128
    ## 
    ## $nB
    ## [1] 14128

*Observation: The minimum sample required is now nine times as large.*

**Remember: If our MDE decreases by a factor of X, the required sample size increases by the square of X!**

You can verify this from the other side i.e. look at the impact on the MDE of increasing your sample size by a factor of X. Say X is 4:

``` r
new_sample <- 4 * (3142) # Global n of the STATA exercise
new_mde <- (0.842 + 1.96) * sqrt(1 / 0.25) * sqrt(1 / new_sample) * control_sd
new_mde / mde
```

    ## [1] 0.4998408

Example 3. Parametric Power Calculation with Controls
=====================================================

Now, say we plan to control for baseline covariates in our main specification. The inclusion of these controls will improve our power, since they explain some of the variance in our outcome. For example, including prior test scores on the right-hand side as controls when our left-hand side variable is test scores during the study period, can improve power if prior test scores predict future test scores. Economists will often control for a lagged dependent variable (recorded prior to randomization) if it makes a first order improvement in power.

To see how potential controls affect power, we would ideally have access to a sample data set (e.g. historical or pilot data). With these data, we would want to regress Y\_i (the outcome) on X\_i (the controls) to evaluate how much variance is explained by the set of covariates we plan to include. With access to historical data, for example, this would involve regressing last year's test scores (Y\_i = Y\_t-1) on test scores from the year before (X\_i = Y\_t-2).

From this regression, we are interested in the residual standard deviation of the outcome variables, or the variance of the outcome that is NOT explained by controls. This residual SD becomes the new SD we include in our parametric power calculations.

Part 1: We have pilot/historical data.
--------------------------------------

Using Balsakhi data, this would be:

``` r
fit <- lm(post_totnorm ~ pre_totnorm, data = balsakhi, subset = bal == 0)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = post_totnorm ~ pre_totnorm, data = balsakhi, subset = bal == 
    ##     0)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6007 -0.5264 -0.0086  0.5312  3.1473 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.40028    0.01247   32.10   <2e-16 ***
    ## pre_totnorm  0.80506    0.01243   64.79   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.821 on 4340 degrees of freedom
    ##   (866 observations deleted due to missingness)
    ## Multiple R-squared:  0.4916, Adjusted R-squared:  0.4915 
    ## F-statistic:  4197 on 1 and 4340 DF,  p-value: < 2.2e-16

``` r
balsakhi$y_predicted <- fit$coefficients['pre_totnorm'] * balsakhi$pre_totnorm +
  fit$coefficients['(Intercept)']

balsakhi$residual_predicted <- balsakhi$post_totnorm - balsakhi$y_predicted
res_control_sd <- sd(balsakhi$residual_predicted, na.rm = TRUE)
res_control_sd
```

    ## [1] 0.8254881

An alternate longer way to do the same is

``` r
control_subset <- subset(balsakhi, bal == 0)
fit <- lm(post_totnorm ~ pre_totnorm, data = control_subset)
```

If we knew the effect size and wanted to know the sample size needed..

``` r
sampsi.mean(control_mean, treated_mean, sd = signif(res_control_sd, digits = 6))
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 0.825488
    ## 
    ## $sd2
    ## [1] 0.825488
    ## 
    ## $nA
    ## [1] 807
    ## 
    ## $nB
    ## [1] 807

### Questions:

1.  What percent of the variance of study period test scores is explained by test scores at baseline? (Hint: Look at the R^2 statistic of the regression.)

2.  How does this affect our sample size (compared to not including controls)?

3.  How about our MDE?

Part 2: We do not have pilot/historical data for our study population, but plan to include controls.
----------------------------------------------------------------------------------------------------

We must first guess the percentage of variance in outcome variables that we expect to be explained by controls. (This can be done by looking at other data-sets with similar outcomes, etc.)

Say our controls explain 49% of the outcome. We can then calculate the residual standard deviation as follows.

``` r
expected_res_control_sd <- sqrt(.51 * control_sd ^ 2)
sampsi.mean(control_mean, treated_mean, sd = expected_res_control_sd)
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 0.8222782
    ## 
    ## $sd2
    ## [1] 0.8222782
    ## 
    ## $nA
    ## [1] 801
    ## 
    ## $nB
    ## [1] 801

We can also trace out an MDE "range" using various assumptions for how much of the variance of the outcome is explained by covariates:

``` r
for (proportion in seq(.5, .9, .1)) {
  expected_res_control_sd <- sqrt(proportion * control_sd ^ 2)
  cat("\nThe assumed proportion of variance explained by covariates = ",
    proportion, "\n")
  print(sampsi.mean(control_mean, treated_mean, sd = expected_res_control_sd))
}
```

    ## 
    ## The assumed proportion of variance explained by covariates =  0.5 
    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 0.8141767
    ## 
    ## $sd2
    ## [1] 0.8141767
    ## 
    ## $nA
    ## [1] 785
    ## 
    ## $nB
    ## [1] 785
    ## 
    ## 
    ## The assumed proportion of variance explained by covariates =  0.6 
    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 0.8918859
    ## 
    ## $sd2
    ## [1] 0.8918859
    ## 
    ## $nA
    ## [1] 942
    ## 
    ## $nB
    ## [1] 942
    ## 
    ## 
    ## The assumed proportion of variance explained by covariates =  0.7 
    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 0.9633469
    ## 
    ## $sd2
    ## [1] 0.9633469
    ## 
    ## $nA
    ## [1] 1099
    ## 
    ## $nB
    ## [1] 1099
    ## 
    ## 
    ## The assumed proportion of variance explained by covariates =  0.8 
    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 1.029861
    ## 
    ## $sd2
    ## [1] 1.029861
    ## 
    ## $nA
    ## [1] 1256
    ## 
    ## $nB
    ## [1] 1256
    ## 
    ## 
    ## The assumed proportion of variance explained by covariates =  0.9 
    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.5440201
    ## 
    ## $sd1
    ## [1] 1.092333
    ## 
    ## $sd2
    ## [1] 1.092333
    ## 
    ## $nA
    ## [1] 1413
    ## 
    ## $nB
    ## [1] 1413

*Please note that the code snippet above is the equivalent of the\``controlling_for_covariates.do` file in STATA. There is no separate R script.*

Example 4. Parametric Power Calculation for Cluster-RCTs
========================================================

Many designs randomize at the group level instead of at the individual level. For such designs, we need to adjust our power calculations so that they incorporate the fact that individuals within the same group may be subject to similar shocks, and thereby have correlated outcomes. Duflo et al. presents a modified parametric approach, which takes into account the intra-cluster correlation (ICC) that arises from randomization at the group level.

We can think of cluster-RCTs as follows:

-   When ICC = 0, then our N is effectively the number of individuals in the study.

-   When ICC = 1, then our N is effectively just the number of clusters.

-   Usually the ICC lies somewhere between 0 and 1, requiring that we adjust our power calculations to account for this.

Below we adjust R's power estimates based on Duflo et al.'s model.

Note: This model assumes that all clusters are of the same size and have the same number of individuals. It's usually okay if this is violated in reality, but you would not want to use these adjustments if groups are dramatically different in size (e.g. group one has 10 individuals, group two has 1,000 individuals.) More on this model is explained in Duflo et al.'s article ["Using Randomization in Development Economics Research: A Toolkit."](http://economics.mit.edu/files/806)

Part 1: Calculating MDE
-----------------------

First, let's calculate the intra-cluster correlation (ICC) which measures how correlated the error terms of individuals in the same cluster are.

``` r
control_subset <- subset(balsakhi, bal == 0 & !is.na(divid) & !is.na(post_totnorm))
control_subset$divid = as.factor(control_subset$divid)
icc <- ICCest(divid, post_totnorm, data = control_subset)
rho <- icc$ICC
```

Now, let's specify the number of individuals in each cluster...

``` r
m <- 53
```

and the number of clusters (as documented in the Balsakhi experiment.

``` r
j <- 193
```

Let us assume 95% confidence intervals and 80% power...

``` r
t_stat <- 1.96 + .842 
```

and 50% of the study population is assigned to treatment and 50% to control.

``` r
P <- .5
```

Now, we have Duflo et al.'s power adjustment:

``` r
mde <- t_stat * sqrt(1 / (P * (1 - P) * j)) * sqrt(rho + (1 - rho) / m) * control_sd
mde_ldv <- t_stat * sqrt(1 / (P * (1 - P) * j)) * sqrt(rho + (1 - rho) / m) * res_control_sd
```

And lastly, the total N of our study and the number who are treated, respectively.

``` r
n <- j * m
treated <- n * P

mdes <- c(.05, 0.8, rho, j, m, n, treated, control_mean, control_sd, mde, .05, 0.8, rho, j, m, n,   treated, control_mean, control_sd, mde_ldv)
table_names <- list(c("No_controls", "Control_LDV"), c("Signif", "Power", "ICC",  "Clusters", "Cluster_size", "N", "Treated", "Cntrl_mn", "Cntrl_SD", "MDE"))
mde_table <- matrix(data = mdes, nrow = 2, ncol = 10, byrow = 2, dimnames = table_names)
mde_table
```

    ##             Signif Power       ICC Clusters Cluster_size     N Treated
    ## No_controls   0.05   0.8 0.1554845      193           53 10229  5114.5
    ## Control_LDV   0.05   0.8 0.1554845      193           53 10229  5114.5
    ##              Cntrl_mn Cntrl_SD       MDE
    ## No_controls 0.4288781  1.15142 0.1923013
    ## Control_LDV 0.4288781  1.15142 0.1378667

### Part 2: Calculating Sample Size

In STATA,sampclus command can be used in combination with the sampsi command used above to incorporate clustering into our sample size calculations. A more parsimonious alternative is the clustersampsi command. However, note that clustersampsi rounds the group means to the tenth place after the decimal, making for a less precise calculation.

For R, please refer to the `sample_calculation.R` script for the emulation of the function `sampclus` that adjust for the number of individuals per cluster.

``` r
samp.clus <- function (nA, nB, rho, obs_clus) {
  nA <- ceiling(nA)
  nB <- ceiling(nB)
  rho <- signif(rho, digits= 8)
  deff <- 1+(obs_clus-1)*rho
  nA.clus <- as.numeric(nA)*signif(deff, digits = 8)
  nB.clus <- as.numeric(nB)*signif(deff, digits = 8)
  num.clus <- (nA.clus+nB.clus)/obs_clus
  num.clus <- ceiling(num.clus)
  
  # The following steps are to align the output as the sampclus command shows in STATA
  return_list_val <- c(ceiling(nA), ceiling(nB), rho, obs_clus,
    ceiling(nA.clus), ceiling(nB.clus), num.clus)
  return_list_name <- c("n1 uncorrected", "n2 uncorrected", "interclass correlation", "Average obs. per cluster", "n1 corrected", "n2 corrected", "Minimum number of clusters")
  return_list <- as.data.frame(cbind(return_list_name, return_list_val))
  return(return_list)
}
```

Let us calculate the sample size

``` r
smaller_treated_mean2 <- control_mean + mde
sampsi = sampsi.mean(control_mean, smaller_treated_mean2, sd = control_sd)
samp.clus(sampsi$nA, sampsi$nB, rho, m)
```

    ##             return_list_name return_list_val
    ## 1             n1 uncorrected             563
    ## 2             n2 uncorrected             563
    ## 3     interclass correlation      0.15548448
    ## 4   Average obs. per cluster              53
    ## 5               n1 corrected            5115
    ## 6               n2 corrected            5115
    ## 7 Minimum number of clusters             194

If we include the lagged dependent variable as a control we have

``` r
treated_mean_ldv <- control_mean + mde_ldv
sampsi = sampsi.mean(control_mean, treated_mean_ldv, sd = res_control_sd)
samp.clus(sampsi$nA, sampsi$nB, rho, m)
```

    ##             return_list_name return_list_val
    ## 1             n1 uncorrected             563
    ## 2             n2 uncorrected             563
    ## 3     interclass correlation      0.15548448
    ## 4   Average obs. per cluster              53
    ## 5               n1 corrected            5115
    ## 6               n2 corrected            5115
    ## 7 Minimum number of clusters             194

*Note that again it doesn't matter if we start with the MDE, or with the sample size.*

Questions:
==========

1.  Why do we have to adjust power for clustering when running a cluster-RCT?

2.  Assuming ICC &gt; 0, does adding a new cluster of 5 individuals or adding 5 individuals to already-existing clusters give us more power to detect effects?

Example 5. Parametric Power Calculation with Partial Take-up
============================================================

In randomized designs, it is common that there is partial take-up of the intervention. For example, in the [Oregon Health Insurance Experiment](http://nber.org/oregon/), the offer to apply for health insurance was associated with only a 25 percentage point increase in take-up of health insurance. When take-up is not 100 percent, researchers are often interested in the answers to second stage questions, such as what the average effect of becoming insured is on health care utilization.

Below, we provide code that adjusts R's power to take into account that only some individuals in the treatment group take up the intervention.

The measure of take-up that we care about is "effective take-up", or the percentage of individuals in the treatment group that takes up the intervention MINUS the percentage of individuals in the control group that takes up.

Let us say 90% take up in the treatment group, and 10% do so in the control group. We then have an effective take-up rate of 80%:

``` r
eff_tu <- .9 - .1
```

Now we calculate the adjusted MDE by multiplying the unadjusted effect size times the effective take-up rate

``` r
adjusted_mde <- ((0.842 + 1.96) * sqrt(1 / 0.25) * sqrt(1 / n) * control_sd) * eff_tu
adjusted_mde
```

    ## [1] 0.05103936

If we want to estimate sample size, we follow a similar procedure:

``` r
treated_mean_adjusted <- control_mean + adjusted_mde
sampsi.mean(control_mean, treated_mean_adjusted, sd = control_sd)
```

    ## $`Alpha Two sided`
    ## [1] 0.05
    ## 
    ## $Power
    ## [1] 0.8
    ## 
    ## $mA
    ## [1] 0.4288781
    ## 
    ## $mB
    ## [1] 0.4799175
    ## 
    ## $sd1
    ## [1] 1.15142
    ## 
    ## $sd2
    ## [1] 1.15142
    ## 
    ## $nA
    ## [1] 7990
    ## 
    ## $nB
    ## [1] 7990

**Note that unlike with the MDE, partial take-up affects sample size quadratically; we divide estimates of sample size by the square of the effective take-up rate!**

**Note: For more on partial take-up and how it affects estimation of the power to detect local average treatment effects, please see the aforementioned article by Duflo et al.**

Example 6. Non-parametric Power Simulations
===========================================

Non-parametric power simulations do better than parametric power calculations when we have access to good data (historical, baseline, or pilot) on our study population. From these data, we can simulate a fake dataset that assumes the treatment has no effect and then see what effects we are powered to detect, by looking at the simulated 95% confidence interval around our null effect. We should expect that any effect greater than this confidence interval would be detected by our study.

In particular, power simulations do not require the assumption that the sampling distribution of your Beta coefficient(s) of interest takes a normal distribution in your (finite) sample. You may be particularly worried about this assumption (of parametric power calculations) if your sample is very small.

To do non-parametric power simulations we need to create a (reasonable) "fake" or simulated dataset. For example, if you have baseline data for the 3 months prior to a 12 month trial, then a reasonable way to expand this dataset would be to simply randomly draw days with replacement until you have 365 days in your dataset. Similarly, you could use historical data from the two years prior to the study to estimate the confidence interval around a null effect in the past year, with data on your outcome variable from two years ago serving as controls.

Let's do an example using Balsakhi data to make this procedure more clear.

Step 1. Upload pre-period data
------------------------------

For this example, we expand study data for the control group of the Balsakhi experiment by 2, since this is similar to what would be available from historical data.

``` r
control_subset <- subset(balsakhi, bal == 0)
simulated = control_subset 
simulated$studentid <- (-1) * simulated$studentid
simulated <- rbind.data.frame(control_subset, simulated)

keep_cols <- c("studentid", "pre_tot", "mid_tot", "post_tot", "divid",
  "pre_totnorm", "mid_totnorm", "post_totnorm")
simulated <- simulated[, keep_cols, drop = FALSE]
simulated <- simulated[order(simulated$studentid), ]

nrow(simulated)
```

    ## [1] 10416

``` r
summary.data.frame(simulated)
```

    ##    studentid            pre_tot         mid_tot          post_tot    
    ##  Min.   :-64041343   Min.   : 0.00   Min.   :  0.00   Min.   : 0.00  
    ##  1st Qu.:-12041566   1st Qu.:14.00   1st Qu.: 24.00   1st Qu.:20.00  
    ##  Median :        0   Median :29.00   Median : 43.00   Median :38.00  
    ##  Mean   :        0   Mean   :32.13   Mean   : 44.46   Mean   :40.88  
    ##  3rd Qu.: 12041566   3rd Qu.:48.00   3rd Qu.: 65.00   3rd Qu.:61.00  
    ##  Max.   : 64041343   Max.   :94.00   Max.   :234.00   Max.   :98.00  
    ##                                      NA's   :1264     NA's   :1732   
    ##      divid        pre_totnorm       mid_totnorm       post_totnorm    
    ##  Min.   :31130   Min.   :-1.9114   Min.   :-1.9114   Min.   :-1.9114  
    ##  1st Qu.:33440   1st Qu.:-0.7920   1st Qu.:-0.3418   1st Qu.:-0.4886  
    ##  Median :41070   Median :-0.1851   Median : 0.5230   Median : 0.3207  
    ##  Mean   :38704   Mean   : 0.0000   Mean   : 0.5970   Mean   : 0.4289  
    ##  3rd Qu.:43250   3rd Qu.: 0.7045   3rd Qu.: 1.5131   3rd Qu.: 1.3228  
    ##  Max.   :46400   Max.   : 3.4059   Max.   : 9.2180   Max.   : 3.7094  
    ##                                    NA's   :1264      NA's   :1732

``` r
write.csv(simulated, "bal_power_data_r.csv", row.names = FALSE, quote = FALSE)
```

Step 2. Write randomization code as you plan to randomize
---------------------------------------------------------

Step 3. Write simulation code.
------------------------------

Your randomization code should take into account how you plan to randomize, including your plan to stratify, etc.

This power simulation program does the following:

1.  specifies main regression equation,

2.  simulates treatment assignment and runs main regressions 1000 times,

3.  summarizes the results from these regressions into an easy-to-read matrix.(left as an exercise)

To reduce the run-time, we have set this program to run over 1 iterations, but if you are truly calculating power via simulation, you should set this at 1000.

``` r
alpha <- 0.05                                    # Standard significance level
sims <- 1                                        # Number of simulations to conduct
```

Initialize a matrix to collect results. The matrix can be filed up by users according to their choice. and has been left as an exercise. For hint see the end of the file.

``` r
colnames <- c("pvalue" , "tstat", "control_mean", "control_sd", "beta", "se", "ci_low", "ci_high")
results <- matrix(nrow = sims, ncol = 8)
colnames(results) <- colnames
```

Part 1. Basic MDE (clustering on divid because data is from C-RCT)
------------------------------------------------------------------

Loop to conduct experiments "sims" times over. Output is shown for sims = 1.

``` r
for (i in 1:sims){
  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clust_var = simulated$divid)
  # View(simulated)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ treatment, data = simulated, na.action = na.exclude)      
  # call for clustered SE  by divid
  source('D:/R/J-PAL Power Tutorial/Clustered_SE.R')
  output <- cluster_summary(fit, simulated$divid)
  print(output)
}
```

    ## Loading required package: multiwayvcov

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## [[1]]
    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.511722   0.064676  7.9121 2.841e-15 ***
    ## treatment   -0.171863   0.104301 -1.6478   0.09944 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## [[2]]
    ## Wald test
    ## 
    ## Model 1: post_totnorm ~ treatment
    ## Model 2: post_totnorm ~ 1
    ##   Res.Df Df      F  Pr(>F)  
    ## 1   8682                    
    ## 2   8683 -1 2.7151 0.09944 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [[3]]
    ##               Estimate    LowerCI    UpperCI
    ## (Intercept)  0.5117222  0.3849423 0.63850209
    ## treatment   -0.1718629 -0.3763184 0.03259252

Part 2. Controlling for LDV
---------------------------

Loop to conduct experiments "sims" times over. Output is shown for sims = 1.

``` r
for (i in 1:sims) {
  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clust_var = simulated$divid)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ pre_totnorm + treatment, data = simulated, na.action
    = na.exclude)      
  # call for clsutered SE  by divid

  output <- cluster_summary(fit, simulated$divid)
  print(output)
}
```

    ## [[1]]
    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.464185   0.050130  9.2596  < 2e-16 ***
    ## pre_totnorm  0.804276   0.025286 31.8069  < 2e-16 ***
    ## treatment   -0.125113   0.073491 -1.7024  0.08871 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## [[2]]
    ## Wald test
    ## 
    ## Model 1: post_totnorm ~ pre_totnorm + treatment
    ## Model 2: post_totnorm ~ 1
    ##   Res.Df Df      F    Pr(>F)    
    ## 1   8681                        
    ## 2   8683 -2 551.47 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## [[3]]
    ##               Estimate    LowerCI    UpperCI
    ## (Intercept)  0.4641849  0.3659176 0.56245225
    ## pre_totnorm  0.8042755  0.7547086 0.85384242
    ## treatment   -0.1251129 -0.2691718 0.01894611

Since the Balsakhi dataset has many observations, we find that our simulated power estimates of MDE are very similar to those found through parametric estimates. To see this, compare Example 4 (parts 1 & 2) to Example 6 (parts 1 & 2). As you can see, it is much easier to cluster or control for covariates using this method; you simply run the regression specification that you intend to use at the analysis stage.

Similarly, if you wanted to estimate the (local) average treatment effect in the presence of partial take-up of the intervention, then you can simply make basic assumptions on the effective take-up rate, and run your two-stage least squares regression.

Generally speaking, parametric power calculations are great for back-of-the-envelope calculations of power, while non-parametric simulations provide more precise estimates of power when you have good baseline data.

### Questions:

1.  How is "non-parametric" power different from "parametric" power?

2.  How do our parametric and non-parametric estimates of power compare? (Hint: Compare Example 4 to Example 6.)

3.  When would you want to run parametric power calculations?

4.  Non- paramteric simulations

Answers
=======

Ex. 3, part 1:

1.  49%

2.  Including controls cuts sample size almost in half.

3.  Reduces MDE to a lesser extent (~28%).

Ex. 4, part 2:

1.  Assignment is only random at the cluster level; thus we must cluster our standard errors in our main specification. To this end, our power calculations must also take this into account, since, by clustering in our main specification, we will lose all precision gained from intra-cluster correlation in outcomes.

2.  Adding a new cluster of 5 individuals, since in the limiting case of ICC = 1, adding 5 individuals to previous clusters would have no effect on power, while adding 5 individuals in a new cluster would increase our effective N by 1.

Ex. 6:

1.  Non-parametric power simulations do not require the assumption that beta coefficients take a normal distribution (which follows from the central limit theorem), while parametric power calculations do.

2.  Our power simulations in Example 6 find that we are powered to detect an ~.20 SD increase in test scores without controls and a ~.14 SD increase in test scores when including a lagged dependent variable. Using parametric methods, Example 4 illustrates these are .19 SD and .13 SD, respectively. Thus, we find very similar results using each method.

3.  We would want to run parametric power calculations when we want a quick, rough estimate of power, or when we do not have access to high quality baseline data.

4.  We would want to run non-parametric simulations when we have access to high-quality baseline data and have reason to believe parametric assumptions may be violated. For example, we would prefer to run simulations if our randomization is complex (i.e. multiple randomizations), have a small sample size, or are running complex specifications.

### Hint for aggregating results of regression simluation

Example is shown to tabulate the results of coeftest. Others can be done similarly

``` r
# Define a function that will extract coefficients during the simulation
extract_estimates <- function(output) {
  class(output) <- 'matrix'
  df_out <- data.frame(output)
  df_out$coefficient <- rownames(df_out)
  rownames(df_out) <- NULL
  return(df_out)
}

# simulation

# Standard significance level
alpha <- 0.05
# Number of simulations to conduct
sims <- 5

coefs <- data.frame()
for (i in 1:sims) {

  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clust_var = simulated$divid)
  # View(simulated)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ treatment, data = simulated, na.action = na.exclude)      
  # call for clsutered SE  by divid
  sim_output <- cluster_summary(fit, simulated$divid)
  sim_coef <- extract_estimates(sim_output[[1]])
  sim_coef$sim = i
  coefs <- rbind.data.frame(coefs, sim_coef)
}
coefs
```

    ##       Estimate Std..Error    t.value     Pr...t.. coefficient sim
    ## 1   0.50978968 0.07491582  6.8048335 1.078698e-11 (Intercept)   1
    ## 2  -0.16310033 0.10281407 -1.5863619 1.126936e-01   treatment   1
    ## 3   0.46085838 0.07827225  5.8878897 4.056924e-09 (Intercept)   2
    ## 4  -0.07131914 0.10297209 -0.6926065 4.885750e-01   treatment   2
    ## 5   0.47313164 0.07405641  6.3888009 1.757891e-10 (Intercept)   3
    ## 6  -0.08460981 0.10477610 -0.8075296 4.193835e-01   treatment   3
    ## 7   0.43508432 0.08026809  5.4203895 6.106432e-08 (Intercept)   4
    ## 8  -0.01357552 0.10351314 -0.1311477 8.956615e-01   treatment   4
    ## 9   0.44346683 0.07233847  6.1304421 9.145738e-10 (Intercept)   5
    ## 10 -0.02822827 0.10553748 -0.2674716 7.891125e-01   treatment   5

Similar data frames for aggregation can be made for other objects returned by super.cluster.fun, namely w(wald test) and ci(confidence intervals). Instead of using a loop, the same functionality can be achieved through apply family of functions.
