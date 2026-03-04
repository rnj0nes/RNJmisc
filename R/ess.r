# Clustered samples and Effective Sample Size
#
# van Breukelen, G. J., & Candel, M. J. (2012). 
# Calculating sample sizes for cluster randomized trials:
# we can keep it simple and efficient! 
# Journal of Clinical Epidemiology, 65(11), 1212-1218. 
#
# The factor ((m-1)*r + 1) ... is known as the 
# design effect (DE). Dividing the total sample size 
# m*k by the DE gives the effective sample size, 
# that is, the sample size needed for individual 
# randomization to have the same power and precision
# as the cluster randomized trial. 

ess <- function(m,k,r) {
  # m is number of observations per cluster (i.e., 
  #    repeated observations on a patient)
  # k is the number of clusters (i.e., patients)
  # m*k is the total number of observations studied
  # r is the intra-cluster correlation coefficient
  design_effect <- ((m-1)*r+1)
  effective_sample_size <- (m*k)/design_effect
  c(design_effect, effective_sample_size)
}

# checks out confirmed from
# https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/clustered-data
# ess(25,4,.017)
# [1]  1.40800 71.02273

# Consider an EMA study where the goal is to
# find the correlation between two variables.
# What is the smallest correlation that can be
# detected?
#
# We know that the minimum R that can be detected
# with two-tailed alpha level of 5% and 80% power is
# minR <- tanh(sqrt(8/n)) where n is the sample size.
#
# We will use the effective sample size of the
# EMA study to figure out what the minimum detectable
# correlation is
#
# Assume 80 patients (k)
# with 21 repeated observations (m)
# and an ICC of 0.5
# result_ess <- ess(21,80,.5)
# result_ess
# minR <- tanh(sqrt(8/result_ess[2]))
# minR


