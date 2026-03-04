#' Effective Sample Size for Clustered Data
#'
#' Calculates the design effect and effective sample size for clustered data
#' (e.g., repeated measurements on subjects).
#'
#' @param m Number of observations per cluster
#' @param k Number of clusters
#' @param r Intra-cluster correlation coefficient (ICC)
#'
#' @return A vector with two elements: design effect and effective sample size
#'
#' @details
#' Design effect = (m-1)*r + 1
#' When dividing total sample size by the design effect, you get the effective
#' sample size that would be needed for simple random sampling to achieve the
#' same power and precision as the clustered design.
#'
#' References: van Breukelen & Candel (2012). Journal of Clinical Epidemiology.
#'
#' @examples
#' ess(25, 4, 0.017)  # 25 obs per cluster, 4 clusters, ICC = 0.017
#'
#' @export
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


