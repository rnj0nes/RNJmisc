#' Cohen's d Effect Size with Confidence Interval
#'
#' Computes Cohen's d (Hedge's g) standardized mean difference with 95%
#' confidence interval using weighted pooled standard deviation.
#'
#' @param m1 Mean of group 1
#' @param sd1 Standard deviation of group 1
#' @param n1 Sample size of group 1
#' @param m2 Mean of group 2
#' @param sd2 Standard deviation of group 2
#' @param n2 Sample size of group 2
#'
#' @return A vector with d, lower CI limit, and upper CI limit
#'
#' @details
#' Uses weighted pooled SD to produce Hedge's g version of Cohen's d.
#' References: Hedge LV, Olkin I. Statistical methods for meta-analysis.
#'
#' @examples
#' esiD(0.7, 0.5, 317, 1.1, 0.8, 24)
#'
#' @export
esiD <- function(m1,sd1,n1,m2,sd2,n2) {
  # Uses weighted pooled SD, so produces Hedge's g version of Cohen's d
  # 95% Confidence interval from Hedge LV Oklin I. Statistical methods for 
  # meta-analysis. Orlando, Academic Press Inc., 2014 p86 per Dong Kyu Lee,
  # Alternatives to P value: confidence interval and effect size. Korean 
  # Journal of Anaesthesiology 2016,69(6):555-562
  pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))
  d <-(m1-m2)/pooled.sd
  vard <- sqrt(((n1+n2)/(n1*n2))+(d^2/(2*(n1+n2))))
  alpha <- .05
  d.ll <- d+qnorm(alpha/2)*vard
  d.ul <- d+qnorm(1-(alpha/2))*vard
  message(paste0("Cohen's d (Hedge's g), the standardized mean difference: ", sprintf("%0.2f", d)))
  message(paste0("d = ","(",m1," - ",m2,")/(sqrt((((",n1,"-1)*",sd1,"^2)+((",n2,"-1)*",sd2,"^2))/(",n1," + ",n2,"-2)))"))
  message(paste0("95% CI on d: ", sprintf("%0.2f", d.ll) ,"," , sprintf("%0.2f", d.ul) ))
  dobj <- c(d,d.ll,d.ul)
  dobj
}
#dis <- esiD(0.7,0.5,317,1.1,0.8,24)
#dis[1]
