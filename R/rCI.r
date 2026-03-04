#' Confidence Interval for Correlation Coefficient
#'
#' Computes confidence interval on a Pearson correlation using Fisher's z transformation.
#'
#' @param x Correlation coefficient (between -1 and 1)
#' @param n Sample size
#' @param alpha Significance level (default 0.05 for 95% CI)
#' @param digits Number of decimal places to round to (default 3)
#'
#' @return A vector with lower and upper CI limits for the correlation
#'
#' @details
#' Uses Fisher's z transformation for more accurate CIs, especially with small samples.
#'
#' @examples
#' rCI(0.9, n = 120)  # Returns approximately (0.859, 0.929)
#'
#' @export
rCI <- function(x, n, alpha = .05 , digits = 3) {

   ll <- tanh(atanh(x)+qnorm(alpha/2)*(1/sqrt(n-3)))
   ul <- tanh(atanh(x)+qnorm(1-alpha/2)*(1/sqrt(n-3)))
   ll <- round(ll, digits = digits)
   ul <- round(ul, digits = digits)
   ci <- c(ll,ul)
   return(ci)
}
# 
# > rCI(.9, n=120)
# [1] 0.859 0.929

