# rCI function
# Rich Jones (Rich_Jones@Brown.edu) 2021-04-04
# --------------------------------------------------------------
# shows confidence interval on a correlation using Fisher's z transformation

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

