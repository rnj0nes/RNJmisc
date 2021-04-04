# Dstar function
# Rich Jones (Rich_Jones@Brown.edu) 2021-04-04
# --------------------------------------------------------------
# finds the optimal conversion factor between logit and probit
# scaled regression coefficients (D) as the ratio of the 
# normal density and logistic density corresponding to the 
# marginal value of the proportion with the indicated response
# (e.g., proportion making an error)
#
# logit function
# places a proportion (x) on log odds scale
logit=function(x){
   return(log((x)/(1-(x))))
}
# logistic probability density function
# x is a proportion
# mu is the mean, s is the scale
logit.pdf=function(x,mu=0,s=1){
   k=((x)-mu)/s
   return(exp(-k)/(s*(1+exp(-k))^2))
}
# Dstar is the conversion factor for moving between
# logit and probit parameters (sometimes people use
# pi/sqrt(3), sometimes 1.7, sometimes 1.6, here's an
# exact value determined by marginal proportion of
# the grouping variable (proportion with outcome,
# proportion with indicated response on test item)
Dstar=function(x){
  return(dnorm(qnorm((x)))/logit.pdf(logit((x))))
}
# > Dstar(.5)
# [1] 1.595769
# > Dstar(.05)
# [1] 2.171277
# > Dstar(.1+.4)
# [1] 1.595769
# curve(expr = Dstar, from = .01, to = .99)

