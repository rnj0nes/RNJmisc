#' Conversion Factor Between Logit and Probit Scales
#'
#' Finds the optimal conversion factor (D*) between logit and probit scaled
#' regression coefficients as the ratio of the normal and logistic density.
#'
#' @param x A proportion (between 0 and 1) for which to compute D*
#'
#' @return The D* conversion factor for the given proportion
#'
#' @details
#' The D* factor is used to convert between logit and probit regression coefficients.
#' It is determined by the marginal proportion of the grouping variable (e.g.,
#' proportion making an error). Common approximations use pi/sqrt(3), 1.7, or 1.6,
#' but this function provides the exact value.
#'
#' @examples
#' Dstar(0.5)  # Returns approximately 1.596
#' Dstar(0.05) # Returns approximately 2.171
#'
#' @export
Dstar <- function(x){

  # logit function
  # places a proportion (x) on log odds scale
  logit <- function(x){
      return(log((x)/(1-(x))))
   }

   # logistic probability density function
   # x is a proportion
   # mu is the mean, s is the scale
   logit.pdf=function(x,mu=0,s=1){
      k=((x)-mu)/s
      return(exp(-k)/(s*(1+exp(-k))^2))
   }

   return(dnorm(qnorm((x)))/logit.pdf(logit((x))))

}
# > Dstar(.5)
# [1] 1.595769
# > Dstar(.05)
# [1] 2.171277
# > Dstar(.1+.4)
# [1] 1.595769
# curve(expr = Dstar, from = .01, to = .99)

