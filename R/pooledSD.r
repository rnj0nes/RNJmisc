#' Pooled Standard Deviation
#'
#' Computes the pooled standard deviation for two independent groups.
#'
#' @param sd1 Standard deviation of group 1
#' @param sd2 Standard deviation of group 2
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#'
#' @return The pooled standard deviation
#'
#' @details
#' pooled_sd = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1 + n2 - 2))
#'
#' @examples
#' pooledSD(0.43, 0.442, 48, 144)
#'
#' @export
pooledSD <- function(sd1,sd2,n1,n2) {

   pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))
   pooled.sd <- as.double(pooled.sd)
   show.pooled.sd <- round(pooled.sd,digits=3)
   message(paste0("n1 <- ",n1))
   message(paste0("n2 <- ",n2))
   message(paste0("sd1 <- ",sd1))
   message(paste0("sd2 <- ",sd2))
   message(paste0("pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))"))
   message(paste0("# pooled.sd = sqrt((((",n1,"-1)*",sd1,"^2)+((",n2,"-1)*",sd2,"^2))/(",n1,"+",n2,"-","2))"))
   a <- round((n1-1)*sd1^2,digits = 3)
   b <- round((n2-1)*sd2^2,digits = 3)
   c <- round((n1+n2-2),digits= 3)
   message(paste0("# pooled.sd = sqrt((((",a,")+((",b,"))/(",c,"))"))
   message(paste0("# pooled.sd = ",show.pooled.sd))
   pooled.sd

}

# pooledSD(sd1=0.43,sd2=0.442,n1=48,n2=144)
