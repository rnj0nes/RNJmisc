# ztesti# get a two-tailed p-value from a z valueztesti <- function(x) {  pv <- (1-pnorm(abs(x)))*2  message(paste0("Two-tailed P-value with z = " , sprintf("%6.4f", x))," is :")  pv}

