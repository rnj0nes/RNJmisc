#' Two-Tailed P-value from z-statistic
#'
#' Calculates two-tailed p-value from a z-statistic.
#'
#' @param x The z-statistic value
#'
#' @return The two-tailed p-value
#'
#' @examples
#' ztesti(2.5)
#'
#' @export
ztesti <- function(x) {
  pv <- (1-pnorm(abs(x)))*2
  message(paste0("Two-tailed P-value with z = " , sprintf("%6.4f", x))," is :")
  pv
}

