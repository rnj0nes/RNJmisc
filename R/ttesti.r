#' Two-Tailed P-value from t-statistic
#'
#' Calculates two-tailed p-value from a t-statistic and degrees of freedom.
#'
#' @param x The t-statistic value
#' @param df Degrees of freedom
#'
#' @return The two-tailed p-value
#'
#' @examples
#' ttesti(2.5, 28)
#'
#' @export
ttesti <- function(x,df) {
  pv <- (1-pt(abs(x),df))*2
  message(paste0("Two-tailed P-value with t(",n,") = " , sprintf("%6.4f", x))," is :")
  pv
}
