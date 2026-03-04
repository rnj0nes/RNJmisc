#' Effective Sample Size (Harmonic Mean)
#'
#' Calculates the effective per-group sample size as the harmonic mean of two group sizes.
#'
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#'
#' @return The harmonic mean of n1 and n2
#'
#' @details
#' neff = 2 / (1/n1 + 1/n2)
#'
#' @examples
#' neff(20, 30)
#'
#' @export
neff <- function(n1,n2) {
    2/((1/n1)+(1/n2))
}

