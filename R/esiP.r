#' Effect Size (h) from Two Proportions
#'
#' Computes Cohen's h effect size from two proportions using
#' the arcsine transformation.
#'
#' @param p1 First proportion (between 0 and 1)
#' @param p2 Second proportion (between 0 and 1)
#'
#' @return The h effect size
#'
#' @details
#' Computes h = |2*arcsin(sqrt(p1)) - 2*arcsin(sqrt(p2))|
#'
#' @examples
#' esiP(0.6, 0.4)
#'
#' @export
esiP <- function(p1,p2) {

	h <- abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))

	h

}
