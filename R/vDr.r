#' Verbal Description of Correlation Coefficient
#'
#' Provides a qualitative descriptor for a given correlation coefficient.
#'
#' @param r Pearson correlation coefficient (between -1 and 1)
#'
#' @return A character string describing the correlation magnitude
#'
#' @details
#' Converts |r| to verbal descriptors:
#' - |r| < .1: "trivial"
#' - .1 <= |r| < .12: "small"
#' - .12 <= |r| < .28: "small-to-medium"
#' - .28 <= |r| < .32: "about a medium"
#' - .32 <= |r| < .48: "medium-to-large"
#' - .48 <= |r| < .52: "about a large"
#' - |r| >= .52: "large"
#'
#' @examples
#' vDr(0.3)   # "about a medium"
#' vDr(0.5)   # "about a large"
#' vDr(0.7)   # "large"
#'
#' @export
vDr <- function(r){
  r.is <- abs(r)
  if (r.is<.1) { 
    descriptor <- "trivial" 
  }
  if (r.is>=.1&r.is<.12) { 
    descriptor <- "small"
  }
  if (r.is>=.12&r.is<.28) { 
    descriptor <- "small-to-medium"
  }
  if (r.is>=.28&r.is<.32) { 
    descriptor <- "about a medium"
  }
  if (r.is>=.32&r.is<.48) {
    descriptor <- "medium-to-large" 
  }
  if (r.is>=.48&r.is<.52) {
    descriptor <- "about a large" 
  }
  if (r.is>=.52) { 
    descriptor <- "large" 
  }
  descriptor
}
