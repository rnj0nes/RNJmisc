#' Verbal Description of Cohen's d Effect Size
#'
#' Provides a qualitative descriptor for a given Cohen's d effect size.
#'
#' @param d Cohen's d effect size
#'
#' @return A character string describing the effect size magnitude
#'
#' @details
#' Converts numerical d values to verbal descriptors:
#' - d < .2: "trivial"
#' - .2 <= d < .25: "small"
#' - .25 <= d < .45: "small-to-medium"
#' - .45 <= d < .55: "about a medium"
#' - .55 <= d < .75: "medium-to-large"
#' - .75 <= d < .85: "about a large"
#' - d >= .85: "large"
#'
#' @examples
#' vDd(0.5)   # "about a medium"
#' vDd(0.8)   # "about a large"
#' vDd(1.0)   # "large"
#'
#' @export
vDd <- function(d){
  d.is <- abs(d)
  if (d.is<.2) { 
    descriptor <- "trivial" 
  }
  if (d.is>=.2&d.is<.25) { 
    descriptor <- "small"
  }
  if (d.is>=.25&d.is<.45) { 
    descriptor <- "small-to-medium"
  }
  if (d.is>=.45&d.is<.55) { 
    descriptor <- "about a medium"
  }
  if (d.is>=.55&d.is<.75) {
    descriptor <- "medium-to-large" 
  }
  if (d.is>=.75&d.is<.85) {
    descriptor <- "about a large" 
  }
  if (d.is>=.85) { 
    descriptor <- "large" 
  }
  descriptor
}
