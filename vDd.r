# Rich Jones (rnjones@brown.edu)
# 2021 May 30
# ------------------------------------------------
# How large is that standardized mean difference?
# verbal descriptor on a Cohen's d
# -----------------------------------------------
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
