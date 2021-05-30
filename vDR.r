# Rich Jones (rnjones@brown.edu)
# 2021 May 30
# ------------------------------------------------
# How large is that correlation?
# verbal descriptor on a correlation coefficient
# -----------------------------------------------
vDR <- function(r){
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
