# lehr_repeated.r
# Rich Jones
# 2019-09-26
# lehr_repeated is a function that computes the minimum 
# detectable effect size in Cohen's d metric assuming
# a two-group design with equal sample sizes (n) and
# a repeated measures design (m repeated measures) and
# a auto-correlation of the outcome (r) under the
# assumption that 80% power and a type-I error level are
# desired.
lehr_repeated <- function(n,m,r) {
  # n per group sample size
  # m number of repeated observations (including the first)
  # r the time-to-time correlation of the outcome
  # n = (16/d^2)*((1+(m-1)*r)/m)
  # d = (4*sqrt((m-1)*r+1))/(sqrt(m)*sqrt(n)) 
  (4*sqrt((m-1)*r+1))/(sqrt(m)*sqrt(n)) 
}
# Check/example
# > lehr_repeated(64,1,0)
# [1] 0.5
# the minimum detectable effect size with 64 per group
# with 1 observation and n=64 per group is Cohen's d of 0.5
# (That is correct). It does not matter what the value
# is for the correlation in this example
#
# Example
# > lehr_repeated(40,4,.7)
# [1] 0.5567764
# The minimum effect size (in Cohen's d metric) across gorups
# with 4 repeated observations, an autocorrelation of .7 and
# and a common group sample size of 40 is d=0.56

