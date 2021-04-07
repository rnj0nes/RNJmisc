RNJmisc
=======

Rich Jones' miscellaneous stuff

# ci.corr.R
Function to compute confidence interval on correlation coefficient using Fisher's z transformation

```
ci.corr(.5, n=120)
ci.corr(.5, n=120, digits=2)
```

# Dstar.R
Get the optimal value for D to convert between logit and probit regression parameters. Solved as the ratio of heights of probability density functions under normal and logistic at a specific value for p (the probability of an indicated response).

```
Dstar(.3)
```


# ess.R 
Determine the effective sample size given a clustered design. 
Uses intra-cluster correlation (ICC), number of units (e.g. patients)
and number of observations per unit (e.g., repeated observations). Also
the code shows how to find minimum detectable correlation given
effective sample size.

Note, however, that the ess calculation only addresses non-independence of
one variable for the correlation, so the minimum detectable correlation
is anti-conservative.





