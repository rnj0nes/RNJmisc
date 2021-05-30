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
Get the optimal value for D to convert between logit and probit regression parameters. Solved as the 
ratio of heights of probability density functions under normal and logistic at a specific value 
for p (the probability of an indicated response). I suggest using the overall marginal value for p.
In the example below, p is .3. This could mean that overall 30% of the sample make an error on 
a test item, and one wants a value for $D$ to convert from probit to logit coefficients. 

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

# vDd.r
Provides a verbal descriptor on the size of a standardized mean difference effect
size. Loosely following
what Cohen (1988; Statistical power analysis for the behavioral sciences. Hillsdale, 
New Jersey: Lawrence Erlbaum Associates.) suggested.
|r|descriptor|r|descriptor|
|--|--|--|--|
|<0.2|trivial|[0.2,0.25)|small|
|[0.25,0.45)|small-to-medium|[0.45,0.55)|about a medium|
|[0.55,0.75)|medium-to-large|[0.75,0.85)|about a large|
|>=0.85|large|||

```
d <- .3
vDd(d)
```

# vDR.r
Provides a verbal descriptor on the size of a correlation coefficient. Loosely following
what Cohen (1988; Statistical power analysis for the behavioral sciences. Hillsdale, 
New Jersey: Lawrence Erlbaum Associates.) suggested.
|r|descriptor|r|descriptor|
|--|--|--|--|
|<.1|trivial|[.1,.12)|small|
|[.12,.28)|small-to-medium|[.28,.32)|about a medium|
|[.32,.48)|medium-to-large|[.48,52)|about a large|
|>=.52|large|||

```
r <- .3
vDR(r)
```





