RNJmisc
=======

Rich Jones' miscellaneous stuff

# ess.R 
Determine the effective sample size given a clustered design. 
Uses intra-cluster correlation (ICC), number of units (e.g. patients)
and number of observations per unit (e.g., repeated observations). Also
the code shows how to find minimum detectable correlation given
effective sample size.

Note, however, that the ess calculation only addresses non-independence of
one variable for the correlation, so the minimum detectable correlation
is anti-conservative.





