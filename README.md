RNJmisc
=======

Miscellaneous R utilities developed by Rich Jones. This collection includes tools for effect size estimation, descriptive statistics, psychometric scoring, and general utility functions.

---

### `ci.corr()`
Computes the confidence interval for a correlation coefficient using Fisher's z-transformation.

### `Dstar()`
Computes the optimal scaling factor `D` for converting probit regression coefficients to logit equivalents, based on a specified probability.

### `es.neteffect()`
Estimates the net effect of a treatment in the presence of confounding, using potential outcomes logic.

### `esi.descriptor()`
Returns a text description (e.g., "small", "medium") based on Cohen's d effect size magnitude.

### `esi.p()`
Computes the p-value for a given effect size and standard error.

### `esi.se()`
Computes the standard error for a given effect size estimate.

### `ess()`
Calculates effective sample size given ICC, number of clusters, and size per cluster.

### `extractEigenvalues()`
Extracts and returns eigenvalues from a matrix, with optional trimming of small values.

### `hist_discrete()`
Generates histograms for discrete (integer-valued) variables, with cleaner axis formatting.

### `include()`
Source and r file and replays log to console

### `itemrest()`
Computes item-rest correlations across variables in a data frame.

### `itemsummary()`
Frequency table of item resopnses 

### `Lehr_repeated()`
Computes sample size requirements for repeated-measures designs using Lehr’s formula.

### `neff()`
Computes effective sample size using design-based correction for survey weights.

### `pooled.sd()`
Computes pooled standard deviation across two groups.

### `pvf()`
Formats P-values

### `scoreit()`
Scores a set of items using sum or predictive mean matching; reports alpha and item-rest correlations.

### `showme()`
Replays a text file to the console (like Mplus output, for example)

### `showme.section()`  
Replays sections of a text file to the console (like Mplus output, for example)

### `showme.section.lines()`  
Replays sections of a text file to the console (like Mplus output, for example)

### `source_https()`
Sources one or more `.R` scripts from HTTPS URLs, bypassing SSL verification if necessary.

### `svalues()`
Extracts starting values from Mplus output 

### `tab1()`
One-way table summary with frequencies and proportions, for a labeled variable.

### `tab2()`
Two-way cross-tabulation using `gmodels::CrossTable()`, with labeled output and proportion options.

### `ttesti()`
Conducts a two-sample t-test based on summary statistics (mean, SD, n).

### `varlablist()`
Prints a variable-label table from a data frame. Adapts output format to console, HTML, LaTeX, or markdown.

### `varnames()`
Prints variable names from a data frame as a comma-separated string.

### `vDd()`
Returns a verbal descriptor (e.g., “small-to-medium”) based on a standardized mean difference value.

### `vDR()`
Returns a verbal descriptor based on the value of a correlation coefficient.

### `ztesti()`
Conducts a two-sample z-test based on summary statistics.


