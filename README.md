RNJmisc
=======

Miscellaneous R utilities developed by Rich Jones. This collection includes tools for effect size estimation, descriptive statistics, psychometric scoring, and general utility functions.

## Installation

You can install RNJmisc directly from GitHub using `devtools`:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install RNJmisc from GitHub
devtools::install_github("rnj/RNJmisc")
```

## Functions

---

### Statistical Tests & P-values

**`ttesti()`**
Calculate two-tailed p-value from a t-statistic and degrees of freedom.

**`ztesti()`**
Calculate two-tailed p-value from a z-statistic.

**`pvf()`**
Format p-values for academic publication.

### Effect Size Estimation

**`Dstar()`**
Compute optimal conversion factor between logit and probit regression coefficients.

**`esiD()`**
Compute Cohen's d (Hedge's g) effect size with 95% confidence interval.

**`esiDneteffect()`**
Compute effect size for pre-post designs with control group.

**`esiDse()`**
Compute Cohen's d effect size from standard error values.

**`esiP()`**
Compute Cohen's h effect size from two proportions.

**`vDd()`**
Get verbal description of Cohen's d magnitude.

**`vDr()`**
Get verbal description of correlation coefficient magnitude.

**`rCI()`**
Compute confidence interval for a Pearson correlation.

### Sample Size & Design

**`neff()`**
Compute effective sample size (harmonic mean).

**`ess()`**
Compute effective sample size and design effect for clustered data.

**`pooledSD()`**
Compute pooled standard deviation for two groups.

**`Lehr_repeated()`**
Compute minimum detectable effect size for repeated measures.

### Data Transformation & Scoring

**`z1()`**
Transform variables to (0,1) scale with flexible options. Generates transformation equations.

**`scoreit()`**
Score multi-item scales with imputation; computes Cronbach's alpha.

**`itemrest()`**
Compute item-rest correlations for multi-item scales.

**`itemsummary()`**
Create frequency table of item responses.

### Descriptive Tables

**`tab1()`**
Create one-way frequency table with flexible output formats.

**`tab2()`**
Create two-way cross-tabulation with flexible proportion options.

**`varlablist()`**
Display variable names and labels as formatted table.

**`varnames()`**
Print variable names as comma-separated string.

### File Utilities

**`include()`**
Source R script and save commands and output to file.

**`showme()`**
Display contents of text file to console.

**`showmeSection()`**
Display section of text file from search string to blank lines.

**`showmeSectionLines()`**
Display specified number of lines from text file.

**`source_https()`**
Source R scripts from HTTPS URLs.

**`svalues()`**
Extract SVALUES from Mplus output files.

**`extractEigenvalues()`**
Extract eigenvalues from factor analysis output.

**`runmplus_load_savedata()`**
Utility for Mplus output (limited functionality).

### Visualization

**`hist_discrete()`**
