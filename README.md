RNJmisc
=======

Rich Jones' miscellaneous stuff

# ADOs converted to R files

## wftree.r
Creates a workflow folder directory structure following the IRAR/QSP interpretation of
JS Long's Workflow for Data Analysis using Stata.

The code below sets up a set of workflow folders in the
current working directory. Note: since it is the
current working director, you should setwd() to where
you'd like to have the workflow folder first
```
> setwd("c:/work")
> wftree("foo", private = c("RNJ", "DT"))
Current working folder is c:/work/FOO/POSTED/ANALYSIS
> getwd()
[1] "c:/work/FOO/POSTED/ANALYSIS"
```




