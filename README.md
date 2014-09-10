RNJmisc
=======

Rich Jones' miscellaneous stuff

# ADOs converted to R files

## wftree.r
Creates a workflow folder directory structure following the 
IFAR/QSP interpretation of JS Long's [The Workflow of
Data Analysis using Stata (2008).](http://www.indiana.edu/~jslsoc/web_workflow/wf_home.htm)

The code below sets up a set of workflow folders in the
current working directory. Note: since it is the
current working directory, you should setwd() to where
you'd like to have the workflow folder first
```
> setwd("c:/work")
> wftree("foo", private = c("RNJ", "DT"))
Current working folder is c:/work/FOO/POSTED/ANALYSIS
> getwd()
[1] "c:/work/FOO/POSTED/ANALYSIS"
```
Note that the folder name was forced to uppercase. Also 
note that specifying private folders is optional. The code:
```
> wftree("foo")
```
will work just fine.

If the workflow folder aldready exists, nothing will
happen. No files/folders will be overwritten. But,
the function will end up with the working directory
being reset to ./posted/analysis.

Here is what gets set up:
```
C:\WORK\FOO
  \- HOLD THEN DELETE
  \- TO FILE
  \ADMIN
     \BUDGET
     \CORRESPONDENCE
     \IRB
     \PROPOSAL
  \DOCUMENTATION
     \CODEBOOK
  \MAILBOX
  \POSTED
     \ANALYSIS
     \DATA
        \DERIVED
        \SOURCE
     \DESCSTATS
     \FIGURES
     \TEXT
  \PRIVATE
     \DT
     \RNJ
```



