# Rich Jones (rich_jones@brown.edu)
# 2021-05-07
#
# svalues 
#            a function to look in a specified file, ideally a Mplus
#            output file, for that part of the output that specifies
#            the results of having asked for SVALUES in the output
#            and return the result as a list.

svalues <- function(filename) {
   svalues <- read.table(filename,sep="?", header=FALSE)
   svalues$linenum <- as.numeric(rownames(svalues))
   start<- svalues$linenum[which(svalues$V1=="MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES")]+1
   svalues$foo<-trimws(substr(svalues$V1,1,15),"l")
   end <- svalues$linenum[which(svalues$foo=="Beginning ")]-1
   svalues <- svalues[start:end,1]
   svalues <- trimws(svalues,"l")
   svalues <- trimws(svalues,"r")
   svalues
}

# > svalues.are <- svalues("foo.out")
# > svalues.are
# [1] "mpg ON weight*-0.00601;" "[ mpg*39.44028 ];"       "mpg*11.50634;"

