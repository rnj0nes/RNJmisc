#' Extract SVALUES from Mplus Output File
#'
#' Extracts the SVALUES section from an Mplus output file to retrieve starting values
#' for model parameters.
#'
#' @param filename Path to the Mplus output file
#'
#' @return A character vector containing the model parameters with final estimate values
#'
#' @details
#' Parses Mplus output to extract the section containing model commands with final
#' estimates that can be used as starting values for subsequent analyses.
#'
#' @examples
#' \dontrun{
#' svalues_import <- svalues("mymodel.out")
#' }
#'
#' @export
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

