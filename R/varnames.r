#' Print Variable Names from Data Frame 
#'
#' Prints the names of a data frame as a comma-separated string on a single line.
#'
#' @param df A data frame
#'
#' @return Prints variable names separated by commas
#'
#' @details
#' Useful for quickly viewing all variable names in a data frame.
#' Works with both native pipe and standard function calls.
#'
#' @examples
#' iris |> varnames()
#' varnames(mtcars)
#'
#' @export
varnames <- function(df) {
  cat(paste(names(df), collapse = ", "), "\n")
}