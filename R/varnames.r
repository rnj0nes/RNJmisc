# varnames: Print variable names from a data frame in a single line
#
# Description:
#   varnames() prints the names of a data frame as a comma-separated string.
#   Can be used with the native pipe (`|>`) or as a regular function call.
#
# Usage:
#   df |> varnames
#   varnames(df)
#
# Arguments:
#   df : A data frame.
#
# Output:
#   Prints variable names separated by commas on a single line.
#
# Example:
#   iris |> varnames
#   varnames(mtcars)

varnames <- function(df) {
  cat(paste(names(df), collapse = ", "), "\n")
}