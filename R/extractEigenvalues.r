#' Extract Eigenvalues from Factor Analysis Output
#'
#' Extracts eigenvalues from exploratory factor analysis output files
#' (such as Mplus or other statistical software output).
#'
#' @param filename Path to the output file containing eigenvalue information
#'
#' @return A numeric vector of eigenvalues
#'
#' @details
#' Searches for the section marked by "EIGENVALUES FOR SAMPLE CORRELATION MATRIX"
#' and extracts the numeric eigenvalues, removing integer sequence numbers.
#'
#' @export
extractEigenvalues <- function(filename) {

   
   removeIntegers <- function(numbers) {
      # Convert the character vector to numeric if it isn't already
      if (is.character(numbers)) {
         numbers <- as.numeric(numbers)
      }
   
      # Create a logical vector to identify non-integers
      non_integers <- (numbers != floor(numbers)) | (floor(numbers) != numbers - 0.000)
     
      # Return the vector without the integers
      return(numbers[non_integers])
   }


   # Read the entire content of the file
   lines <- readLines(filename)
   
   # Find the start and end of the relevant block
   start_index <- min(which(grepl("EIGENVALUES FOR SAMPLE CORRELATION MATRIX", lines)))
   end_index <- min(which(grepl("EXPLORATORY FACTOR ANALYSIS WITH 1 FACTOR", lines)))
   
   # Extract the block of lines containing eigenvalues
   eigen_block <- lines[start_index:end_index]
   
   # Initialize a vector to hold the extracted eigenvalues
   eigenvalues <- numeric()
   
   # Loop through each line and extract numbers
   for (line in eigen_block) {
      # Use regular expressions to find numeric values
      matches <- regmatches(line, gregexpr("[0-9]+\\.*[0-9]*", line))
      # Append to the eigenvalues vector if numbers are found
      if (length(matches[[1]]) > 0) {
         eigenvalues <- c(eigenvalues, as.numeric(matches[[1]]))
      }
   }
   
   # Return the eigenvalues
   return(removeIntegers(eigenvalues))
}

# Example usage:
# eigenvalues <- extractEigenvalues("path_to_your_file/ex01.out")
# print(eigenvalues)
# extractEigenvalues("EX01/ex01random.out")
# print(eigenvalues)
