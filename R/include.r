#' Source File and Save Output
#'
#' Sources an R script file and saves both the commands and their output to a text file.
#'
#' @param filename Path to the R script file to source
#' @param output_suffix File extension for the output file (default "txt")
#'
#' @details
#' Executes an R script and captures all commands and their output to a file
#' with the same name as the input file but with the specified suffix.
#' Useful for creating reproducible analysis logs.
#'
#' @examples
#' \dontrun{
#' include("myscript.R")
#' # Creates myscript.txt with commands and output
#' }
#'
#' @export
# include is a function that will source a file and 
# pipe the output to a txt file of the same name
# Thanks ChatGPT


# include function
# Thanks to ChatGPT
# use instead of "source". Writes the commands and their output
# to a ".txt" file of the same name as the sourced file.

include <- function(filename, output_suffix = "txt") {
   # Define the output file name based on the input filename
   base_name <- tools::file_path_sans_ext(basename(filename))
   output_directory <- dirname(filename)
   output_filename <- paste0(output_directory, "/", base_name, ".", output_suffix)
   
   # Open the output file for writing
   fileConn <- file(output_filename, open = "wt")
   
   # Read and execute each line of the input file, capturing the output
   lines <- readLines(filename)
   for (line in lines) {
      # Write the command to the file
      cat(">", line, "\n", file = fileConn)
      # Capture and write the output of the command
      result <- capture.output(
         tryCatch({
            eval(parse(text = line))
         }, error = function(e) {
            cat("Error in", line, ":", e$message, "\n")
         })
      )
      writeLines(result, fileConn)
   }
   
   # Close the file connection
   close(fileConn)
   
   # Remove lines containing exactly "NULL"
   removeNullLines(output_filename)
   
   # Inform user of output location
   cat("Captured output has been saved to:", output_filename, "\n")
}

removeNullLines <- function(filepath) {
   # Read the contents of the file
   contents <- readLines(filepath)
   # Filter out any lines that are exactly "NULL"
   contents <- contents[contents != "NULL"]
   # Write the filtered contents back to the file
   writeLines(contents, filepath)
}

# Example usage:
# include("path/to/your_script.R", "txt")
