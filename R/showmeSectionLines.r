#' Display Specific Number of File Lines
#'
#' Displays a specified number of lines from a text file starting from an optional search string.
#'
#' @param x Path to the text file
#' @param search_string Optional string to search for as a starting point
#' @param lines Number of lines to display (default 999)
#'
#' @details
#' If search_string is provided, starts display from that line; otherwise starts from line 1.
#'
#' @examples
#' \dontrun{
#' showmeSectionLines("/path/to/file.txt", "START", lines = 10)
#' }
#'
#' @export

showmeSectionLines <- function(x, search_string = NULL , lines = 999) {
   # Read the contents of the file
   file_contents <- readLines(x)
   
   # Start displaying from the first line if no search string is provided
   start_index <- 1
   
   # If a search string is provided, find the line where it first appears
   if (!is.null(search_string)) {
      matches <- grep(search_string, file_contents, fixed = TRUE)
      if (length(matches) > 0) {
         start_index <- matches[1]
      }
   }
   
   # Initialize a counter for consecutive blank lines
   line_count <- 0
   
   # Print the contents starting from the matched line or the beginning
   for (i in start_index:length(file_contents)) {
      line_count <- line_count + 1
      # Stop printing after "lines" number of lines
      if (line_count == lines) {
         break
      }
      cat(file_contents[i], "\n")
   }
}
