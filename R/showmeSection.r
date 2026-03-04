#' Display File Section
#'
#' Displays a section of a text file starting from an optional search string
#' and continuing until three consecutive blank lines are encountered.
#'
#' @param x Path to the text file
#' @param search_string Optional string to search for as a starting point
#'
#' @details
#' If search_string is provided, starts display from that line; otherwise starts from line 1.
#' Stops when three consecutive blank lines are found.
#'
#' @examples
#' \dontrun{
#' showmeSection("/path/to/file.txt", "SECTION START")
#' }
#'
#' @export

showmeSection <- function(x, search_string = NULL) {

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
   blank_line_count <- 0
   
   # Print the contents starting from the matched line or the beginning
   for (i in start_index:length(file_contents)) {
      if (file_contents[i] == "") {
         blank_line_count <- blank_line_count + 1
      } else {
         blank_line_count <- 0
      }
      
      # Stop printing after encountering three consecutive blank lines
      if (blank_line_count == 3) {
         break
      }
      
      cat(file_contents[i], "\n")
   }
}
