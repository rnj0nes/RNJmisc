#' Display File Contents
#'
#' Reads and displays the contents of a text file to the console.
#'
#' @param x Path to the text file
#'
#' @examples
#' \dontrun{
#' showme("/path/to/file.txt")
#' }
#'
#' @export
showme <- function(x) {
    # Replace 'path/to/your/file.txt' with the path to your text file
    file_path <- x
    
    # Read the contents of the file
    file_contents <- readLines(file_path)
    
    # Print the contents to the console
    cat(file_contents, sep = "\n")
}
