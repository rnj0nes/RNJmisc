folder_path <- "/Users/rnj/Dropbox/Work/Syntax/RNJMISC/"

  # Get a list of all files in the specified folder that end with ".R"
  r_files <- list.files(path = folder_path, pattern = "\\.R$", ignore.case = TRUE, full.names = TRUE)
  
  # Check if there are any ".R" files in the folder
  if (length(r_files) == 0) {
    message("No .R files found in the specified directory.")
    return()
  }
  
  # Source each of the ".R" or ".r" files except the specified file
  for (file in r_files) {
    # Extract the base name and check if it matches the excluded file name
    file_name <- basename(file)
    if (file_name == "- source RNJmisc.r") {
      message("Skipping: ", file)
      next
    }
    
    message("Sourcing: ", file)
    tryCatch({
      source(file)
    }, error = function(e) {
      message("Error sourcing ", file, ": ", e$message)
    })
  }
  

