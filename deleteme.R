# Set your directory path
r_folder <- here::here()  # or use "." or a specific path like "~/GitHub/RNJmisc"

# List all .r or .R files
r_files <- list.files(path = r_folder, pattern = "\\.[rR]$", full.names = TRUE)

# Optional: sort alphabetically
r_files <- sort(r_files)

# Read and combine with file headers
all_code <- purrr::map2_chr(
  r_files,
  basename(r_files),
  ~ c(paste0("# ---- FILE: ", .y, " ----\n"), readLines(.x, warn = FALSE), "\n") |> paste(collapse = "\n")
)

# Collapse all into one long string
final_output <- paste(all_code, collapse = "\n")

# Write to file
writeLines(final_output, file.path(r_folder, "all_functions_combined.R"))