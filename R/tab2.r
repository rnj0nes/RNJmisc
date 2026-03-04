# tab2: Labeled 2-Way Table with Flexible Proportions
#
# Description:
#   tab2() produces a labeled 2-way cross-tabulation using gmodels::CrossTable() 
#   with flexible options for displaying proportions. It supports labeled variables 
#   (from haven), preserves variable names, handles missing values explicitly,
#   and provides optional labeling in the output.
#
# Packages used:
#   - dplyr       (for data manipulation and pulling variables)
#   - rlang       (for tidy evaluation with quosures)
#   - stringr     (for formatting variable names)
#   - haven       (for handling variable labels)
#   - gmodels     (for CrossTable function)
#
# Usage:
#   tab2(data, row_var, col_var, prop_option = "t")
#
# Arguments:
#   data        : A data frame containing the variables.
#   row_var     : The variable to display as rows (unquoted).
#   col_var     : The variable to display as columns (unquoted).
#   prop_option : A string indicating which proportions to show:
#                   - "t"    : total proportions (default)
#                   - "r"    : row proportions (in Row Total column)
#                   - "c"    : column proportions (in Column Total row)
#                   - "cell" : proportions of total per cell
#
# Output:
#   Prints a formatted 2-way table with optional variable labels and 
#   selected proportion display. Missing values are displayed as "NA".
#
# Example (with native pipe):
#   data <- haven::zap_labels(haven::read_dta("myfile.dta"))
#   data |> tab2(gender, diagnosis, prop_option = "r")

tab2 <- function(data, row_var, col_var, prop_option = "t") {
  row_var_quo <- rlang::enquo(row_var)
  col_var_quo <- rlang::enquo(col_var)

  # Extract variable names and pad to 8 characters
  row_var_fullname <- rlang::as_label(row_var_quo)
  col_var_fullname <- rlang::as_label(col_var_quo)
  row_var_name <- stringr::str_pad(substr(row_var_fullname, 1, 8), 8, side = "right")
  col_var_name <- stringr::str_pad(substr(col_var_fullname, 1, 8), 8, side = "right")

  # Get variable labels (before zapping)
  row_label <- attr(data[[row_var_fullname]], "label")
  col_label <- attr(data[[col_var_fullname]], "label")

  # Zap labels to make variables safe for CrossTable
  data <- haven::zap_labels(data)

  # Pull variables and handle missing values
  row_data <- dplyr::pull(data, !!row_var_quo)
  col_data <- dplyr::pull(data, !!col_var_quo)
  row_data <- factor(ifelse(is.na(row_data), "NA", as.character(row_data)))
  col_data <- factor(ifelse(is.na(col_data), "NA", as.character(col_data)))

  # Set proportion flags
  prop.t <- FALSE
  prop.r <- FALSE
  prop.c <- FALSE
  prop.cell <- FALSE
  if (prop_option == "t") {
    prop.t <- TRUE
  } else if (prop_option == "r") {
    prop.r <- TRUE
  } else if (prop_option == "c") {
    prop.c <- TRUE
  } else if (prop_option == "cell") {
    prop.cell <- TRUE
  } else {
    stop('Invalid value for `prop_option`. Choose from "t", "r", "c", or "cell".')
  }

  # Capture CrossTable output
  out <- capture.output(
    gmodels::CrossTable(
      row_data, col_data,
      prop.chisq = FALSE,
      prop.t = prop.t,
      prop.r = prop.r,
      prop.c = prop.c,
      prop.cell = prop.cell,
      format = "SAS",
      digits = 3
    )
  )

  # Replace printed variable names
  out <- gsub("row_data", row_var_name, out, fixed = TRUE)
  out <- gsub("col_data", col_var_name, out, fixed = TRUE)

  # Insert label lines if either label is present
  label_lines <- character()
  if (!is.null(row_label)) {
    label_lines <- c(label_lines, paste0(row_var_name, ": ", row_label))
  }
  if (!is.null(col_label)) {
    label_lines <- c(label_lines, paste0(col_var_name, ": ", col_label))
  }
  if (length(label_lines) > 0) {
    out <- c(label_lines, "", out)
  }

  # Clean spacing: add one blank line at top, collapse extras
  out <- c("", out)
  out <- out[!(duplicated(out) & out == "")]

  # Print
  cat(out, sep = "\n")
}
