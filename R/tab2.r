#' Two-Way Cross-Tabulation with Flexible Proportions
#'
#' Produces a labeled 2-way cross-tabulation with flexible options for
#' displaying proportions. Supports labeled variables from haven package.
#'
#' @param data A data frame containing the variables
#' @param row_var The variable to display as rows (unquoted)
#' @param col_var The variable to display as columns (unquoted)
#' @param prop_option Proportions to show: "t" (total), "r" (row), "c" (column), "cell"
#'
#' @return Prints a formatted 2-way table
#'
#' @details
#' Uses gmodels::CrossTable for tabulation. Handles missing values explicitly
#' and preserves variable labels and names when available.
#'
#' @examples
#' \dontrun{
#' tab2(mtcars, as.factor(cyl), as.factor(gear))
#' }
#'
#' @export
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
