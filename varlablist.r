# varlablist: Display variable names and labels as a table
#
# Description:
#   varlablist() shows a table of variable names and their labels.
#   It adapts to the output format automatically:
#     - HTML: styled table with kableExtra
#     - LaTeX or Word: markdown/LaTeX table with knitr::kable
#     - Console or fallback: plain text with print()
#   You can also force plain text output using `text_only = TRUE`.
#
# Packages used:
#   - tibble       (builds the variable-label table)
#   - purrr        (extracts label attributes)
#   - dplyr        (filters selected variables)
#   - knitr        (formats tables)
#   - kableExtra   (styles HTML tables)
#
# Usage:
#   varlablist(data, vars = NULL, caption = "Variables and labels", text_only = FALSE)
#
# Arguments:
#   data      : A data frame with optional variable labels (e.g., from haven)
#   vars      : Optional character vector of variable names to include
#   caption   : Table caption (default = "Variables and labels")
#   text_only : If TRUE, forces plain text output (default = FALSE)
#
# Examples:
#   varlablist(mydata)
#   varlablist(mydata, vars = c("age", "sex"), text_only = TRUE)

varlablist <- function(data, vars = NULL, caption = "Variables and labels", text_only = FALSE) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  var_table <- tibble::tibble(
    variable = names(data),
    label = purrr::map_chr(data, ~ attr(.x, "label") %||% "")
  )

  var_table_selected <- var_table |>
    dplyr::filter(variable %in% vars)

  if (isTRUE(text_only)) {
    print(var_table_selected)
  } else if (knitr::is_html_output()) {
    label_table <- knitr::kable(var_table_selected, format = "html", caption = caption)
    print(kableExtra::kable_styling(label_table))
  } else if (knitr::is_latex_output()) {
    knitr::kable(var_table_selected, format = "latex", caption = caption)
  } else {
    knitr::kable(var_table_selected, format = "markdown", caption = caption)
  }
}
