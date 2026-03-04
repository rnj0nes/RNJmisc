#' Display Variable Names and Labels as a Table
#'
#' Shows a table of variable names and their labels with automatic format detection.
#'
#' @param data A data frame with optional variable labels (from haven package)
#' @param vars Optional character vector of variable names to include
#' @param caption Table caption (default "Variables and labels")
#' @param text_only If TRUE, forces plain text output
#'
#' @return A formatted table of variable names and their labels
#'
#' @details
#' Adapts output format automatically:
#' - HTML: styled table with kableExtra
#' - LaTeX/Word: formatted table with knitr::kable
#' - Console: plain text
#'
#' @examples
#' \dontrun{
#' varlablist(mydata)
#' }
#'
#' @export

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
