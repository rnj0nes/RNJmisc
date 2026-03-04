#' One-Way Frequency Table
#'
#' Creates a one-way frequency table for a variable with flexible output formats.
#'
#' @param data A data frame or vector
#' @param var The variable to tabulate (optional if data is a single-column data frame)
#' @param digits Number of decimal places for proportions (default 1)
#' @param show_na Whether to include missing values (default TRUE)
#' @param style Output format: "ascii" (default), "md", "markdown", "rmarkdown", or "rmd"
#' @param headings Whether to include column headings
#'
#' @return A formatted frequency table
#'
#' @examples
#' \dontrun{
#' # ASCII output
#' tab1(mtcars, hp)
#'
#' # Markdown output
#' tab1(mtcars, hp, style = "md")
#' }
#'
#' @export
tab1 <- function(data, var = NULL, digits = 1, show_na = TRUE,
                 style = c("ascii","md","markdown","rmarkdown","rmd"),
                 headings = FALSE) {

  # ---- extract vector ----
  x_raw <- if (!is.null(var)) {
    var <- rlang::enquo(var)
    dplyr::pull(data, !!var)
  } else if (is.data.frame(data)) {
    if (ncol(data) != 1L) stop("When 'var' is missing, 'data' must have exactly 1 column.")
    data[[1]]
  } else {
    data
  }

  # keep variable label (if any) before converting
  lab <- tryCatch(labelled::var_label(x_raw), error = function(e) NULL)
  # convert haven-labelled to factors for nice levels
  x <- haven::as_factor(x_raw)

  # choose output style (no global st_options changes)
  style_in <- tolower(style[1])
  st_style <- if (style_in %in% c("md","markdown","rmarkdown","rmd")) "rmarkdown" else "simple"
  plain_ascii <- (st_style != "rmarkdown")

  # build freq object
  fr <- summarytools::freq(
    x,
    digits     = digits,
    report.nas = show_na,
    style      = st_style
  )

  # optional: print just Label/Type header (no "Frequencies" / var name)
  if (!is.null(lab) && nzchar(lab)) {
    if (st_style == "rmarkdown") {
      cat(paste0("**Label:** ", lab, "  \n"))
      cat(paste0("**Type:** ", if (is.factor(x)) "Factor" else paste(class(x), collapse = ", "), "\n\n"))
    } else {
      cat(paste0("Label: ", lab, "\n"))
      cat(paste0("Type: ", if (is.factor(x)) "Factor" else paste(class(x), collapse = ", "), "\n\n"))
    }
  }

  # print the table WITHOUT headings (so no "Frequencies" and no "#### x")
  print(fr, headings = FALSE, plain.ascii = plain_ascii, style = st_style)

  invisible(fr)
}
