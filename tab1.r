# 1-way table function
tab1 <- function(data, var) {
  var <- rlang::enquo(var)  # capture the variable name
  data |>
    dplyr::count(!!var) |>
    janitor::adorn_totals("row") |>
    janitor::adorn_percentages("col") |>
    janitor::adorn_pct_formatting(digits = 1) |>
    janitor::adorn_ns()
}

