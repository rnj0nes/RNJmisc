contents <- function(df, head = FALSE) {
  stopifnot(is.data.frame(df))

  # Column widths (sum(w) + 4*6 = 90)
  w_var <- 26L
  w_num <- 8L
  widths <- c(w_var, rep(w_num, 5))

  # which variables
  vars <- names(df)
  if (isTRUE(head)) vars <- head(vars, 10)

  # labels: haven/Hmisc-compatible, else attr(label)
  get_label <- function(x) {
    lbl <- attr(x, "label", exact = TRUE)
    if (is.null(lbl) || !nzchar(as.character(lbl))) "" else as.character(lbl)
  }
  labels <- vapply(df[vars], get_label, character(1))

  # helpers
  pad_cell <- function(x, w, align = c("left", "right")) {
    align <- match.arg(align)
    s <- substr(x, 1L, w)  # hard truncate by chars
    n <- nchar(s, type = "width", allowNA = FALSE, keepNA = FALSE)
    if (n < w) {
      if (align == "left") {
        paste0(s, strrep(" ", w - n))
      } else {
        paste0(strrep(" ", w - n), s)
      }
    } else s
  }

  fmt_N   <- function(x) if (is.na(x)) strrep(" ", w_num) else sprintf("%8.0f", x)
  fmt_num <- function(x) if (is.na(x)) strrep(" ", w_num) else sprintf("%8.3f", x)

  build_var_cell <- function(name, lbl) {
    # if label exists, append as "name (trunc_label)" within 26 chars.
    if (!nzchar(lbl)) {
      pad_cell(name, w_var, "left")
    } else {
      base <- name
      # space + "(" + label + ")" costs 1 + 2 = 3 plus label chars
      room_for_label <- w_var - nchar(base, type = "width") - 3L
      if (room_for_label > 0) {
        lbl_trunc <- substr(lbl, 1L, room_for_label)
        pad_cell(paste0(base, " (", lbl_trunc, ")"), w_var, "left")
      } else {
        # no room: just the name, truncated/padded
        pad_cell(base, w_var, "left")
      }
    }
  }

  # header (exactly 90 chars)
  header_titles <- c("Variable", "N", "Mean", "SD", "Min", "Max")
  header_cells <- mapply(
    function(title, w, align) pad_cell(title, w, align),
    header_titles, widths, c("left", rep("right", 5)),
    USE.NAMES = FALSE
  )
  header <- paste0(
    paste0("| ", header_cells, " ", collapse = ""),
    "|"
  )

  # alignment row (markdown), widths must match exactly
  align_cells <- c(
    paste0(":", strrep("-", w_var - 1L)),
    rep(paste0(strrep("-", w_num - 1L), ":"), 5)
  )
  align_row <- paste0(
    paste0("| ", align_cells, " ", collapse = ""),
    "|"
  )

  # data rows
  data_rows <- vapply(seq_along(vars), function(i) {
    v <- vars[[i]]
    x <- df[[v]]
    lbl <- labels[[i]]

    # numeric-ish? logical treated as 0/1
    if (is.logical(x)) x <- as.numeric(x)
    nn <- sum(!is.na(x))
    if (is.numeric(x)) {
      m   <- if (nn > 0) mean(x, na.rm = TRUE) else NA_real_
      sdv <- if (nn > 1) stats::sd(x, na.rm = TRUE) else NA_real_
      mn  <- if (nn > 0) min(x, na.rm = TRUE) else NA_real_
      mx  <- if (nn > 0) max(x, na.rm = TRUE) else NA_real_
    } else {
      m <- sdv <- mn <- mx <- NA_real_
    }

    cells <- c(
      build_var_cell(v, lbl),
      fmt_N(nn),
      fmt_num(m),
      fmt_num(sdv),
      fmt_num(mn),
      fmt_num(mx)
    )

    # assemble one 90-char line
    paste0(paste0("| ", cells, " ", collapse = ""), "|")
  }, character(1))

  out <- c(header, align_row, data_rows)

  # print and return (so you can cat(writeLines) or write to file)
  cat(paste0(out, collapse = "\n"), "\n", sep = "")
  return(invisible(out))
}
