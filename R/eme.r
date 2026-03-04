#' Extract Mplus estimate row by header + param (with forgiving matching)
#'
#' @param params data.frame from MplusAutomation::readModels()$parameters$...
#'               (must include columns "paramHeader" and "param")
#' @param paramHeader single character value (e.g., "DY1.ON", "Residual.Variances")
#' @param param       single character value (e.g., "LY1", "Y2")
#'
#' @return If a match is found: the matching row(s) from `params`. As a
#'         side-effect, the result is assigned into the caller's environment
#'         under the name paste0(paramHeader, "_", param) (sanitized to a
#'         legal R name). If no exact match is found, prints suggestions or
#'         all available pairs and returns NULL (invisibly).
eme <- function(params, paramHeader, param) {
  stopifnot(is.data.frame(params))
  req_cols <- c("paramHeader", "param")
  if (!all(req_cols %in% names(params))) {
    stop("`params` must contain columns: ", paste(req_cols, collapse = ", "))
  }
  if (length(paramHeader) != 1L || length(param) != 1L)
    stop("`paramHeader` and `param` must each be length-1 character values.")
  paramHeader <- as.character(paramHeader)
  param <- as.character(param)

  # cleaner: lowercase, strip punctuation and whitespace
  .clean <- function(x) tolower(gsub("[[:punct:]\\s]+", "", as.character(x)))

  ph_user <- .clean(paramHeader)
  p_user  <- .clean(param)

  ph_clean <- .clean(params$paramHeader)
  p_clean  <- .clean(params$param)

  # 1) exact match on the cleaned pair -------------------------------
  idx <- which(ph_clean == ph_user & p_clean == p_user)

  if (length(idx) > 0) {
    res <- params[idx, , drop = FALSE]

    # assign with a readable name, sanitized for R
    obj_name_raw <- paste0(paramHeader, "_", param)
    obj_name <- gsub("[^A-Za-z0-9._]", "_", obj_name_raw)
    assign(obj_name, res, envir = parent.frame())

    return(res)
  }

  # 2) no exact pair match: offer suggestions ------------------------
  #    show rows where either header OR param (cleaned) matches the user input
  cand <- params[ ph_clean == ph_user | p_clean == p_user, c("paramHeader","param"), drop = FALSE ]
  cand <- unique(cand)

  if (nrow(cand) > 0) {
    message(
      "No exact match for paramHeader = '", paramHeader, "', param = '", param, "'.\n",
      "Did you mean one of these (original case & punctuation preserved)?"
    )
    print(cand, row.names = FALSE)
    return(invisible(NULL))
  }

  # 3) nothing matches at all: list all available pairs --------------
  message(
    "No matching entries. Listing all available (paramHeader, param) pairs:"
  )
  print(unique(params[c("paramHeader","param")]), row.names = FALSE)
  invisible(NULL)
}
