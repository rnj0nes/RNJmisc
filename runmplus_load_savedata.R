# Example 1
#
# res <- runmplus_load_savedata("model.out", m = 20, debug = TRUE)
# d   <- res$data
#
# Example 2
#
# savedata <- runmplus_load_savedata("ex0204.out")
# df <- savedata$data
#-------------------------
#' Load Mplus SAVEDATA using an .out file
#'
#' @param out Path to the Mplus .out file that lists the SAVEDATA file and the
#'            "Order [and format] of variables" section.
#' @param m   Integer. Number of imputations if the variable list uses leading
#'            '+' to denote imputed blocks (e.g., +var -> varm1..varmM).
#' @param debug Logical. If TRUE, prints progress messages.
#' @param force_numeric Logical. If TRUE, coerce all columns to numeric
#'                      (non-numeric coerced to NA), matching Stata destring, force.
#' @return A list with elements:
#'         - data: data.frame of the SAVEDATA
#'         - mplus_version, idvariable, saved_file
#'         - variable_names, widths, fixed_width (logical), imputations (M or NULL)
runmplus_load_savedata <- function(out, m = NULL, debug = FALSE,
                                   force_numeric = TRUE, encoding = "UTF-8") {
   stopifnot(length(out) == 1L, file.exists(out))
   
   lines <- readLines(out, encoding = encoding, warn = FALSE)
   low   <- trimws(tolower(lines))
   
   # helpers
   .msg <- function(...) if (isTRUE(debug)) message(...)
   .last_token <- function(s) {
      toks <- strsplit(s, "\\s+")[[1]]
      toks <- toks[toks != ""]
      if (length(toks)) toks[length(toks)] else NA_character_
   }
   
   # Mplus version (third token on the "Mplus VERSION" line in many outputs)
   v_idx <- grep("mplus version", low, fixed = TRUE)[1]
   mplus_version <- if (!is.na(v_idx)) {
      toks <- strsplit(lines[v_idx], "\\s+")[[1]]
      toks <- toks[toks != ""]
      if (length(toks) >= 3) toks[3] else NA_character_
   } else NA_character_
   
   # IDVARIABLE (best effort: last token on the line)
   id_idx <- grep("idvariable", low, fixed = TRUE)[1]
   idvariable <- if (!is.na(id_idx)) .last_token(lines[id_idx]) else NA_character_
   
   # Save file: the line after a literal "Save file"
   sf_idx <- which(low == "save file")[1]
   if (is.na(sf_idx)) stop("Could not find a 'Save file' header in: ", out)
   k <- sf_idx + 1L
   while (k <= length(lines) && trimws(lines[k]) == "") k <- k + 1L
   if (k > length(lines)) stop("Missing filename after 'Save file' in: ", out)
   filename <- trimws(lines[k])
   
   # Resolve relative path against the .out directory, if needed
   if (!file.exists(filename)) {
      candidate <- file.path(dirname(out), filename)
      if (file.exists(candidate)) filename <- candidate
   }
   if (!file.exists(filename)) stop("Saved data file not found: ", filename)
   
   # Find header for variable order block
   hdr_idx <- which(low %in% c("order and format of variables", "order of variables"))[1]
   if (is.na(hdr_idx)) stop("Could not find the 'Order ... of variables' section in: ", out)
   
   # The block starts two lines after header; ends at first blank line
   i <- hdr_idx + 2L
   
   specs <- list()
   while (i <= length(lines) && nzchar(trimws(lines[i]))) {
      raw  <- trimws(lines[i])
      toks <- strsplit(raw, "\\s+")[[1]]
      w    <- length(toks)
      
      if (w == 1L) {
         v <- toks[1]; f <- NA_character_
      } else if (w == 2L) {
         v <- toks[1]; f <- toks[2]
      } else {
         f <- toks[w]
         v <- paste(toks[-w], collapse = " ")
         v <- gsub(" ", "_", v, fixed = TRUE)
         v <- gsub("%", "", v, fixed = TRUE)
         v <- gsub("\\.", "", v)
      }
      
      v <- tolower(v)
      v <- gsub("#", "_c", v, fixed = TRUE)  # match Stata's filefilter step
      specs[[length(specs) + 1L]] <- list(name = v, fmt = f)
      i <- i + 1L
   }
   
   # Map Mplus formats to widths (F12.5 -> 12; I2 -> 2). Unknown -> NA.
   fmt_width <- function(f) {
      if (is.na(f) || f == "") return(NA_integer_)
      F <- toupper(f)
      if (grepl("^F[0-9]+\\.[0-9]+$", F)) {
         as.integer(sub("^F([0-9]+)\\..*$", "\\1", F))
      } else if (grepl("^I[0-9]+$", F)) {
         as.integer(sub("^I([0-9]+)$", "\\1", F))
      } else {
         NA_integer_
      }
   }
   
   # Expand imputation markers (+var -> varm1..varmM), and build widths
   col_names <- character()
   widths    <- integer()
   any_plus  <- FALSE
   
   for (sp in specs) {
      v <- sp$name
      w <- fmt_width(sp$fmt)
      
      if (startsWith(v, "+")) {
         any_plus <- TRUE
         base <- sub("^\\+", "", v)
         if (is.null(m) || !is.numeric(m) || length(m) != 1L || is.na(m) || m < 1)
            stop("Variable '", v, "' uses '+'. Supply a valid 'm' (number of imputations).")
         for (ii in seq_len(m)) {
            col_names <- c(col_names, paste0(base, "m", ii))
            widths    <- c(widths, w)
         }
      } else {
         col_names <- c(col_names, v)
         widths    <- c(widths, w)
      }
   }
   
   fixed_width <- all(!is.na(widths))
   .msg("Mplus version: ", mplus_version)
   .msg("IDVARIABLE: ", idvariable)
   .msg("Saved data file: ", filename)
   .msg("Parsed ", length(col_names), " variables; fixed_width = ", fixed_width)
   
   # Read the data as character, then numeric if requested (destring, force)
   if (fixed_width) {
      dat <- utils::read.fwf(file = filename,
                             widths = widths,
                             col.names = col_names,
                             strip.white = TRUE,
                             colClasses = rep("character", length(col_names)))
   } else {
      dat <- utils::read.table(file = filename,
                               header = FALSE,
                               col.names = col_names,
                               colClasses = "character",
                               stringsAsFactors = FALSE,
                               fill = TRUE,
                               comment.char = "")
   }
   
   if (isTRUE(force_numeric)) {
      dat[] <- lapply(dat, function(x) suppressWarnings(as.numeric(x)))
   }
   
   out_list <- list(
      data           = dat,
      mplus_version  = mplus_version,
      idvariable     = idvariable,
      saved_file     = filename,
      variable_names = col_names,
      widths         = widths,
      fixed_width    = fixed_width,
      imputations    = if (any_plus) as.integer(m) else NULL
   )
   class(out_list) <- c("runmplus_savedata", class(out_list))
   out_list
}
