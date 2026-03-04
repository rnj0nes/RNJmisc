#' Format P-values for Academic Reporting
#'
#' Formats p-values according to academic publication guidelines.
#'
#' @param p A p-value (between 0 and 1)
#'
#' @return A character string with formatted p-value
#'
#' @details
#' Follows standard academic formatting:
#' - P > .999: "> .99"
#' - P >= .01: 2 decimal places (or 3 if between .045 and .049)
#' - .001 <= P < .01: 3 decimal places
#' - P < .001: "< .001"
#'
#' @examples
#' pvf(0.046)
#' pvf(0.001)
#' pvf(0.00001)
#'
#' @export
pvf <- function(p) {
  # For expressing P values in manuscripts and articles,
  # - If P > .999
  if (p > .999) {
    foo <- paste0("> .99")
  }
  # Else, if P ≥.01 [whether or not P is significant]
  if ((p >= .01) & (p < .999)) {
      # -  The actual value for P should be expressed
      # - Express to 2 digits, unless the 3-digit P-value is between .045 and .049, in which case express in 3 digits.
      foo <- sub("0.",".",paste0(round(p,digits=2)))
      if ((p < .055) &  (p>= .045)) {
        foo <- sub("0.",".",paste0(round(p,digits=3)))
      }
  }
  # - Else, if P < .01 and P > .001 
  # - Express to 3 digits. 
  if ((p < .01) & (p >= .001)) {
    foo <- sub("0.",".",paste0(round(p,digits=3)))
  }
  # - Else, If P < .001 
  if (p < .001) {
     # - use P < .001 (because precise P values with extreme results are sensitive to biases or departures from the statistical model.37 [p198])
     foo <- paste0("< .001")
  }
  foo
}
# pvf(.046)

