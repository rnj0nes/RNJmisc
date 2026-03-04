#' Summarize Item Response Frequencies
#'
#' Creates a frequency table showing response values and their counts for specified items.
#'
#' @param data A data frame
#' @param vars Character vector of variable names to summarize
#'
#' @return A data frame with items as rows and response values as columns, containing frequencies
#'
#' @examples
#' \dontrun{
#' itemsummary(survey_df, c("question1", "question2", "question3"))
#' }
#'
#' @export
itemsummary <- function(data, vars) {
   # Melt the data frame to long format with variable-value pairs
   long_data <- tidyr::pivot_longer(data, cols = all_of(vars), values_to = "Values", names_to = "Variable")
   # Replace actual NA with a character "NA" for counting
   long_data$Values <- tidyr::replace_na(as.character(long_data$Values), "NA")
   # Count occurrences of each value for each variable
   frequency_data <- long_data |>
      dplyr::group_by(Variable, Values) |>
      dplyr::summarise(Frequency = dplyr::n(), .groups = 'drop')
   # Spread the frequency data to wide format
   wide_frequency_data <- tidyr::pivot_wider(frequency_data, names_from = Values, values_from = Frequency, values_fill = list(Frequency = 0))
   return(wide_frequency_data)
}
# use:
# itemsummary(df,c("sad","blues","depress","happy","enjoy","hopeful"))
