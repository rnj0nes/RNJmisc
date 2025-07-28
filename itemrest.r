#- function for computing item-rest correlations
itemrest <- function(data) {
   # Ensure data is a data frame
   if (!is.data.frame(data)) {
      stop("Input must be a data frame.")
   }
   
   # Ensure all columns are numeric
   if (!all(sapply(data, is.numeric))) {
      stop("All columns in the data frame must be numeric.")
   }
   
   # Number of variables
   num_vars <- ncol(data)
   
   # Calculate the sum of all variables for each observation
   total_sum <- rowSums(data, na.rm = TRUE)
   
   # Initialize a vector to store the correlations
   correlations <- numeric(num_vars)
   
   # Calculate the correlation for each variable
   for (i in 1:num_vars) {
      # Sum of all other variables
      sum_other_vars <- total_sum - data[, i]
      
      # Calculate correlation and store it
      correlations[i] <- cor(data[, i], sum_other_vars, use = "complete.obs")
   }
   
   # Create a named vector for better readability
   names(correlations) <- colnames(data)
   
   # return the correlations if you want to use them later
   return(correlations)
}
