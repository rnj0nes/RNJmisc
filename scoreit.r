#- Start scoreit Function
library(dplyr)
library(mice)
library(psych)

scoreit <- function(data, var_names, new_var_name, min_items) {
   # Ensure that var_names is a character vector
   var_names <- as.character(var_names)
   
   # Add a check for rows that have at least 'min_items' non-missing values
   valid_rows <- rowSums(!is.na(data[, var_names])) >= min_items
   
   # Perform mice imputation only on the rows that meet the 'min_items' condition
   if (any(valid_rows)) {
      tempData <- data[valid_rows, var_names]
      imputedData <- mice(tempData, method = 'pmm', m = 1, maxit = 5, seed = 500, printFlag = FALSE)
      completedData <- complete(imputedData)
      
      # Bind the imputed variables back to the original data frame (only for the valid rows)
      data[valid_rows, var_names] <- completedData
   }
   
   # Calculate the sum of the imputed variables, create a new variable, and handle min_items
   data <- data %>%
      rowwise() %>%
      mutate(!!new_var_name := if_else(sum(!is.na(c_across(all_of(var_names)))) >= min_items,
                                       sum(c_across(all_of(var_names)), na.rm = TRUE),
                                       as.numeric(NA)))
   
   # Compute internal consistency reliability (Cronbach's alpha) for valid rows
   if (any(valid_rows)) {
      alpha_res <- alpha(data[valid_rows, var_names], check.keys = TRUE)
      cat("Cronbach's alpha for the specified items: ", alpha_res$total$raw_alpha, "\n")
   } else {
      cat("Cronbach's alpha cannot be computed as no rows have the minimum required non-missing items.\n")
   }
   
   itemrest.correlations <- itemrest(data[, var_names])
   cat("Item-sum of Remainder of Item Correlations:","\n", itemrest.correlations,"\n")
   
   # Return the modified data frame
   return(data)
   
}

# Example usage:
# Assuming `df` is your data frame, and you want to sum the variables 'var1', 'var2', 'var3' into a new variable 'total'
# df <- df %>% scoreit(var_names = c('var1', 'var2', 'var3', 'var4', 'var5'), new_var_name = 'total', min_items = 3)