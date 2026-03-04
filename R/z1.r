#' Z1 Score Transformation
#'
#' Transforms variables to a (0,1) scale with optional further transformations.
#'
#' @param data A data frame containing the variables to transform
#' @param vars Character vector of variable names to transform
#' @param method One of "default", "softmax", or "altmethod". Default is "default".
#' @param lambda For softmax method: number of standard deviations over which the 
#'   transformed variable is expected to be linear. Default is 2.
#' @param logit If TRUE, apply logit transformation to the (0,1) scaled variable
#' @param z If TRUE, apply inverse normal transformation (z-score) to the scaled variable
#' @param altmax Alternative maximum value for scaling (overrides observed maximum)
#' @param silent If FALSE, print back-transformation formulas
#'
#' @return A data frame with the original variables and additional `_z1` variables
#'   containing the transformed values. Each `_z1` column has an "equation" attribute
#'   containing the transformation equation used.
#'
#' @details
#' The default method scales variables using:
#'   `(x - min + (5/(8*1000))*sd) / (max - min + (10/(8*1000))*sd)`
#'
#' The softmax method uses a sigmoid transformation based on the mean and standard deviation.
#'
#' The altmethod uses a more complex approach involving regression on collapsed data.
#'
#' Access the transformation equation with `attr(result$var_z1, "equation")`
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = 1:10, y = seq(0, 100, length.out = 10))
#' result <- z1(data, vars = c("x", "y"))
#' head(result)
#' attr(result$x_z1, "equation")  # View transformation equation
#' }
#'
#' @export
z1 <- function(data, vars, method = "default", lambda = 2, 
               logit = FALSE, z = FALSE, altmax = NULL, silent = TRUE) {
  
  # Constants
  a1 <- 5
  b <- 8
  a2 <- 10
  k <- 1000
  
  # Validation
  if (logit && z) {
    stop("specify z or logit, not both")
  }
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Create output data frame
  result <- data
  
  # Process each variable
  for (var in vars) {
    
    if (!var %in% names(data)) {
      warning(paste("Variable", var, "not found in data"))
      next
    }
    
    x <- data[[var]]
    x_z1 <- NA_real_
    equation <- ""  # Initialize equation string
    foo <- NULL
    hoo <- NULL
    
    if (method == "softmax") {
      # Softmax transformation
      x_mean <- mean(x, na.rm = TRUE)
      x_sd <- sd(x, na.rm = TRUE)
      
      x_z1 <- 1 / (1 + exp(-1 * (x - x_mean) / (lambda * x_sd / (2 * pi))))
      equation <- paste0(paste0(var, "_z1"), " = 1 / (1 + exp(-1 * (", var, " - ", 
                         format(x_mean, scientific = FALSE), ") / (", lambda, " * ", 
                         format(x_sd, scientific = FALSE), " / (2 * pi))))")
      
    } else if (method == "altmethod") {
      # Alternative method using intermediate values
      temp_data <- data.frame(var_col = x)
      temp_counts <- table(x, useNA = "no")
      
      collapsed <- data.frame(
        var_col = as.numeric(names(temp_counts)),
        count = as.numeric(temp_counts)
      )
      
      collapsed <- collapsed[order(collapsed$var_col), ]
      collapsed$var_t <- collapsed$var_col + 0.5
      
      if (nrow(collapsed) > 1) {
        collapsed$var_d <- c(diff(collapsed$var_t), 0.5)
        
        # Regression for last value interpolation
        if (nrow(collapsed) > 2) {
          lm_fit <- lm(var_d ~ var_col, data = collapsed[-nrow(collapsed), ])
          b0 <- coef(lm_fit)[1]
          b1 <- coef(lm_fit)[2]
          
          last_idx <- nrow(collapsed)
          collapsed$var_t[last_idx] <- collapsed$var_col[last_idx] + 
            b0 + b1 * collapsed$var_col[last_idx]
        }
      }
      
      min_val <- min(collapsed$var_col)
      collapsed$var_t_scaled <- collapsed$var_t - min_val
      denom <- max(collapsed$var_t_scaled)
      
      # Map back to original data
      x_z1 <- numeric(length(x))
      for (i in seq_along(x)) {
        if (!is.na(x[i])) {
          match_idx <- which(collapsed$var_col == x[i])
          if (length(match_idx) > 0) {
            x_z1[i] <- collapsed$var_t_scaled[match_idx[1]] / denom
          }
        }
      }
      equation <- paste0(paste0(var, "_z1"), " = (transformed_", var, " - min) / (max - min)")
      
    } else {
      # Default method
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)
      x_sd <- sd(x, na.rm = TRUE)
      
      if (!is.null(altmax)) {
        x_max <- altmax
      }
      
      numerator <- (x - x_min + (a1 / (b * k)) * x_sd)
      denominator <- (x_max - x_min + (a2 / (b * k)) * x_sd)
      
      x_z1 <- numerator / denominator
      
      # Calculate foo and hoo for equation
      foo <- (x_max - x_min + (a2 / (b * k)) * x_sd)
      hoo <- (x_min - (a1 / (b * k)) * x_sd)
      
      equation <- paste0(paste0(var, "_z1"), " = (", var, " - ", 
                         format(hoo, scientific = FALSE), ") / ", 
                         format(foo, scientific = FALSE))
    }
    
    # Apply further transformations and update equation
    if (logit) {
      x_z1 <- log(x_z1 / (1 - x_z1))
      equation <- paste0("logit(", equation, ") = log(", paste0(var, "_z1"), " / (1 - ", paste0(var, "_z1"), "))")
    }
    
    if (z) {
      x_z1 <- qnorm(x_z1)
      equation <- paste0("qnorm(", equation, ") = qnorm(", paste0(var, "_z1"), ")")
    }
    
    # Print back-transformation notes if requested
    if (!silent && method == "default") {
      x_min <- min(data[[var]], na.rm = TRUE)
      x_max <- max(data[[var]], na.rm = TRUE)
      x_sd <- sd(data[[var]], na.rm = TRUE)
      
      if (!is.null(altmax)) {
        x_max <- altmax
      }
      
      foo <- (x_max - x_min + (a2 / (b * k)) * x_sd)
      hoo <- (x_min - (a1 / (b * k)) * x_sd)
      
      cat("Equation for", var, ":\n")
      cat(equation, "\n\n")
    }
    
    # Attach equation as attribute
    attr(x_z1, "equation") <- equation
    
    # Add to result data frame
    result[[paste0(var, "_z1")]] <- x_z1
  }
  
  return(result)
}
