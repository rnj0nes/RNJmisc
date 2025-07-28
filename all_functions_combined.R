# ---- FILE: Dstar.r ----

# Dstar function
# Rich Jones (Rich_Jones@Brown.edu) 2021-04-04
# --------------------------------------------------------------
# finds the optimal conversion factor between logit and probit
# scaled regression coefficients (D) as the ratio of the 
# normal density and logistic density corresponding to the 
# marginal value of the proportion with the indicated response
# (e.g., proportion making an error)
#
# Dstar is the conversion factor for moving between
# logit and probit parameters (sometimes people use
# pi/sqrt(3), sometimes 1.7, sometimes 1.6, here's an
# exact value determined by marginal proportion of
# the grouping variable (proportion with outcome,
# proportion with indicated response on test item)

Dstar <- function(x){

  # logit function
  # places a proportion (x) on log odds scale
  logit <- function(x){
      return(log((x)/(1-(x))))
   }

   # logistic probability density function
   # x is a proportion
   # mu is the mean, s is the scale
   logit.pdf=function(x,mu=0,s=1){
      k=((x)-mu)/s
      return(exp(-k)/(s*(1+exp(-k))^2))
   }

   return(dnorm(qnorm((x)))/logit.pdf(logit((x))))

}
# > Dstar(.5)
# [1] 1.595769
# > Dstar(.05)
# [1] 2.171277
# > Dstar(.1+.4)
# [1] 1.595769
# curve(expr = Dstar, from = .01, to = .99)



# ---- FILE: esiD.r ----

# compute effect size (d) from mean, sd, n
esiD <- function(m1,sd1,n1,m2,sd2,n2) {
  # Uses weighted pooled SD, so produces Hedge's g version of Cohen's d
  # 95% Confidence interval from Hedge LV Oklin I. Statistical methods for 
  # meta-analysis. Orlando, Academic Press Inc., 2014 p86 per Dong Kyu Lee,
  # Alternatives to P value: confidence interval and effect size. Korean 
  # Journal of Anaesthesiology 2016,69(6):555-562
  pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))
  d <-(m1-m2)/pooled.sd
  vard <- sqrt(((n1+n2)/(n1*n2))+(d^2/(2*(n1+n2))))
  alpha <- .05
  d.ll <- d+qnorm(alpha/2)*vard
  d.ul <- d+qnorm(1-(alpha/2))*vard
  message(paste0("Cohen's d (Hedge's g), the standardized mean difference: ", sprintf("%0.2f", d)))
  message(paste0("d = ","(",m1," - ",m2,")/(sqrt((((",n1,"-1)*",sd1,"^2)+((",n2,"-1)*",sd2,"^2))/(",n1," + ",n2,"-2)))"))
  message(paste0("95% CI on d: ", sprintf("%0.2f", d.ll) ,"," , sprintf("%0.2f", d.ul) ))
  dobj <- c(d,d.ll,d.ul)
  dobj
}
#dis <- esiD(0.7,0.5,317,1.1,0.8,24)
#dis[1]


# ---- FILE: esiDneteffect.r ----

# compute effect size from two group pre-post differences

esiDneteffect <- function(m01,m02,m11,m12,sd0,sd1,n0,n1) {

  message(paste0("m01 <- ",m01," # control group mean at baseline"))
  message(paste0("m02 <- ",m02," # control group mean at follow-up"))
  message(paste0("m11 <- ",m11," # treatment group mean at baseline"))
  message(paste0("m12 <- ",m12," # treatment group mean at follow-up"))
  message(paste0("n0 <- ",n0," # control group sample size"))
  message(paste0("n1 <- ",n1," # treatment group sample size"))
  message(paste0("sd0 <- ",sd0," # control group standard deviation at baseline"))
  message(paste0("sd1 <- ",sd1," # treatment group standard deviation at baseline"))
  message(paste0("pooled.sd <- sqrt((((n0-1)*sd0^2)+((n1-1)*sd1^2))/(n0+n1-2))"))
  message(paste0("net.effect <- (m12-m11)-(m02-m01)"))
  net.effect <- (m12-m11)-(m02-m01)
  net.effect.show <- round(net.effect,digits=3)
  message(paste0("# net.effect is ",net.effect.show))
  pooled.sd <- sqrt((((n0-1)*sd0^2)+((n1-1)*sd1^2))/(n0+n1-2))
  pooled.sd <- as.double(pooled.sd)
  pooled.sd.show <- round(pooled.sd,digits=3)
  message(paste0("# pooled.sd is ",pooled.sd.show))
  es.neteffect <- net.effect/pooled.sd
  es.neteffect.show <- round(es.neteffect,digits=3)
  message(paste0("effect.size <- net.effect / pooled.sd "))
  message(paste0("effect.size"))
  es.neteffect

}

#esiDneteffect(56.6,46.0,55.1,51.0,14.2,9.1,19,11) #CDI
#esiDneteffect(4.2,2.0,4.3,1.2,2.5,2.8,19,11) #DBDRS
# esiDneteffect(58.288,51.009,59.991,46.027,14.2,9.1,19,11)


# ---- FILE: esiDse.r ----

# compute effect size (d) from mean, se, n

esiDse <- function(m1,se1,n1,m2,se2,n2) {

  sd1 <- se1*sqrt(n1)
  sd2 <- se2*sqrt(n2)
  pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))
  d <-(m1-m2)/pooled.sd
  message(paste0("Cohen's d, the standardized mean difference: ", sprintf("%0.2f", d)))
  sd1.is <- paste0("(",se1,"*sqrt(",n1,"))")
  sd2.is <- paste0("(",se2,"*sqrt(",n2,"))")
  message(paste0("d = ","(",m1," - ",m2,")/(sqrt((((",n1,"-1)*",sd1.is,"^2)+((",n2,"-1)*",sd2.is,"^2))/(",n1," + ",n2,"-2)))"))
  d

}

# esiDse(m1=1.743, se1=0.062,m2=1.787,se2=0.035,n1=48,n2=144)


# ---- FILE: esiP.r ----

# compute effect size from two propotions

esiP <- function(p1,p2) {

	h <- abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))

	h

}


# ---- FILE: ess.r ----

# Clustered samples and Effective Sample Size
#
# van Breukelen, G. J., & Candel, M. J. (2012). 
# Calculating sample sizes for cluster randomized trials:
# we can keep it simple and efficient! 
# Journal of Clinical Epidemiology, 65(11), 1212-1218. 
#
# The factor ((m-1)*r + 1) ... is known as the 
# design effect (DE). Dividing the total sample size 
# m*k by the DE gives the effective sample size, 
# that is, the sample size needed for individual 
# randomization to have the same power and precision
# as the cluster randomized trial. 

ess <- function(m,k,r) {
  # m is number of observations per cluster (i.e., 
  #    repeated observations on a patient)
  # k is the number of clusters (i.e., patients)
  # m*k is the total number of observations studied
  # r is the intra-cluster correlation coefficient
  design_effect <- ((m-1)*r+1)
  effective_sample_size <- (m*k)/design_effect
  c(design_effect, effective_sample_size)
}

# checks out confirmed from
# https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/clustered-data
# ess(25,4,.017)
# [1]  1.40800 71.02273

# Consider an EMA study where the goal is to
# find the correlation between two variables.
# What is the smallest correlation that can be
# detected?
#
# We know that the minimum R that can be detected
# with two-tailed alpha level of 5% and 80% power is
# minR <- tanh(sqrt(8/n)) where n is the sample size.
#
# We will use the effective sample size of the
# EMA study to figure out what the minimum detectable
# correlation is
#
# Assume 80 patients (k)
# with 21 repeated observations (m)
# and an ICC of 0.5
# result_ess <- ess(21,80,.5)
# result_ess
# minR <- tanh(sqrt(8/result_ess[2]))
# minR




# ---- FILE: extractEigenvalues.r ----

#     I have a text file called "ex01.out". In it is a block of text that 
#     I have reproduced below. The key words that identify the start of 
#     this block of text is "EIGENVALUES FOR SAMPLE CORRELATION MATRIX". 
#     The block ends with "EXPLORATORY FACTOR ANALYSIS WITH 1 FACTOR". 
#     Inside those two key phrases are a series of numbers. One set of 
#     numbers are ordinal indicators (1,2,3,4,5,6) and the others are 
#     the actual eigenvalues. I would like to extract the eigenvalues 
#     into a vector. I would like to be able to do this in a function, 
#     so I can extract eigenvalues from other files (the values might
#     be different). Here is the text block:
#
#  I didn't even give the block to ChatGPT. It gave me most of this 
#  function that I only had to modify a little bit to get to work.

# Example usage:
# numbers <- c(1.000, 2.000, 3.000, 4.000, 5.000, 2.942, 0.997, 0.704, 0.502, 0.437, 6.000, 0.418, 1.000)
# filtered_numbers <- removeIntegers(numbers)
# print(filtered_numbers)

extractEigenvalues <- function(filename) {

   
   removeIntegers <- function(numbers) {
      # Convert the character vector to numeric if it isn't already
      if (is.character(numbers)) {
         numbers <- as.numeric(numbers)
      }
   
      # Create a logical vector to identify non-integers
      non_integers <- (numbers != floor(numbers)) | (floor(numbers) != numbers - 0.000)
     
      # Return the vector without the integers
      return(numbers[non_integers])
   }


   # Read the entire content of the file
   lines <- readLines(filename)
   
   # Find the start and end of the relevant block
   start_index <- min(which(grepl("EIGENVALUES FOR SAMPLE CORRELATION MATRIX", lines)))
   end_index <- min(which(grepl("EXPLORATORY FACTOR ANALYSIS WITH 1 FACTOR", lines)))
   
   # Extract the block of lines containing eigenvalues
   eigen_block <- lines[start_index:end_index]
   
   # Initialize a vector to hold the extracted eigenvalues
   eigenvalues <- numeric()
   
   # Loop through each line and extract numbers
   for (line in eigen_block) {
      # Use regular expressions to find numeric values
      matches <- regmatches(line, gregexpr("[0-9]+\\.*[0-9]*", line))
      # Append to the eigenvalues vector if numbers are found
      if (length(matches[[1]]) > 0) {
         eigenvalues <- c(eigenvalues, as.numeric(matches[[1]]))
      }
   }
   
   # Return the eigenvalues
   return(removeIntegers(eigenvalues))
}

# Example usage:
# eigenvalues <- extractEigenvalues("path_to_your_file/ex01.out")
# print(eigenvalues)
# extractEigenvalues("EX01/ex01random.out")
# print(eigenvalues)


# ---- FILE: hist_discrete.r ----

# Define the hist_discrete function with precise break control
hist_discrete <- function(data, variable = NULL) {
   # Check if variable is NULL, which implies direct vector input
   if (is.null(variable)) {
      # data is assumed to be the vector of values
      values <- data
   } else {
      # Extract the variable from the dataframe
      values <- pull(data, {{ variable }})
   }
   
   # Remove missing values
   values <- na.omit(values)
   
   # Check if there are any values left to plot
   if (length(values) == 0) {
      message("No data available to plot after removing missing values.")
      return()
   }
   
   # Calculate the exact breaks needed
   min_val <- floor(min(values))
   max_val <- ceiling(max(values))
   # Create breaks such that there is one for each integer from the smallest to the largest
   breaks <- seq(min_val - 0.5, max_val + 0.5, by = 1)
   
   # Create a histogram with these breaks
   hist(values, breaks = breaks, right = FALSE,
        main = paste("Histogram of", deparse(substitute(variable))),
        xaxt = 'n')  # Turn off default x-axis to customize
   
   # Custom x-axis
   axis(1, at = min_val:max_val, labels = min_val:max_val)
}

# Example usage with the Palmer Penguins data
# library(palmerpenguins)
# data(penguins)
# Visualizing the 'year' column
# hist_discrete(penguins, "year")



# ---- FILE: include.r ----

# include is a function that will source a file and 
# pipe the output to a txt file of the same name
# Thanks ChatGPT


# include function
# Thanks to ChatGPT
# use instead of "source". Writes the commands and their output
# to a ".txt" file of the same name as the sourced file.

include <- function(filename, output_suffix = "txt") {
   # Define the output file name based on the input filename
   base_name <- tools::file_path_sans_ext(basename(filename))
   output_directory <- dirname(filename)
   output_filename <- paste0(output_directory, "/", base_name, ".", output_suffix)
   
   # Open the output file for writing
   fileConn <- file(output_filename, open = "wt")
   
   # Read and execute each line of the input file, capturing the output
   lines <- readLines(filename)
   for (line in lines) {
      # Write the command to the file
      cat(">", line, "\n", file = fileConn)
      # Capture and write the output of the command
      result <- capture.output(
         tryCatch({
            eval(parse(text = line))
         }, error = function(e) {
            cat("Error in", line, ":", e$message, "\n")
         })
      )
      writeLines(result, fileConn)
   }
   
   # Close the file connection
   close(fileConn)
   
   # Remove lines containing exactly "NULL"
   removeNullLines(output_filename)
   
   # Inform user of output location
   cat("Captured output has been saved to:", output_filename, "\n")
}

removeNullLines <- function(filepath) {
   # Read the contents of the file
   contents <- readLines(filepath)
   # Filter out any lines that are exactly "NULL"
   contents <- contents[contents != "NULL"]
   # Write the filtered contents back to the file
   writeLines(contents, filepath)
}

# Example usage:
# include("path/to/your_script.R", "txt")


# ---- FILE: itemrest.r ----

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


# ---- FILE: itemsummary.r ----

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


# ---- FILE: Lehr_repeated.r ----

# lehr_repeated.r
# Rich Jones
# 2019-09-26
# lehr_repeated is a function that computes the minimum 
# detectable effect size in Cohen's d metric assuming
# a two-group design with equal sample sizes (n) and
# a repeated measures design (m repeated measures) and
# a auto-correlation of the outcome (r) under the
# assumption that 80% power and a type-I error level are
# desired.
lehr_repeated <- function(n,m,r) {
  # n per group sample size
  # m number of repeated observations (including the first)
  # r the time-to-time correlation of the outcome
  # n = (16/d^2)*((1+(m-1)*r)/m)
  # d = (4*sqrt((m-1)*r+1))/(sqrt(m)*sqrt(n)) 
  (4*sqrt((m-1)*r+1))/(sqrt(m)*sqrt(n)) 
}
# Check/example
# > lehr_repeated(64,1,0)
# [1] 0.5
# the minimum detectable effect size with 64 per group
# with 1 observation and n=64 per group is Cohen's d of 0.5
# (That is correct). It does not matter what the value
# is for the correlation in this example
#
# Example
# > lehr_repeated(40,4,.7)
# [1] 0.5567764
# The minimum effect size (in Cohen's d metric) across gorups
# with 4 repeated observations, an autocorrelation of .7 and
# and a common group sample size of 40 is d=0.56



# ---- FILE: neff.r ----

# Effective per-group sample size (harmonic mean of two group sample sizes)
neff <- function(n1,n2) {
    2/((1/n1)+(1/n2))
}



# ---- FILE: pooledSD.r ----

# pooled sd

pooledSD <- function(sd1,sd2,n1,n2) {

   pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))
   pooled.sd <- as.double(pooled.sd)
   show.pooled.sd <- round(pooled.sd,digits=3)
   message(paste0("n1 <- ",n1))
   message(paste0("n2 <- ",n2))
   message(paste0("sd1 <- ",sd1))
   message(paste0("sd2 <- ",sd2))
   message(paste0("pooled.sd <- sqrt((((n1-1)*sd1^2)+((n2-1)*sd2^2))/(n1+n2-2))"))
   message(paste0("# pooled.sd = sqrt((((",n1,"-1)*",sd1,"^2)+((",n2,"-1)*",sd2,"^2))/(",n1,"+",n2,"-","2))"))
   a <- round((n1-1)*sd1^2,digits = 3)
   b <- round((n2-1)*sd2^2,digits = 3)
   c <- round((n1+n2-2),digits= 3)
   message(paste0("# pooled.sd = sqrt((((",a,")+((",b,"))/(",c,"))"))
   message(paste0("# pooled.sd = ",show.pooled.sd))
   pooled.sd

}

# pooledSD(sd1=0.43,sd2=0.442,n1=48,n2=144)


# ---- FILE: pvf.r ----

# p-value format (pvf)
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



# ---- FILE: rCI.r ----

# rCI function
# Rich Jones (Rich_Jones@Brown.edu) 2021-04-04
# --------------------------------------------------------------
# shows confidence interval on a correlation using Fisher's z transformation

rCI <- function(x, n, alpha = .05 , digits = 3) {

   ll <- tanh(atanh(x)+qnorm(alpha/2)*(1/sqrt(n-3)))
   ul <- tanh(atanh(x)+qnorm(1-alpha/2)*(1/sqrt(n-3)))
   ll <- round(ll, digits = digits)
   ul <- round(ul, digits = digits)
   ci <- c(ll,ul)
   return(ci)
}
# 
# > rCI(.9, n=120)
# [1] 0.859 0.929



# ---- FILE: scoreit.r ----

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


# ---- FILE: showme.r ----

showme <- function(x) {
    # Replace 'path/to/your/file.txt' with the path to your text file
    file_path <- x
    
    # Read the contents of the file
    file_contents <- readLines(file_path)
    
    # Print the contents to the console
    cat(file_contents, sep = "\n")
}


# ---- FILE: showmeSection.r ----


showmeSection <- function(x, search_string = NULL) {

   # Read the contents of the file
   file_contents <- readLines(x)
   
   # Start displaying from the first line if no search string is provided
   start_index <- 1
   
   # If a search string is provided, find the line where it first appears
   if (!is.null(search_string)) {
      matches <- grep(search_string, file_contents, fixed = TRUE)
      if (length(matches) > 0) {
         start_index <- matches[1]
      }
   }
   
   # Initialize a counter for consecutive blank lines
   blank_line_count <- 0
   
   # Print the contents starting from the matched line or the beginning
   for (i in start_index:length(file_contents)) {
      if (file_contents[i] == "") {
         blank_line_count <- blank_line_count + 1
      } else {
         blank_line_count <- 0
      }
      
      # Stop printing after encountering three consecutive blank lines
      if (blank_line_count == 3) {
         break
      }
      
      cat(file_contents[i], "\n")
   }
}


# ---- FILE: showmeSectionLines.r ----


showmeSectionLines <- function(x, search_string = NULL , lines = 999) {
   # Read the contents of the file
   file_contents <- readLines(x)
   
   # Start displaying from the first line if no search string is provided
   start_index <- 1
   
   # If a search string is provided, find the line where it first appears
   if (!is.null(search_string)) {
      matches <- grep(search_string, file_contents, fixed = TRUE)
      if (length(matches) > 0) {
         start_index <- matches[1]
      }
   }
   
   # Initialize a counter for consecutive blank lines
   line_count <- 0
   
   # Print the contents starting from the matched line or the beginning
   for (i in start_index:length(file_contents)) {
      line_count <- line_count + 1
      # Stop printing after "lines" number of lines
      if (line_count == lines) {
         break
      }
      cat(file_contents[i], "\n")
   }
}


# ---- FILE: source_https.r ----

# from Tony Breyal
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
# to  source R scripts hosted on a github repository
source_https <- function(url, ...) {
        # load package
        require(RCurl)
        
        # parse and evaluate each .R script
        sapply(c(url, ...), function(u) {
                eval(
                        parse(
                                text = getURL(
                                        u, 
                                        followlocation = TRUE, 
                                        cainfo = system.file(
                                                "CurlSSL", 
                                                "cacert.pem",
                                                package = "RCurl")
                                        )
                                )
                        , envir = .GlobalEnv)
        })
}

# Example
# source_https("https://raw.github.com/rnj0nes/RNJmisc/master/RNJmisc.r")


# ---- FILE: svalues.r ----

# Rich Jones (rich_jones@brown.edu)
# 2021-05-07
#
# svalues 
#            a function to look in a specified file, ideally a Mplus
#            output file, for that part of the output that specifies
#            the results of having asked for SVALUES in the output
#            and return the result as a list.

svalues <- function(filename) {
   svalues <- read.table(filename,sep="?", header=FALSE)
   svalues$linenum <- as.numeric(rownames(svalues))
   start<- svalues$linenum[which(svalues$V1=="MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES")]+1
   svalues$foo<-trimws(substr(svalues$V1,1,15),"l")
   end <- svalues$linenum[which(svalues$foo=="Beginning ")]-1
   svalues <- svalues[start:end,1]
   svalues <- trimws(svalues,"l")
   svalues <- trimws(svalues,"r")
   svalues
}

# > svalues.are <- svalues("foo.out")
# > svalues.are
# [1] "mpg ON weight*-0.00601;" "[ mpg*39.44028 ];"       "mpg*11.50634;"



# ---- FILE: tab1.r ----

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



# ---- FILE: tab2.r ----

# tab2: Labeled 2-Way Table with Flexible Proportions
#
# Description:
#   tab2() produces a labeled 2-way cross-tabulation using gmodels::CrossTable() 
#   with flexible options for displaying proportions. It supports labeled variables 
#   (from haven), preserves variable names, handles missing values explicitly,
#   and provides optional labeling in the output.
#
# Packages used:
#   - dplyr       (for data manipulation and pulling variables)
#   - rlang       (for tidy evaluation with quosures)
#   - stringr     (for formatting variable names)
#   - haven       (for handling variable labels)
#   - gmodels     (for CrossTable function)
#
# Usage:
#   tab2(data, row_var, col_var, prop_option = "t")
#
# Arguments:
#   data        : A data frame containing the variables.
#   row_var     : The variable to display as rows (unquoted).
#   col_var     : The variable to display as columns (unquoted).
#   prop_option : A string indicating which proportions to show:
#                   - "t"    : total proportions (default)
#                   - "r"    : row proportions (in Row Total column)
#                   - "c"    : column proportions (in Column Total row)
#                   - "cell" : proportions of total per cell
#
# Output:
#   Prints a formatted 2-way table with optional variable labels and 
#   selected proportion display. Missing values are displayed as "NA".
#
# Example (with native pipe):
#   data <- haven::zap_labels(haven::read_dta("myfile.dta"))
#   data |> tab2(gender, diagnosis, prop_option = "r")

tab2 <- function(data, row_var, col_var, prop_option = "t") {
  row_var_quo <- rlang::enquo(row_var)
  col_var_quo <- rlang::enquo(col_var)

  # Extract variable names and pad to 8 characters
  row_var_fullname <- rlang::as_label(row_var_quo)
  col_var_fullname <- rlang::as_label(col_var_quo)
  row_var_name <- stringr::str_pad(substr(row_var_fullname, 1, 8), 8, side = "right")
  col_var_name <- stringr::str_pad(substr(col_var_fullname, 1, 8), 8, side = "right")

  # Get variable labels (before zapping)
  row_label <- attr(data[[row_var_fullname]], "label")
  col_label <- attr(data[[col_var_fullname]], "label")

  # Zap labels to make variables safe for CrossTable
  data <- haven::zap_labels(data)

  # Pull variables and handle missing values
  row_data <- dplyr::pull(data, !!row_var_quo)
  col_data <- dplyr::pull(data, !!col_var_quo)
  row_data <- factor(ifelse(is.na(row_data), "NA", as.character(row_data)))
  col_data <- factor(ifelse(is.na(col_data), "NA", as.character(col_data)))

  # Set proportion flags
  prop.t <- FALSE
  prop.r <- FALSE
  prop.c <- FALSE
  prop.cell <- FALSE
  if (prop_option == "t") {
    prop.t <- TRUE
  } else if (prop_option == "r") {
    prop.r <- TRUE
  } else if (prop_option == "c") {
    prop.c <- TRUE
  } else if (prop_option == "cell") {
    prop.cell <- TRUE
  } else {
    stop('Invalid value for `prop_option`. Choose from "t", "r", "c", or "cell".')
  }

  # Capture CrossTable output
  out <- capture.output(
    gmodels::CrossTable(
      row_data, col_data,
      prop.chisq = FALSE,
      prop.t = prop.t,
      prop.r = prop.r,
      prop.c = prop.c,
      prop.cell = prop.cell,
      format = "SAS",
      digits = 3
    )
  )

  # Replace printed variable names
  out <- gsub("row_data", row_var_name, out, fixed = TRUE)
  out <- gsub("col_data", col_var_name, out, fixed = TRUE)

  # Insert label lines if either label is present
  label_lines <- character()
  if (!is.null(row_label)) {
    label_lines <- c(label_lines, paste0(row_var_name, ": ", row_label))
  }
  if (!is.null(col_label)) {
    label_lines <- c(label_lines, paste0(col_var_name, ": ", col_label))
  }
  if (length(label_lines) > 0) {
    out <- c(label_lines, "", out)
  }

  # Clean spacing: add one blank line at top, collapse extras
  out <- c("", out)
  out <- out[!(duplicated(out) & out == "")]

  # Print
  cat(out, sep = "\n")
}


# ---- FILE: ttesti.r ----

ttesti <- function(x,df) {
  pv <- (1-pt(abs(x),df))*2
  message(paste0("Two-tailed P-value with t(",n,") = " , sprintf("%6.4f", x))," is :")
  pv
}


# ---- FILE: varlablist.r ----

# varlablist: Display variable names and labels as a table
#
# Description:
#   varlablist() shows a table of variable names and their labels.
#   It adapts to the output format automatically:
#     - HTML: styled table with kableExtra
#     - LaTeX or Word: markdown/LaTeX table with knitr::kable
#     - Console or fallback: plain text with print()
#   You can also force plain text output using `text_only = TRUE`.
#
# Packages used:
#   - tibble       (builds the variable-label table)
#   - purrr        (extracts label attributes)
#   - dplyr        (filters selected variables)
#   - knitr        (formats tables)
#   - kableExtra   (styles HTML tables)
#
# Usage:
#   varlablist(data, vars = NULL, caption = "Variables and labels", text_only = FALSE)
#
# Arguments:
#   data      : A data frame with optional variable labels (e.g., from haven)
#   vars      : Optional character vector of variable names to include
#   caption   : Table caption (default = "Variables and labels")
#   text_only : If TRUE, forces plain text output (default = FALSE)
#
# Examples:
#   varlablist(mydata)
#   varlablist(mydata, vars = c("age", "sex"), text_only = TRUE)

varlablist <- function(data, vars = NULL, caption = "Variables and labels", text_only = FALSE) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  var_table <- tibble::tibble(
    variable = names(data),
    label = purrr::map_chr(data, ~ attr(.x, "label") %||% "")
  )

  var_table_selected <- var_table |>
    dplyr::filter(variable %in% vars)

  if (isTRUE(text_only)) {
    print(var_table_selected)
  } else if (knitr::is_html_output()) {
    label_table <- knitr::kable(var_table_selected, format = "html", caption = caption)
    print(kableExtra::kable_styling(label_table))
  } else if (knitr::is_latex_output()) {
    knitr::kable(var_table_selected, format = "latex", caption = caption)
  } else {
    knitr::kable(var_table_selected, format = "markdown", caption = caption)
  }
}


# ---- FILE: varnames.r ----

# varnames: Print variable names from a data frame in a single line
#
# Description:
#   varnames() prints the names of a data frame as a comma-separated string.
#   Can be used with the native pipe (`|>`) or as a regular function call.
#
# Usage:
#   df |> varnames
#   varnames(df)
#
# Arguments:
#   df : A data frame.
#
# Output:
#   Prints variable names separated by commas on a single line.
#
# Example:
#   iris |> varnames
#   varnames(mtcars)

varnames <- function(df) {
  cat(paste(names(df), collapse = ", "), "\n")
}


# ---- FILE: vDd.r ----

# Rich Jones (rnjones@brown.edu)
# 2021 May 30
# ------------------------------------------------
# How large is that standardized mean difference?
# verbal descriptor on a Cohen's d
# -----------------------------------------------
vDd <- function(d){
  d.is <- abs(d)
  if (d.is<.2) { 
    descriptor <- "trivial" 
  }
  if (d.is>=.2&d.is<.25) { 
    descriptor <- "small"
  }
  if (d.is>=.25&d.is<.45) { 
    descriptor <- "small-to-medium"
  }
  if (d.is>=.45&d.is<.55) { 
    descriptor <- "about a medium"
  }
  if (d.is>=.55&d.is<.75) {
    descriptor <- "medium-to-large" 
  }
  if (d.is>=.75&d.is<.85) {
    descriptor <- "about a large" 
  }
  if (d.is>=.85) { 
    descriptor <- "large" 
  }
  descriptor
}


# ---- FILE: vDr.r ----

# Rich Jones (rnjones@brown.edu)
# 2021 May 30
# ------------------------------------------------
# How large is that correlation?
# verbal descriptor on a correlation coefficient
# -----------------------------------------------
vDr <- function(r){
  r.is <- abs(r)
  if (r.is<.1) { 
    descriptor <- "trivial" 
  }
  if (r.is>=.1&r.is<.12) { 
    descriptor <- "small"
  }
  if (r.is>=.12&r.is<.28) { 
    descriptor <- "small-to-medium"
  }
  if (r.is>=.28&r.is<.32) { 
    descriptor <- "about a medium"
  }
  if (r.is>=.32&r.is<.48) {
    descriptor <- "medium-to-large" 
  }
  if (r.is>=.48&r.is<.52) {
    descriptor <- "about a large" 
  }
  if (r.is>=.52) { 
    descriptor <- "large" 
  }
  descriptor
}


# ---- FILE: ztesti.r ----

# ztesti
# get a two-tailed p-value from a z value
ztesti <- function(x) {
  pv <- (1-pnorm(abs(x)))*2
  message(paste0("Two-tailed P-value with z = " , sprintf("%6.4f", x))," is :")
  pv
}



