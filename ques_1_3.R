library(readxl)
library(rstudioapi)

# Set working directory to the location of Excel file
setwd("C:/Users/ASUS/Desktop/assignment")

# Verify if the file exists
file.exists("United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls")

excel_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xlsx"
excel_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls"

# Read the Excel file with the specified range
dataset <- read_excel(excel_file, range = "B2:W158")
dataset

# Helper function to extract row data
extract_row_data <- function(row_num, data = dataset) {
  return(na.omit(as.numeric(data[row_num, -1])))
}
extract_row_data

# Extract data from specific rows
data_small_narrow <- extract_row_data(6)     # For small narrowbodies
data_large_narrow <- extract_row_data(45)    # For large narrowbodies
data_widebody <- extract_row_data(84)        # For widebodies
data_total_fleet <- extract_row_data(123)    # For total fleet

# Combine all extracted data into one set
combined_data <- c(data_small_narrow, data_large_narrow, data_widebody, data_total_fleet)
combined_data

# Check the number of observations in the combined set
length(combined_data)

# Randomly select 17 observations
set.seed(123)  # For reproducibility
sampled_data <- sample(combined_data, 17, replace = FALSE)
sampled_data
# View the sampled data
print(sampled_data)

# Function to get modes
calculate_modes <- function(data) {
  freq_table <- table(data)
  max_freq <- max(freq_table)
  modes <- as.numeric(names(freq_table[freq_table == max_freq]))
  if (length(modes) == length(data)) {
    return(NULL)
  }
  return(modes)
}

# Function to calculate frequency distribution
calculate_frequency_distribution <- function(data_sample) {
  n <- length(data_sample)
  k <- ceiling(log2(n))  # Number of classes
  min_val <- min(data_sample)
  max_val <- max(data_sample)
  class_width <- ceiling((max_val - min_val) / k)  # Ensure whole number width
  
  breakpoints <- seq(
    min_val - (class_width / 2),
    max_val + (class_width / 2),
    by = class_width
  )
  
  salary_bins <- cut(data_sample, breaks = breakpoints, right = TRUE)
  frequency_table <- table(salary_bins)
  return(frequency_table)
}

# Generate frequency distribution for the sample
frequency_table <- calculate_frequency_distribution(sampled_data)

# Print the frequency distribution
cat("Frequency Distribution for Sample:\n")
print(frequency_table)

# Function to analyze sample data
analyze_data <- function(data_sample, title) {
  mean_val <- mean(data_sample)
  median_val <- median(data_sample)
  modes <- calculate_modes(data_sample)
  sample_sd <- sd(data_sample)
  sample_var <- var(data_sample)
  quartiles <- quantile(data_sample, probs = c(0.25, 0.5, 0.75))
  tenth_percentile <- quantile(data_sample, probs = 0.10)
  ninth_decile <- quantile(data_sample, probs = 0.90)
  range_val <- max(data_sample) - min(data_sample)
  
  cat("Analysis of", title, ":\n")
  cat("Mean:", mean_val, "\n")
  cat("Median:", median_val, "\n")
  if (is.null(modes) || length(modes) == 0) {
    cat("Modes: None\n")
  } else {
    cat("Modes:", paste(modes, collapse = ", "), "\n")
  }
  cat("Sample Standard Deviation:", sample_sd, "\n")
  cat("Sample Variance:", sample_var, "\n")
  cat("Quartiles (Q1, Q2, Q3):", quartiles, "\n")
  cat("10th Percentile:", tenth_percentile, "\n")
  cat("9th Decile:", ninth_decile, "\n")
  cat("Range:", range_val, "\n\n")
}

# Analyze the 17 sample data
analyze_data(sampled_data, "Sample of 17 Observations")

# Plot histogram with color
plot_histogram <- function(frequency_table, title) {
  barplot(frequency_table,
          xlab = "Salary Ranges",
          ylab = "Frequency",
          col = "lightgreen",  
          border = "darkgreen", 
          space = 0,
          main = title
  )
}

# Plot the histogram for the sample data
plot_histogram(frequency_table, "Histogram of Sample Data")
