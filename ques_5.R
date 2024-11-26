library(readxl)
library(rstudioapi)

# Set working directory to the location of the Excel file
setwd("C:/Users/ASUS/Desktop/assignment")

# Verify if the file exists
file.exists("United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls")
# If the file exists, this should return TRUE

# If the file extension is .xlsx, update the file name accordingly:
input_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xlsx"

# If the file extension is .xls, use:
input_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls"

# Read the Excel file with the specified range
data_frame <- read_excel(input_file, range = "B2:W158")
data_frame

# Define categories
utilization_categories <- c("Block Hours", "Airborne Hours", "Departures")
asset_ownership_categories <- c("Rental", "Depreciation and Amortization")
supply_costs_categories <- c("Fuel/Oil", "Insurance", "Other (Inc. Tax)")
fleet_types <- c(
  "Small Narrowbodies",
  "Large Narrowbodies",
  "Widebodies",
  "Total Fleet"
)

# Row numbers
supply_costs_rows <- c(16, 55, 94, 133) - 5
asset_ownership_rows <- supply_costs_rows + 12
utilization_rows <- asset_ownership_rows + 13

extract_row_data <- function(row_number) {
  if (row_number > nrow(data_frame)) {
    stop("Row number exceeds data range.")
  }
  return(na.omit(as.numeric(data_frame[row_number, -1])))
}

extract_category_data <- function(row_number, category_labels) {
  rows_data <- lapply(
    seq_along(category_labels),
    function(i) extract_row_data(row_number + i)
  )
  costs <- unlist(rows_data)
  category <- factor(rep(category_labels, sapply(rows_data, length)))
  return(data.frame(costs = costs, category = category))
}

generate_box_plot <- function(data, title, y_label) {
  boxplot(costs ~ category,
          data = data,
          main = title,
          col = "lightblue",
          ylab = y_label,
          border = "darkblue"
  )
}

plot_by_category <- function(rows, category_labels, title, y_label) {
  windows(width = 1920 / 100, height = 1080 / 100) # Set window size
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  lapply(
    seq_along(rows),
    function(i) {
      generate_box_plot(
        extract_category_data(
          rows[i], category_labels
        ), fleet_types[i], y_label
      )
    }
  )
  mtext(title, outer = TRUE, cex = 1.5)
  par(mfrow = c(1, 1))
}

# Generate box plots for each category
plot_by_category(
  supply_costs_rows,
  supply_costs_categories,
  "Supply Costs",
  "Cost ($)"
)
plot_by_category(
  asset_ownership_rows,
  asset_ownership_categories,
  "Aircraft Ownership",
  "Cost ($)"
)
plot_by_category(
  utilization_rows,
  utilization_categories,
  "Daily Utilization",
  "Hours"
)
