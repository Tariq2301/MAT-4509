library(readxl)
library(rstudioapi)

# Set working directory to the location of the Excel file
setwd("C:/Users/ASUS/Desktop/assignment")

# Verify if the file exists
file.exists("United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls")

# If the file extension is .xlsx, update the file name accordingly:
input_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xlsx"

# If the file extension is .xls, use:
input_file <- "United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls"

# Read the Excel file with the specified range
data_table <- read_excel(input_file, range = "B2:W158")
data_table

cost_categories <- c("labor", "materials", "third_party", "burden")
year_range <- 1995:2015

# Function to extract data for a given row number (Maintenance/Load Factor)
extract_row_data <- function(row_number) {
  return(na.omit(as.numeric(data_table[row_number, -1])))
}

# Function to calculate maintenance category data
calculate_maintenance <- function(row_number) {
  labor_cost <- extract_row_data(row_number + 1)
  material_cost <- extract_row_data(row_number + 2)
  third_party_cost <- extract_row_data(row_number + 3)
  burden_cost <- extract_row_data(row_number + 5)
  
  return(setNames(
    c(sum(labor_cost), sum(material_cost), sum(third_party_cost), sum(burden_cost)),
    cost_categories
  ))
}

# Function to create a bar plot
generate_bar_plot <- function(data, title) {
  barplot(data,
          main = title,
          xlab = "Years",
          ylab = "Load Factor (%)",
          col = "skyblue",
          border = "darkblue"
  )
}

# Row numbers for maintenance and load factor
maintenance_rows <- c(16, 55, 94, 133)
load_factor_rows <- c(34, 73, 112, 151)

fleet_labels <- c(
  "Small Narrowbodies",
  "Large Narrowbodies",
  "Widebodies",
  "Total Fleet"
)

# Load necessary library
library(RColorBrewer)  # For color palettes

# Pie chart for maintenance
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Define a new color palette
color_palette <- c("tomato", "gold", "darkgreen", "cornflowerblue")

# Create pie charts for each maintenance category with enhancements
lapply(1:4, function(i) {
  maintenance_data <- calculate_maintenance(maintenance_rows[i])
  
  # Calculate percentages
  percentages <- round(100 * maintenance_data / sum(maintenance_data), 1)
  
  # Create labels with category names and percentages
  chart_labels <- paste0(names(maintenance_data), ": ", percentages, "%")
  
  # Create pie chart
  pie(maintenance_data, 
      labels = chart_labels,        # Use labels with percentages
      main = paste("Maintenance Costs for", fleet_labels[i]),  # Descriptive title
      col = color_palette,          # Set colors for slices
      border = "black")             # Add border to slices
})

# Add an outer title for all pie charts
mtext("Maintenance Cost Distribution", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))


# Bar chart for load factor
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

lapply(1:4, function(i) {
  load_factor_data <- setNames(extract_row_data(load_factor_rows[i]), year_range)
  generate_bar_plot(load_factor_data, fleet_labels[i])
})

mtext("Load Factor Analysis", outer = TRUE, cex = 1.5)
par(mfrow = c(1, 1))
