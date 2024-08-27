# Load required libraries
library(dplyr)
library(readr)
library(lubridate)

# Define the folder path and file names
folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/1.0 Housing Prices/"
file_names <- c("pp-2020.csv", "pp-2021.csv", "pp-2022.csv", "pp-2023.csv")

# Function to read and verify column count
verify_column_count <- function(file_path) {
  data <- read_csv(file_path, col_types = cols(.default = col_character()), n_max = 1)
  num_columns <- ncol(data)
  return(num_columns)
}

# Initialize an empty list to hold data frames
data_list <- list()

# Loop through each file, verify column count, and store in data_list
for (file_name in file_names) {
  file_path <- file.path(folder_path, file_name)
  
  # Verify column count
  num_columns <- verify_column_count(file_path)
  
  if (num_columns == 16) {
    # Read CSV and rename columns
    new_column_names <- c("Category", "Price", "Date", "Postcode", "Not Required 1", "Not Required 2", "Not Required 3", 
                          "PAON", "SAON", "Street", "Locality", "Town/City", "District", "County", "Not Required 4", "Not Required 5")
    
    data <- read_csv(file_path, col_types = cols(.default = col_character())) %>%
      rename_with(~ new_column_names, everything())
    
    # Store data frame in list
    data_list[[file_name]] <- data
    
    # Show column names of the current file
    cat("Column names of", file_name, ":\n")
    print(colnames(data))
    cat("\n")
  } else {
    # Handle case where column count is not as expected
    warning(paste("Incorrect number of columns in file:", basename(file_path)))
    
    # Show actual column count for troubleshooting
    actual_columns <- verify_column_count(file_path)
    cat("Actual number of columns in", file_name, ":", actual_columns, "\n\n")
  }
}

# Combine all data frames into one
combined_data <- bind_rows(data_list)

# Convert Price column to double
combined_data$Price <- as.double(combined_data$Price)

# Convert Date column from "2020-06-12T00:00:00Z" format to POSIXct datetime format
combined_data$Date <- ymd_hms(combined_data$Date, tz = "UTC")

# Define the output file path
output_file_path <- file.path(folder_path, "housing-prices-2020-2023.csv")

# Write the combined data to the new CSV file with a single header row
write_csv(combined_data, output_file_path, col_names = TRUE)

# Confirmation message
cat("Files merged and saved to housing-prices-2020-2023.csv successfully.\n")

# Show the first 10 rows of the merged data
cat("First 10 rows of housing-prices-2020-2023.csv:\n")
print(head(combined_data, 10))
