# Load required libraries
library(dplyr)
library(readr)

# Define the file path for pp-2020.csv
file_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/1.0 Housing Prices/pp-2020.csv"

# Function to verify column count and convert Date format
verify_and_process_pp_2020 <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path, col_types = cols(.default = col_character()))
  
  # Verify column count
  if (ncol(data) != 16) {
    stop("The file does not have 16 columns as expected.")
  }
  
  # Rename columns if necessary (assuming the column names are consistent)
  new_column_names <- c("Category", "Price", "Date", "Postcode", "Not Required 1", "Not Required 2", "Not Required 3", 
                        "PAON", "SAON", "Street", "Locality", "Town/City", "District", "County", "Not Required 4", "Not Required 5")
  colnames(data) <- new_column_names
  
  # Convert Date column to POSIXct format if needed
  data$Date <- as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M", tz = "UTC")
  
  # Convert Price column to double
  data$Price <- as.double(data$Price)
  
  return(data)
}

# Call the function to verify and process pp-2020.csv
processed_data <- verify_and_process_pp_2020(file_path)

# Show column names and first 10 rows to verify
cat("Column names of pp-2020.csv after processing:\n")
print(colnames(processed_data))
cat("\n")

cat("First 10 rows of pp-2020.csv after processing:\n")
print(head(processed_data, 10))

# Save the processed data back to pp-2020.csv
write_csv(processed_data, file_path, col_names = TRUE)

# Confirmation message
cat("Processed data has been saved to pp-2020.csv successfully.\n")

# Summary of processed data
summary(processed_data)
