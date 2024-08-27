# Load Required Libraries
library(dplyr)
library(readr)

# Defining Folder Paths for Bristol and Cornwall Crime Data
bristol_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/Bristol/"
cornwall_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/Cornwall/"

# Verification of Column Names for Crime Rate Data
expected_column_names <- c("Crime ID", "Month", "Reported by", "Falls within", "Longitude", "Latitude", "Location", 
                           "LSOA code", "LSOA name", "Crime type", "Last outcome category", "Context")

# Read and Verify Column Count and Names
read_and_verify_columns <- function(file_path) {
  data <- read_csv(file_path, col_types = cols(.default = col_character()))
  if (all(colnames(data) %in% expected_column_names) && ncol(data) == length(expected_column_names)) {
    return(data)
  } else {
    warning(paste("Incorrect columns in file:", basename(file_path)))
    return(NULL)
  }
}

# Initialize Empty List to Hold Data frames for Bristol and Cornwall
data_list_bristol <- list()
data_list_cornwall <- list()

# Read and Store Data frames in a List
read_and_store_data <- function(folder_path, data_list) {
  file_names <- list.files(folder_path, pattern = "*.csv")
  
  for (file_name in file_names) {
    file_path <- file.path(folder_path, file_name)
    data <- read_and_verify_columns(file_path)
    if (!is.null(data)) {
      data_list[[file_name]] <- data
      cat("File read successfully:", file_name, "- Rows:", nrow(data), "\n")
    }
  }
  return(data_list)
}

# Read and Store Data for Bristol
data_list_bristol <- read_and_store_data(bristol_folder_path, data_list_bristol)

# Read and Store Data for Cornwall
data_list_cornwall <- read_and_store_data(cornwall_folder_path, data_list_cornwall)

# Combine Data frames
combined_data_bristol <- bind_rows(data_list_bristol)
combined_data_cornwall <- bind_rows(data_list_cornwall)

# Merge Combined Datasets from Both Locations
combined_data <- bind_rows(combined_data_bristol, combined_data_cornwall)

# Define Output File Paths
output_file_path_bristol <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/combined-bristol-crime-rate.csv"
output_file_path_cornwall <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/combined-cornwall-crime-rate.csv"
output_file_path_combined <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/combined-crime-rate.csv"

# Write new CSV files
write_csv(combined_data_bristol, output_file_path_bristol, col_names = TRUE)
write_csv(combined_data_cornwall, output_file_path_cornwall, col_names = TRUE)

# Write Combined Data for both locations to a single CSV file
write_csv(combined_data, output_file_path_combined, col_names = TRUE)
cat("Files merged and saved to combined-bristol-crime-rate.csv, combined-cornwall-crime-rate.csv, and combined-crime-rate.csv successfully.\n")

# Check first 10 rows of the Merged Data
cat("First 10 rows of combined-bristol-crime-rate.csv:\n")
print(head(combined_data_bristol, 10))

cat("First 10 rows of combined-cornwall-crime-rate.csv:\n")
print(head(combined_data_cornwall, 10))

cat("First 10 rows of combined-crime-rate.csv:\n")
print(head(combined_data, 10))
