# Load required libraries
library(dplyr)
library(readr)

# Define the folder paths and file names
broadband_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/3.0 Broadband Speed/"
postcode_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data"
broadband_file_names <- c("201805_fixed_pc_performance_r03.csv", "201809_fixed_pc_coverage_r01.csv")
postcode_file_name <- "filtered_postcode_to_lsoa_data.csv"
combined_broadband_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/3.0 Broadband Speed"

# Initialize an empty list to hold data frames
data_list <- list()

# Loop through each file, read and store in data_list
for (file_name in broadband_file_names) {
  file_path <- file.path(broadband_folder_path, file_name)
  
  # Read CSV file
  data <- read_csv(file_path)
  
  # Store data frame in list
  data_list[[file_name]] <- data
}

# View the dataframes
View(filtered_postcode_data)

# Read filtered postcode to LSOA data
filtered_postcode_data <- read_csv(file.path(postcode_folder_path, postcode_file_name))

# Combine datasets based on 'postcode' column
combined_broadband_data <- inner_join(data_list[[1]], data_list[[2]], by = "pcds")
View(combined_broadband_data)


# Inner join the combined broadband data with filtered postcode data based on the 'pcds' column
bc_filtered_broadband <- inner_join(combined_broadband_data, filtered_postcode_data, by = c("postcode" = "pcds"))

# View the filtered broadband data
View(bc_filtered_broadband)

# Optional: Save the filtered data to a new CSV file
output_file <- file.path(combined_broadband_folder_path, "bc_filtered_broadband.csv")
write_csv(bc_filtered_broadband, output_file)

# Confirmation message
cat("Filtered broadband speed data saved to", output_file, "successfully.\n")
