# Load the necessary packages
library(readr)
library(dplyr)

# Define the file paths
folder_path_obtain <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data"
folder_path_clean <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data"

postcode_to_lsoa_file <- file.path(folder_path_obtain, "Postcode to LSOA.csv")
output_file_path <- file.path(folder_path_clean, "filtered_postcode_to_lsoa_with_town_city.csv")

# Read the CSV files
postcode_to_lsoa_data <- read_csv(postcode_to_lsoa_file)

# View the initial datasets
View(postcode_to_lsoa_data)

# Select only the required columns from postcode_to_lsoa_data
selected_data <- postcode_to_lsoa_data %>%
  select(pcds, lsoa11cd, ladnm, lsoa11nm)

# Filter data for Bristol or Cornwall based on the 'ladnm' column
filtered_data <- selected_data %>%
  filter(grepl("Bristol", ladnm, ignore.case = TRUE) | grepl("Cornwall", ladnm, ignore.case = TRUE))

# Replace 'Bristol, City of' with 'Bristol' in 'ladnm' column
filtered_data <- filtered_data %>%
  mutate(ladnm = ifelse(ladnm == "Bristol, City of", "Bristol", ladnm))

# View the filtered dataset
View(filtered_data)


# Write the merged data to CSV
write_csv(filtered_data, output_file_path)

# Confirmation message
cat(paste("Filtered and merged data saved to:", output_file_path, "\n"))
