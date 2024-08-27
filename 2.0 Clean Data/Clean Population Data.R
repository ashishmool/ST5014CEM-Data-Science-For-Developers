# Load Required Libraries
library(dplyr)
library(readr)
library(tidyr)

# Define File Paths for Uncleaned Population Dataset and Cleaned Postcode Dataset
folder_path_uncleaned <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/"
file_name_uncleaned <- "Population2011_1656567141570.csv"
postcode_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/filtered_postcode_to_lsoa_with_town_city.csv"

# Define File Path for Cleaned Population Dataset
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/"
file_name_cleaned <- "cleaned_population_data.csv"

# Read the Uncleaned Population Data
population_data <- read_csv(file.path(folder_path_uncleaned, file_name_uncleaned))

# Showing the Structure of the Uncleaned Data before Cleaning
str(population_data)

View(population_data)

# Remove Records with Null Values
population_data <- population_data %>%
  drop_na()

# Remove Extra Spaces from Postcode column
population_data <- population_data %>%
  mutate(Postcode = gsub("\\s+", " ", Postcode))  # Replace multiple spaces with a single space

# Read the Cleaned Postcode Data
postcode_data <- read_csv(postcode_file_path)

# View the structure of postcode_data to verify
str(postcode_data)

# Extract First 5 Characters of 'pcds' Column
postcode_data <- postcode_data %>%
  mutate(pcds = substr(pcds, 1, 5))

# Inner Join with Population Data on 'Postcode' and 'pcds' columns
merged_data <- inner_join(postcode_data, population_data, by = c("pcds" = "Postcode"))

# Add a New Column "New Population" by Multiplying the Current Population
merged_data <- merged_data %>%
  mutate(`New Population` = 1.00561255390388033 * Population)

# Remove Duplicate Records based on pcds column
merged_data <- merged_data %>%
  distinct(pcds, .keep_all = TRUE)

# View the merged data to check
View(merged_data)

# Save the Cleaned Population Data
output_file_path_cleaned <- file.path(folder_path_cleaned, file_name_cleaned)
write_csv(merged_data, output_file_path_cleaned, col_names = TRUE)

# Confirming File Saved Message
cat("Cleaned population data saved to:", output_file_path_cleaned, "\n")

str(merged_data)