# Load Required Libraries
library(dplyr)
library(readr)

# Defining Folder Paths for Cleaned and Uncleaned Dataset
folder_path_uncleaned <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/2.0 Crime Rates/"
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/"
filtered_postcode_to_lsoa_file <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/filtered_postcode_to_lsoa_with_town_city.csv"
towns_file <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/Bristol_Cornwall_Towns.csv"

# Defining File Names for Cleaned and Uncleaned Dataset
file_name_uncleaned <- "combined-crime-rate.csv"
file_name_cleaned <- "cleaned-combined-crime-rate.csv"

# Reading the Uncleaned Data
uncleaned_data <- read_csv(file.path(folder_path_uncleaned, file_name_uncleaned))

# Add County Column based on Reported by
uncleaned_data <- uncleaned_data %>%
  mutate(County = case_when(
    `Reported by` == "Avon and Somerset Constabulary" ~ "Bristol",
    `Reported by` == "Devon & Cornwall Police" ~ "Cornwall",
    TRUE ~ as.character(`Reported by`)  # In case there are other values, keep them as is
  ))

# Reading the Bristol_Cornwall_Towns.csv File
towns_data <- read_csv(towns_file)

# Ensure that towns_data has unique rows
towns_data <- towns_data %>%
  distinct()

# View towns_data for verification
View(towns_data)

# Rename the 'LSOA' column in towns_data to 'LSOA code' to match cleaned_data
towns_data <- towns_data %>%
  rename(`LSOA code` = LSOA)

# Remove Unwanted Data Columns
columns_to_remove <- c("Crime ID", "Context", "Last outcome category", "Falls within", "Reported by", "Latitude", "Longitude", "Location")
uncleaned_data <- uncleaned_data %>%
  select(-one_of(columns_to_remove))

# Removing Rows with Missing Values in Critical Fields
cleaned_data <- uncleaned_data %>%
  filter(!is.na(`LSOA code`) & !is.na(`Crime type`) & !is.na(`County`) & !is.na(`Month`))

View(cleaned_data)
str(cleaned_data)

# Merge cleaned_data with towns_data based on 'LSOA code'
final_merged_data <- cleaned_data %>%
  left_join(towns_data, by = "LSOA code")


# Removing Rows with Missing Values in Critical Fields
final_merged_data <- final_merged_data %>%
  filter(!is.na(`Town/City`))

str(final_merged_data)
View(final_merged_data)

# Saving the Final Merged Data to the Cleaned Dataset File Path
final_output_file_path_cleaned <- file.path(folder_path_cleaned, file_name_cleaned)
write_csv(final_merged_data, final_output_file_path_cleaned, col_names = TRUE)

# Confirming File Saved Message
cat("Filtered and merged data with towns information saved to:", final_output_file_path_cleaned, "\n")
