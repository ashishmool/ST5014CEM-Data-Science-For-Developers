# Load Required Libraries
library(dplyr)
library(readr)

# Define Folder Paths for Cleaned and Uncleaned Dataset
folder_path_uncleaned <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/"
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/4.0 School/"

# Define File Names for Cleaned and Uncleaned Dataset
file_name_uncleaned <- "comb_uncleaned_school_info_with_totatt8.csv"
file_name_cleaned <- "comb_cleaned_school_info_with_totatt8.csv"

# Read the Uncleaned Data
uncleaned_school_data <- read_csv(file.path(folder_path_uncleaned, file_name_uncleaned))

# Showing the Structure of the Uncleaned Data before Cleaning
str(uncleaned_school_data)

# Optionally, view the uncleaned dataset
# View(uncleaned_school_data)

# Remove Unwanted Columns
columns_to_remove <- c("LA", "ESTAB", "LAESTAB", "OPENDATE", "CLOSEDATE", 
                       "RELCHAR", "ADMPOL", "OFSTEDLASTINSP", "GENDER", "AGELOW", 
                       "AGEHIGH", "ISPRIMARY", "ISSECONDARY", "ISPOST16")
cleaned_school_data <- uncleaned_school_data %>%
  select(-all_of(columns_to_remove))

# Checking for empty and non-numeric rows in ATT8SCR
invalid_att8scr_rows <- cleaned_school_data %>%
  filter(is.na(ATT8SCR) | ATT8SCR == "" | !grepl("^[0-9]+(\\.[0-9]+)?$", ATT8SCR))

# Remove rows with empty or non-numeric ATT8SCR values
cleaned_school_data <- cleaned_school_data %>%
  filter(!is.na(ATT8SCR) & ATT8SCR != "" & grepl("^[0-9]+(\\.[0-9]+)?$", ATT8SCR))

# Convert ATT8SCR to numeric
cleaned_school_data <- cleaned_school_data %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Display the rows with invalid ATT8SCR
if (nrow(invalid_att8scr_rows) > 0) {
  cat("Rows with invalid ATT8SCR values removed:\n")
  print(invalid_att8scr_rows)
} else {
  cat("No rows with invalid ATT8SCR values found.\n")
}

# Checking Missing Values in each Column
missing_values <- sapply(cleaned_school_data, function(x) sum(is.na(x)))
print(missing_values)

# Remove Rows with Missing SCHNAME, POSTCODE, SCHSTATUS OR OFSTEDRATING
cleaned_school_data <- cleaned_school_data %>%
  filter(!is.na(SCHNAME) & !is.na(POSTCODE) & !is.na(OFSTEDRATING) & !is.na(SCHSTATUS))

# Ensuring Data Consistency
cleaned_school_data <- cleaned_school_data %>%
  mutate(OFSTEDRATING = as.factor(OFSTEDRATING))

# Rechecking Missing Values after Cleaning
missing_values_after_cleaning <- sapply(cleaned_school_data, function(x) sum(is.na(x)))
print(missing_values_after_cleaning)

# Optionally, view the cleaned data
# View(cleaned_school_data)

# Save the Cleaned Data
output_file_path_cleaned <- file.path(folder_path_cleaned, file_name_cleaned)
write_csv(cleaned_school_data, output_file_path_cleaned, col_names = TRUE)

# Confirming File Saved Message
cat("Cleaned school information data with TOTATT8 saved to:", output_file_path_cleaned, "\n")

# Checking the Structure of the Cleaned Data
str(cleaned_school_data)

# Function to remove outliers based on IQR
remove_outliers <- function(data, variable) {
  qnt <- quantile(data[[variable]], probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(data[[variable]], na.rm = TRUE)
  data %>%
    filter(data[[variable]] >= (qnt[1] - H) & data[[variable]] <= (qnt[2] + H))
}

# Remove outliers from ATT8SCR in cleaned_school_data
cleaned_school_data_no_outliers <- cleaned_school_data %>%
  remove_outliers("ATT8SCR")

# Display summary statistics after outlier removal
summary(cleaned_school_data_no_outliers$ATT8SCR)

# Remove Unwanted Columns
columns_to_remove <- c("ADDRESS3")
cleaned_school_data_no_outliers <- cleaned_school_data_no_outliers %>%
  select(-all_of(columns_to_remove))

# Optionally, view the cleaned data without outliers
View(cleaned_school_data_no_outliers)

# Load the filtered_postcode_to_lsoa_with_town_city.csv File
file_path_postcode <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/filtered_postcode_to_lsoa_with_town_city.csv"
postcode_data <- read_csv(file_path_postcode)

# Confirm the structure of the postcode_data
str(postcode_data)

# Perform Left Join with cleaned_school_data_no_outliers on POSTCODE (pcds in postcode_data)
merged_data <- cleaned_school_data_no_outliers %>%
  left_join(postcode_data, by = c("POSTCODE" = "pcds"))

# Check the structure of the merged data
str(merged_data)

# Optionally, view the merged data
View(merged_data)

# Save the Merged Data
output_file_path_merged <- file.path(folder_path_cleaned, "merged_school_info_with_lsoa.csv")
write_csv(merged_data, output_file_path_merged, col_names = TRUE)

# Confirming File Saved Message
cat("Merged school information data saved to:", output_file_path_merged, "\n")

# Load the Bristol_Cornwall_Towns.csv File
towns_file <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/Bristol_Cornwall_Towns.csv"
towns_data <- read_csv(towns_file)

# Ensure that towns_data has unique rows
towns_data <- towns_data %>%
  distinct()

str(towns_data)
str(merged_data)

# Perform Left Join with merged_data on 'lsoa11cd' (from merged_data) and 'LSOA' (from towns_data)
final_data <- merged_data %>%
  left_join(towns_data, by = c("lsoa11cd" = "LSOA"))

# Check the structure of the final data
str(final_data)

# Optionally, view the final data
View(final_data)

# Save the Final Data
output_file_path_final <- file.path(folder_path_cleaned, "final_merged_school_info_with_towns.csv")
write_csv(final_data, output_file_path_final, col_names = TRUE)

# Confirming File Saved Message
cat("Final merged school information data saved to:", output_file_path_final, "\n")

