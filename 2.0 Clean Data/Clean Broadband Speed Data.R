# Load Required Libraries
library(dplyr)
library(readr)

# Defining Folder Paths for Cleaned and Uncleaned Dataset
folder_path_uncleaned <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/3.0 Broadband Speed/"
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/"
postcode_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data"
obtain_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data"

# Defining File Names for Cleaned and Uncleaned Dataset
file_name_uncleaned <- "combined_broadband_data.csv"
file_name_cleaned <- "cleaned-combined-broadband-speed.csv"
postcode_file_name <- "filtered_postcode_to_lsoa_data.csv"
towns_file_name <- "Bristol_Cornwall_Towns.csv"


# Reading the Combined Broadband Speed Data
combined_broadband_data <- read_csv(file.path(folder_path_uncleaned, file_name_uncleaned))

# Showing the Structure of the Uncleaned Data before Cleaning
str(combined_broadband_data)

# List of Columns to Remove
columns_to_remove <- c(
  "postcode", "postcode area",
  "Average upload speed (Mbit/s) for lines < 10Mbit/s",
  "Average upload speed (Mbit/s) for lines 10<30Mbit/s",
  "Average upload speed (Mbit/s) for SFBB lines",
  "Average upload speed (Mbit/s) for UFBB lines",
  "Number of connections < 2 Mbit/s (number of lines)",
  "Number of connections 2<5 Mbit/s (number of lines)",
  "Number of connections 5<10 Mbit/s (number of lines)",
  "Number of connections 10<30 Mbit/s (number of lines)",
  "Number of connections 30<300 Mbit/s (number of lines)",
  "Number of connections >= 300 Mbit/s (number of lines)",
  "Number of connections >= 30 Mbit/s (number of lines)",
  "Average data usage (GB) for Basic BB lines",
  "Average data usage (GB) for SFBB lines",
  "Average data usage (GB) for UFBB lines",
  "All Matched Premises", "SFBB availability (% premises)",
  "UFBB availability (% premises)", "FTTP availability (% premises)",
  "% of premises unable to receive 2Mbit/s",
  "% of premises unable to receive 5Mbit/s",
  "% of premises unable to receive 10Mbit/s",
  "% of premises unable to receive 30Mbit/s",
  "% of premises unable meet USO",
  "% of premises able to receive decent broadband from FWA",
  "% of premises able to receive SFBB from FWA",
  "% of premises able to receive NGA", "pca", "pcds",
  "Median download speed (Mbit/s)",
  "Average download speed (Mbit/s) for lines < 10Mbit/s",
  "Average download speed (Mbit/s) for SFBB lines",
  "Average download speed (Mbit/s) for UFBB lines",
  "Median upload speed (Mbit/s)",
  "Average data usage (GB) for lines < 10Mbit/s",
  "Average download speed (Mbit/s) for lines 10<30Mbit/s",
  "All Premises"
)

# Removing Specified Columns
cleaned_broadband_data <- combined_broadband_data %>%
  select(-one_of(columns_to_remove))

# View cleaned data
View(cleaned_broadband_data)

# Reading the Postcode to LSOA Data
postcode_to_lsoa_data <- read_csv(file.path(postcode_folder_path, postcode_file_name))

# Inner Join with cleaned_broadband_data on 'postcode' and 'pcds' columns
merged_broadband_data <- inner_join(cleaned_broadband_data, postcode_to_lsoa_data, by = c("postcode_space" = "pcds"))

# Rename 'lsoa11cd' to 'LSOA'
merged_broadband_data <- merged_broadband_data %>%
  rename(LSOA = lsoa11cd)

# Replace "Bristol, City of" with "Bristol" in 'ladnm'
merged_broadband_data <- merged_broadband_data %>%
  mutate(ladnm = ifelse(ladnm == "Bristol, City of", "Bristol", ladnm))

# Check Merged Data
View(merged_broadband_data)

# Show the structure of the cleaned data
str(merged_broadband_data)


# Reading the Bristol_Cornwall_Towns.csv Data
towns_data <- read_csv(file.path(obtain_folder_path, towns_file_name))

# Removing Duplicate Rows in Towns Data
towns_data <- towns_data %>%
  distinct()

# View Towns Data to Confirm Duplicates Removed
View(towns_data)

# Merging Town/City Column with Merged Broadband Data
final_broadband_data <- merged_broadband_data %>%
  left_join(towns_data, by = "LSOA")

View(final_broadband_data)

str(final_broadband_data)

final_broadband_data_cleaned <- final_broadband_data %>%
  filter(
    !is.na(`Average download speed (Mbit/s)`) &
      !is.na(`Minimum download speed (Mbit/s)`) &
      !is.na(`Maximum download speed (Mbit/s)`) &
      !is.na(`Average upload speed (Mbit/s)`) &
      !is.na(`Minimum upload speed (Mbit/s)`) &
      !is.na(`Maximum upload speed (Mbit/s)`) &
      !is.na(`Average data usage (GB)`) &
      !is.na(ladnm) &
      !is.na(`Town/City`)
  )


# Saving the Final Merged Data to the Cleaned Dataset File Path
output_file_path_final <- file.path(folder_path_cleaned, file_name_cleaned)
write_csv(final_broadband_data, output_file_path_final, col_names = TRUE)

# Confirming File Saved Message
cat("Final merged data with Town/City column saved to:", output_file_path_final, "\n")

# Check Final Merged Data
View(final_broadband_data)

# Show the structure of the final cleaned data
str(final_broadband_data)