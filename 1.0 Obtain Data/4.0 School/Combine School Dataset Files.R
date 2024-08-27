# Load Required Libraries
library(dplyr)
library(readr)

# Defining Folder Paths for School Data
folder_path_bristol <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/Bristol/"
folder_path_cornwall <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/Cornwall/"

# Function to Read and Combine Files
read_and_combine_files <- function(folder_path) {
  # List all files in the folder
  files <- list.files(folder_path, pattern = "*_school_information.csv", full.names = TRUE, recursive = TRUE)
  
  # Read and combine all files
  combined_data <- lapply(files, read_csv) %>%
    bind_rows()
  
  return(combined_data)
}

# Reading and Combining Bristol School Information
bristol_data <- read_and_combine_files(folder_path_bristol)

# Reading and Combining Cornwall School Information
cornwall_data <- read_and_combine_files(folder_path_cornwall)

# Combining Bristol and Cornwall Data
combined_school_data <- bind_rows(bristol_data, cornwall_data)

# Reading specific Bristol file to get TOTATT8
bristol_ks4_file <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/Bristol/2021-22/801_ks4final.csv"
bristol_ks4_data <- read_csv(bristol_ks4_file) %>%
  select(URN, TOTATT8, ATT8SCR)

# Reading specific Cornwall file to get TOTATT8
cornwall_ks4_file <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/Cornwall/2021-22/908_ks4final.csv"
cornwall_ks4_data <- read_csv(cornwall_ks4_file) %>%
  select(URN, TOTATT8, ATT8SCR)

# Combining TOTATT8 data from Bristol and Cornwall
ks4_data <- bind_rows(bristol_ks4_data, cornwall_ks4_data)

# Joining TOTATT8 data with combined school data
combined_school_data <- combined_school_data %>%
  left_join(ks4_data, by = "URN")

# Saving the Updated Combined Data
output_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/4.0 School/comb_uncleaned_school_info_with_totatt8.csv"
write_csv(combined_school_data, output_file_path)

# Confirming File Saved Message
cat("Combined school information data with TOTATT8 saved to:", output_file_path, "\n")

# View the combined dataset
View(combined_school_data)
