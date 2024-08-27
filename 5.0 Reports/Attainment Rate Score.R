# Load Required Libraries
library(dplyr)
library(readr)
library(scales)

# Define File Path for the Cleaned School Data
folder_path_cleaned_school <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/4.0 School/"
file_name_cleaned_school <- "final_merged_school_info_with_towns.csv"

# Reading the Cleaned School Data
cleaned_school_data <- read_csv(file.path(folder_path_cleaned_school, file_name_cleaned_school))

# Normalize the Attainment 8 Scores to create the EducationScore
cleaned_school_data <- cleaned_school_data %>%
  mutate(EducationScore = rescale(ATT8SCR, to = c(0, 10)))

# Handle missing values in the LOCALITY column and rename it
final_education_score <- cleaned_school_data %>%
  select(`Town/City`, SCHNAME, LANAME, ATT8SCR, EducationScore) %>%
  # Remove duplicate entries based on 'Town/City'
  distinct(`Town/City`, .keep_all = TRUE) %>%
  # Capitalize 'Town/City' column
  mutate(`Town/City` = str_to_title(`Town/City`)) %>%
  # Rename columns
  rename(
    `School Name` = SCHNAME,
    `County` = LANAME
  ) %>%
  # Update values in 'County' column
  mutate(County = ifelse(County == "Bristol, City of", "Bristol", County))

# Display the top 10 schools with the highest Education Scores
top_10_education_score <- final_education_score %>%
  arrange(desc(EducationScore)) %>%
  head(10)

# Print the top 10 schools
print(top_10_education_score)

# View the top 10 education scores in a table
View(top_10_education_score)

# Save the combined scores to a CSV file
write_csv(final_education_score, "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/EducationScores.csv")
