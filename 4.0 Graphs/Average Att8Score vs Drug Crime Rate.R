# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define File Paths for Cleaned School Data and Crime Data
folder_path_final_school <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/4.0 School/"
file_name_final_school <- "final_merged_school_info_with_towns.csv"
folder_path_crime <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/"
file_name_crime <- "cleaned-combined-crime-rate.csv"

# Read the Final School Data with Attainment 8 Scores
final_school_data <- read_csv(file.path(folder_path_final_school, file_name_final_school))

# Read the Cleaned Crime Data for 2022
crime_data <- read_csv(file.path(folder_path_crime, file_name_crime))

# Filter crime_data to include only the first four months of 2022
crime_data_filtered <- crime_data %>%
  filter(substr(Month, 1, 4) == "2022" & substr(Month, 6, 7) %in% c("01", "02", "03", "04"))

# Count occurrences of drug crimes per Town/City
drug_crime_counts <- crime_data_filtered %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(`LSOA code`, `Town/City`) %>%
  summarize(Drug_Crime_Count = n(), .groups = 'drop')

# Rename 'lsoa11cd' to 'LSOA code' in final_school_data to match with crime_data
final_school_data <- final_school_data %>%
  rename(`LSOA code` = lsoa11cd)

# Join the datasets by 'LSOA code'
combined_data <- inner_join(final_school_data, drug_crime_counts, by = "LSOA code")

# Resolve column name conflicts by keeping the relevant 'Town/City' column
combined_data <- combined_data %>%
  select(-`Town/City.x`) %>%
  rename(Town_City = `Town/City.y`)

# Calculate Average Attainment 8 Score and Drug Crime Rate
aggregated_data <- combined_data %>%
  group_by(Town_City, LANAME) %>%
  summarize(Average_ATT8Score = mean(ATT8SCR, na.rm = TRUE),
            Drug_Crime_Rate = sum(Drug_Crime_Count, na.rm = TRUE) / n(), .groups = 'drop')

# Remove outliers where Drug Crime Rate > 40
filtered_data <- aggregated_data %>%
  filter(Drug_Crime_Rate <= 40)

# Create Linear Model: Average Attainment 8 Score vs Drug Crime Rate
linear_model <- lm(Average_ATT8Score ~ Drug_Crime_Rate, data = filtered_data)

# Summary Statistics of the Linear Model
summary(linear_model)

# Plot: Average Attainment 8 Score vs Drug Crime Rate, Differentiated by County
ggplot(filtered_data, aes(x = Drug_Crime_Rate, y = Average_ATT8Score, color = LANAME)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("Cornwall" = "skyblue", "Bristol, City of" = "orange")) +
  labs(title = "Average Attainment 8 Score vs Drug Crime Rate by Town/City",
       x = "Drug Crime Rate", y = "Average Attainment 8 Score",
       color = "County") +
  theme_minimal()
