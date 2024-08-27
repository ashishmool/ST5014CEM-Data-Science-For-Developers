# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define File Paths for Cleaned Broadband Speed and Cleaned Crime Data
folder_path_broadband <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/"
file_name_broadband <- "cleaned-combined-broadband-speed.csv"
folder_path_crime <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/"
file_name_crime <- "cleaned-combined-crime-rate.csv"

# Read the Cleaned Broadband Speed Data
broadband_data <- read_csv(file.path(folder_path_broadband, file_name_broadband))

# Read the Cleaned Crime Data for 2022
crime_data <- read_csv(file.path(folder_path_crime, file_name_crime))

# Filter crime_data to include only the first four months of 2022
crime_data_filtered <- crime_data %>%
  filter(substr(Month, 1, 4) == "2022" & substr(Month, 6, 7) %in% c("01", "02", "03", "04"))

# Count occurrences of drug crimes per Town/City
drug_crime_counts <- crime_data_filtered %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(`Town/City`) %>%
  summarize(Drug_Crime_Count = n(), .groups = 'drop')

# Rename 'LSOA' to match with broadband_data
broadband_data <- broadband_data %>%
  rename(`Town/City` = `Town/City`)

# Join the datasets on 'Town/City'
combined_data <- broadband_data %>%
  left_join(drug_crime_counts, by = "Town/City")

# Calculate Average Download Speed and Drug Crime Rate
aggregated_data <- combined_data %>%
  group_by(`Town/City`, ladnm) %>%
  summarize(Average_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
            Drug_Crime_Rate = sum(Drug_Crime_Count, na.rm = TRUE) / n(), .groups = 'drop')

# Remove outliers where Drug Crime Rate > 40
filtered_data <- aggregated_data %>%
  filter(Drug_Crime_Rate <= 40)

# Create Linear Model: Average Download Speed vs Drug Crime Rate
linear_model <- lm(Average_Download_Speed ~ Drug_Crime_Rate, data = filtered_data)

# Summary Statistics of the Linear Model
summary(linear_model)

# Plot: Average Download Speed vs Drug Crime Rate, Differentiated by County
ggplot(filtered_data, aes(x = Drug_Crime_Rate, y = Average_Download_Speed, color = ladnm)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("Cornwall" = "skyblue", "Bristol" = "orange")) +
  labs(title = "Average Download Speed vs Drug Crime Rate by Town/City",
       x = "Drug Crime Rate", y = "Average Download Speed (Mbit/s)",
       color = "County") +
  theme_minimal()
