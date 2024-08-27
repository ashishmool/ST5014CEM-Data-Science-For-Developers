# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Define File Paths for Cleaned Housing Prices and Cleaned Crime Data
folder_path_housing <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/1.0 Housing Prices/"
file_name_housing <- "cleaned-housing-prices-2020-2023.csv"
folder_path_crime <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/"
file_name_crime <- "cleaned-combined-crime-rate.csv"

# Read the Cleaned Housing Prices Data
housing_data <- read_csv(file.path(folder_path_housing, file_name_housing))

# Read the Cleaned Crime Data for 2022
crime_data <- read_csv(file.path(folder_path_crime, file_name_crime))

# View column names to confirm structure
View(housing_data)
View(crime_data)

# Calculate the average house price per LSOA code in the housing data
housing_data_avg <- housing_data %>%
  group_by(`Town/City`, County) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE)) %>%
  mutate(`Town/City` = str_to_title(`Town/City`))


# Filter crime_data
crime_data_filtered <- crime_data %>%
  filter(substr(Month, 1, 4) == "2022")

# Filter crime_data_filtered for "Drugs" and count occurrences per LSOA code
drug_crime_counts <- crime_data_filtered %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(`Town/City`) %>%
  summarize(Drug_Crime_Count = n()) %>%
  mutate(`Town/City` = str_to_title(`Town/City`))


str(housing_data_avg)
str(drug_crime_counts)


combined_data <- housing_data_avg %>%
  left_join(drug_crime_counts, by = "Town/City")


# Replace NA values in Drug_Crime_Count with 0
combined_data <- combined_data %>%
  mutate(Drug_Crime_Count = ifelse(is.na(Drug_Crime_Count), 0, Drug_Crime_Count))

# Create Drug_Crime_Rate (for simplicity, using the count directly as a proxy for the rate)
combined_data <- combined_data %>%
  mutate(Drug_Crime_Rate = Drug_Crime_Count)

# Filter data based on a specific range, for example, drug crime rates between 0 and 10
filtered_data <- combined_data %>%
  filter(Drug_Crime_Rate <= 10)

# Removing Outlier in Bristol (assuming outlier filtering is defined earlier)
data_to_plot <- filtered_data 

# Fit the Linear Model
linear_model_filtered <- lm(Average_Price ~ Drug_Crime_Rate, data = data_to_plot)

# Summary Statistics of the Linear Model
summary(linear_model_filtered)

# Plot: Average House Prices vs Drug Crime Rate (Filtered), Differentiated by County
ggplot(data_to_plot, aes(x = Drug_Crime_Rate, y = Average_Price, color = County)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("Cornwall" = "skyblue", "Bristol" = "orange")) +
  labs(title = "Average House Prices vs Drug Crime Rate by Town/City",
       x = "Drug Crime Rate (Filtered)", y = "Average House Prices",
       color = "County") +
  theme_minimal()
