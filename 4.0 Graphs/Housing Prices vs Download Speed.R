# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define File Paths for Cleaned Housing Prices and Broadband Speed Data
folder_path_housing <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/1.0 Housing Prices/"
file_name_housing <- "cleaned-housing-prices-2020-2023.csv"
folder_path_broadband <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/"
file_name_broadband <- "cleaned-combined-broadband-speed.csv"

# Read the Cleaned Housing Prices Data
housing_data <- read_csv(file.path(folder_path_housing, file_name_housing))

# Read the Cleaned Broadband Speed Data
broadband_data <- read_csv(file.path(folder_path_broadband, file_name_broadband))

# Ensure there is a common key to merge the datasets (adjusted for column names)
# Join the datasets by "postcode_space" from broadband_data and "Postcode" from housing_data
combined_data <- inner_join(housing_data, broadband_data, by = c("Postcode" = "postcode_space"))

# Rename the Town/City columns for clarity
combined_data <- combined_data %>%
  select(-`Town/City.y`)%>%
  rename(Town_City = `Town/City.x`)



# Group by Town/City and County, Calculate Average House Price and Average Download Speed
aggregated_data <- combined_data %>%
  group_by(Town_City, County) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE),
            Average_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Create Linear Model: Average House Prices vs Average Download Speed
linear_model <- lm(Average_Price ~ Average_Download_Speed, data = aggregated_data)

# Summary Statistics of the Linear Model
summary(linear_model)

# Plot: Average House Prices vs Average Download Speed, Differentiated by County
ggplot(aggregated_data, aes(x = Average_Download_Speed, y = Average_Price, color = County)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("Cornwall" = "skyblue", "Bristol" = "orange")) +
  labs(title = "Average House Prices vs Average Download Speed by Town/City",
       x = "Average Download Speed (Mbit/s)", y = "Average House Prices",
       color = "County") +
  theme_minimal()
