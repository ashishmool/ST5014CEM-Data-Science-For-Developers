# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define File Paths for Cleaned Broadband Speed and Final School Data
folder_path_broadband <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/"
file_name_broadband <- "cleaned-combined-broadband-speed.csv"
folder_path_final_school <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/4.0 School/"
file_name_final_school <- "final_merged_school_info_with_towns.csv"

# Read the Cleaned Broadband Speed Data
broadband_data <- read_csv(file.path(folder_path_broadband, file_name_broadband))

# Read the Final School Data with Attainment 8 Scores
final_school_data <- read_csv(file.path(folder_path_final_school, file_name_final_school))

# Rename 'lsoa11cd' to 'LSOA' in final_school_data to match with broadband_data
final_school_data <- final_school_data %>%
  rename(LSOA = lsoa11cd)

# Join the datasets by 'LSOA'
combined_data <- inner_join(broadband_data, final_school_data, by = "LSOA")

# Rename the columns for clarity
combined_data <- combined_data %>%
  select(-`Town/City.y`) %>%
  rename(Town_City = `Town/City.x`)

# Group by Town/City and County, Calculate Average Download Speed and Average Attainment 8 Score
aggregated_data <- combined_data %>%
  group_by(Town_City, ladnm) %>%
  summarize(Average_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
            Average_ATT8Score = mean(ATT8SCR, na.rm = TRUE),
            .groups = 'drop')

# Create Linear Model: Average Download Speed vs Average Attainment 8 Score
linear_model <- lm(Average_Download_Speed ~ Average_ATT8Score, data = aggregated_data)

# Summary Statistics of the Linear Model
summary(linear_model)

# Plot: Average Download Speed vs Average Attainment 8 Score, Differentiated by County
ggplot(aggregated_data, aes(x = Average_ATT8Score, y = Average_Download_Speed, color = ladnm)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("Cornwall" = "skyblue", "Bristol" = "orange")) +
  labs(title = "Average Download Speed vs Average Attainment 8 Score by Town/City",
       x = "Average Attainment 8 Score", y = "Average Download Speed (Mbit/s)",
       color = "County") +
  theme_minimal()
