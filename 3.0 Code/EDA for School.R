library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

# Defining Folder Path for Cleaned School Data
folder_path_cleaned_school <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/4.0 School/"
file_name_cleaned_school <- "cleaned_school_info_with_totatt8_no_outliers.csv"

# Reading the Cleaned School Data
cleaned_school_data <- read_csv(file.path(folder_path_cleaned_school, file_name_cleaned_school))

# Update `LANAME` to `County` and adjust values
cleaned_school_data <- cleaned_school_data %>%
  rename(County = LANAME) %>%  # Rename LANAME to County
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Bristol, City Of", "Bristol", County))

# View cleaned data
View(cleaned_school_data)

# Boxplot for Average Attainment 8 Score (2021-2022)
plot_attainment8_boxplot <- ggplot(cleaned_school_data, aes(x = County, y = ATT8SCR, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Score (2021-2022) for Bristol and Cornwall",
       x = "County", y = "Attainment 8 Score") +
  theme_minimal()

# Display boxplot
print(plot_attainment8_boxplot)

# Filter data for Bristol
bristol_data_2021_2022 <- cleaned_school_data %>%
  filter(County == "Bristol")

# Line Chart for Bristol
plot_bristol_attainment8_line <- ggplot(bristol_data_2021_2022, aes(x = reorder(SCHNAME, ATT8SCR), y = ATT8SCR, group = 1, color = SCHNAME)) +
  geom_line() +
  geom_point() +  # Add points for each school
  labs(title = "Bristol's Average Attainment 8 Score (2021-2022) by School",
       x = "School", y = "Average Attainment 8 Score") +
  theme_minimal() +
  coord_flip()  # Flip coordinates to make it horizontal

# Display line chart for Bristol
print(plot_bristol_attainment8_line)

# Filter data for Cornwall
cornwall_data_2021_2022 <- cleaned_school_data %>%
  filter(County == "Cornwall")

# Line Chart for Cornwall
plot_cornwall_attainment8_line <- ggplot(cornwall_data_2021_2022, aes(x = reorder(SCHNAME, ATT8SCR), y = ATT8SCR, group = 1, color = SCHNAME)) +
  geom_line() +
  geom_point() +  # Add points for each school
  labs(title = "Cornwall's Average Attainment 8 Score (2021-2022) by School",
       x = "School", y = "Average Attainment 8 Score") +
  theme_minimal() +
  coord_flip()  # Flip coordinates to make it horizontal

# Display line chart for Cornwall
print(plot_cornwall_attainment8_line)
