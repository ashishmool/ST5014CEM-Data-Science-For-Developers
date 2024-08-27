# Load Required Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(stringr)

# Defining Folder Paths for Cleaned Datasets
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/"
file_name_cleaned <- "cleaned-combined-crime-rate.csv"
population_data_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/cleaned_population_data.csv"

# Reading the Cleaned Crime Rate Data
cleaned_crime_data <- read_csv(file.path(folder_path_cleaned, file_name_cleaned))

# Reading the Population Data
population_data <- read_csv(population_data_path)

# Ensure population_data has 'shortPostcode' for joining
population_data <- population_data %>%
  mutate(shortPostcode = substr(pcds, 1, 5))

# Update 'ladnm' column to 'County' and change "Bristol, City of" to "Bristol"
population_data <- population_data %>%
  mutate(County = ifelse(ladnm == "Bristol, City of", "Bristol", ladnm)) %>%
  select(-ladnm)

# Joining cleaned crime data with population data using 'LSOA code'
joined_data <- cleaned_crime_data %>%
  left_join(population_data, by = c("LSOA code" = "lsoa11cd")) %>%
  mutate(County = coalesce(County.x, County.y)) %>%
  select(-County.x, -County.y) %>%
  mutate(`Town/City` = str_to_title(`Town/City`))

### Plots

# Crime Rates Comparison by Town/City, Crime Type, and County
crime_counts_by_town_county <- cleaned_crime_data %>%
  group_by(`Crime type`, `Town/City`, County) %>%
  summarise(Crime_Count = n(), .groups = 'drop')

plot1 <- ggplot(crime_counts_by_town_county, aes(x = `Crime type`, y = Crime_Count, fill = County)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~County, scales = "free_y") +  # Facet by County
  labs(title = "Crime Rates Comparison by Crime Type and County (2021-2024)", 
       x = "Crime Type", 
       y = "Crime Count") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Adjust x-axis text
        axis.text.y = element_text(size = 7),  # Adjust y-axis text
        strip.text = element_text(size = 10))  # Adjust facet titles

print(plot1)

# Drug Offense Rates by Town/City Over Time
drugs_by_county <- joined_data %>%
  filter(grepl("Drug", `Crime type`, ignore.case = TRUE)) %>%
  group_by(Year = substr(Month, 1, 4), County) %>%
  summarise(sum_n = n(), 
            sum_Population2023 = sum(`New Population`, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(Rate = (sum_n / sum_Population2023) * 10000) %>%
  arrange(Rate)

plot2 <- ggplot(data = drugs_by_county, aes(x = Year, y = Rate, group = County, color = County)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Rate (per 10,000 people)", 
       title = "Drug Offense Rates by County Over Time", color = "County") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot2)

# Vehicle Crime Rates by Town/City for a Specific Year
specific_year <- "2022"
vehicle_crime_data <- joined_data %>%
  filter(substr(Month, 1, 4) == specific_year & grepl("Vehicle crime", `Crime type`, ignore.case = TRUE))

# Aggregate data by Town/City
vehicle_crime_counts <- vehicle_crime_data %>%
  group_by(County, `Town/City`) %>%
  summarise(Crime_Count = n(), 
            Population = sum(`New Population`, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(Crime_Rate = (Crime_Count / Population) * 10000)

# Separate data for Bristol and Cornwall
vehicle_crime_counts_bristol <- vehicle_crime_counts %>%
  filter(County == "Bristol")

vehicle_crime_counts_cornwall <- vehicle_crime_counts %>%
  filter(County == "Cornwall")

# Create radar chart function
create_radar_chart <- function(data, county_name, color) {
  radar_data <- data %>%
    mutate(`Town/City` = factor(`Town/City`, levels = unique(`Town/City`))) %>%
    mutate(`Town/City` = str_to_title(`Town/City`))
  
  ggplot(radar_data, aes(x = `Town/City`, y = Crime_Rate, group = 1)) +
    geom_polygon(aes(group = 1), fill = color, color = "blue", alpha = 0.4) +
    geom_line(color = "red", size = 0.1) +
    labs(title = paste("Vehicle Crime Rate per 10,000 People in", specific_year, "-", county_name), 
         x = "Town/City", 
         y = "Crime Rate per 10,000 People") +
    coord_polar(start = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create radar charts for Bristol and Cornwall
plot_vehicle_crime_bristol <- create_radar_chart(vehicle_crime_counts_bristol, "Bristol", "orange")
plot_vehicle_crime_cornwall <- create_radar_chart(vehicle_crime_counts_cornwall, "Cornwall", "skyblue")

# Display the radar charts side by side
plot_vehicle_crime_combined <- plot_vehicle_crime_bristol + plot_vehicle_crime_cornwall
print(plot_vehicle_crime_combined)

# Read the CSV file with town/city and LSOA information
town_city_data <- read.csv("D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/Bristol_Cornwall_Towns.csv")

# Rename columns in town_city_data to match population_data
town_city_data_prepped <- town_city_data %>%
  rename(`LSOA code` = LSOA, `Town/City` = Town.City)

# Check the first few rows to ensure the renaming is correct
print(head(town_city_data_prepped))

# Prepare population data with appropriate column names
population_data_prepped <- population_data %>%
  select(`New Population`, lsoa11cd) %>%
  left_join(town_city_data_prepped, by = c("lsoa11cd" = "LSOA code"))

# Check the first few rows of the prepared population data
print(head(population_data_prepped))

# Filter data for robbery crimes in 2022
robbery_crime_data <- cleaned_crime_data %>%
  filter(substr(Month, 1, 4) == "2022" & grepl("Robbery", `Crime type`, ignore.case = TRUE))

# Check the first few rows of robbery_crime_data
print(head(robbery_crime_data))

# Ensure population_data_prepped has 'LSOA code'
population_data_prepped <- population_data %>%
  rename(`LSOA code` = lsoa11cd) %>%
  select(`LSOA code`, `New Population`, County)


# Join with population data and calculate robbery crime rates
robbery_crime_rates <- robbery_crime_data %>%
  group_by(`Town/City`, County, `LSOA code`) %>%
  summarise(Crime_Count = n(), .groups = 'drop') %>%
  left_join(population_data_prepped, by = "LSOA code") %>%
  mutate(Crime_Rate = (Crime_Count / `New Population`) * 10000) %>%
  filter(!is.na(`New Population`))


# Check the first few rows of robbery_crime_rates
print(head(robbery_crime_rates))

# Function to create pie chart for a specific county
create_pie_chart <- function(data, county_name, color) {
  pie_data <- data %>%
    mutate(`Town/City` = factor(`Town/City`, levels = unique(`Town/City`))) %>%
    arrange(desc(Crime_Rate)) %>%
    mutate(`Town/City` = str_to_title(`Town/City`))
  
  ggplot(pie_data, aes(x = "", y = Crime_Rate, fill = `Town/City`)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(title = paste("Robbery Crime Rate per 10,000 People in", "2022", "-", county_name)) +
    theme_void() +
    theme(legend.title = element_blank())
}

str(robbery_crime_rates)

# Create pie charts for Bristol and Cornwall
pie_chart_bristol <- create_pie_chart(robbery_crime_rates %>% filter(County.x == "Bristol"), "Bristol", "orange")
pie_chart_cornwall <- create_pie_chart(robbery_crime_rates %>% filter(County.x == "Cornwall"), "Cornwall", "skyblue")

# Display the pie charts side by side
pie_chart_combined <- pie_chart_bristol + pie_chart_cornwall
print(pie_chart_combined)

str(robbery_crime_data)

# Step 1: Filter the data for drug-related crimes
drugs_crime_data <- cleaned_crime_data %>%
  filter(grepl("Drugs", `Crime type`, ignore.case = TRUE))

# Step 2: Aggregate data by town and county
drugs_crime_by_town_county <- drugs_crime_data %>%
  group_by(`Town/City`, County) %>%
  summarise(Drug_Crime_Count = n(), .groups = 'drop') %>%
  filter(Drug_Crime_Count > 0) %>%  # Remove towns with no count
  mutate(`Town/City` = str_to_title(`Town/City`))  # Capitalize first letter of each word

# Step 3: Create the plot, faceted by County with different colors for Bristol and Cornwall
ggplot(drugs_crime_by_town_county, aes(x = reorder(`Town/City`, -Drug_Crime_Count), y = Drug_Crime_Count, fill = County)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Bristol" = "orange", "Cornwall" = "skyblue")) +  # Set default colors for Bristol and Cornwall
  facet_wrap(~ County, scales = "free_y") +
  labs(title = "Count of Drug-Related Crimes by Town and County",
       x = "Town/City",
       y = "Number of Drug-Related Crimes") +
  coord_flip() +  # Flip coordinates for better readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        strip.text = element_text(size = 12)) 


# Save plots as images
ggsave("crime_rates_comparison.png", plot = plot1, width = 12, height = 8)
ggsave("drug_offense_rates.png", plot = plot2, width = 12, height = 8)
ggsave("vehicle_crime_rates_bristol_cornwall.png", plot = plot_vehicle_crime_combined, width = 12, height = 8)
ggsave("robbery_crime_rates_pie_charts.png", plot = pie_chart_combined, width = 12, height = 8)
