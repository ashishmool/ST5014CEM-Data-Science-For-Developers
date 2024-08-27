# Load Required Libraries
library(dplyr)
library(readr)
library(scales)
library(stringr)  # Add this package for str_to_title

# Define File Path
housing_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/1.0 Housing Prices/cleaned-housing-prices-2020-2023.csv"

# Read the Cleaned Data
housing_data <- read_csv(housing_file_path)

# Convert the Date column to Date format
housing_data$Date <- as.Date(housing_data$Date, format="%Y-%m-%d")

# Filter the data for the year 2022
data_2022 <- housing_data %>%
  filter(format(Date, "%Y") == "2022")

# Calculate Average Price by Town/City and County
avg_price_town_2022 <- data_2022 %>%
  group_by(County, `Town/City`) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()


avg_price_town_2022 <- avg_price_town_2022 %>%
  # Capitalize 'Town/City' column
  mutate(`Town/City` = str_to_title(`Town/City`))
  
# Creating House Price Score: Lower Price means Higher Score
avg_price_town_2022 <- avg_price_town_2022 %>%
  mutate(HouseScore = rescale(-Avg_Price, to = c(0, 10)))
  
  
avg_price_town_2022 <- avg_price_town_2022 %>%
  # Remove rows with NULL values
  filter(!is.na(`Town/City`), !is.na(Avg_Price), !is.na(HouseScore)) %>%
  
  # Remove duplicate entries based on 'Town/City'
  distinct(`Town/City`, .keep_all = TRUE)

# Select only the relevant columns for the final dataset
final_house_score <- avg_price_town_2022 %>%
  select(`Town/City`, County, Avg_Price, HouseScore)

# Display the top 10 towns/cities with the highest House Scores
top_10_house_score <- final_house_score %>%
  arrange(desc(HouseScore)) %>%
  head(10)

# Print the top 10 towns/cities
print(top_10_house_score)

# View the final house scores in a table
View(top_10_house_score)

# Save the combined scores to a CSV file
write_csv(final_house_score, "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/HousingPriceScores.csv")
