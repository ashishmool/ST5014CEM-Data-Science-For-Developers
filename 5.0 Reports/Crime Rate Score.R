# Load Required Libraries
library(dplyr)
library(readr)
library(scales)
library(stringr)  # For str_to_title

# Define File Paths for the Cleaned Data
crime_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/2.0 Crime Rates/cleaned-combined-crime-rate.csv"
population_data_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/cleaned_population_data.csv"

# Read the Cleaned Crime Rate Data
crime_data <- read_csv(crime_file_path)

# Read the Population Data
population_data <- read_csv(population_data_path)

# Ensure population_data has 'shortPostcode' for joining
population_data <- population_data %>%
  mutate(shortPostcode = substr(pcds, 1, 5))

# Update 'ladnm' column to 'County' and change "Bristol, City of" to "Bristol"
population_data <- population_data %>%
  mutate(County = ifelse(ladnm == "Bristol, City of", "Bristol", ladnm)) %>%
  select(-ladnm)

# Join crime data with population data using 'LSOA code'
joined_data <- crime_data %>%
  left_join(population_data, by = c("LSOA code" = "lsoa11cd")) %>%
  mutate(County = coalesce(County.x, County.y)) %>%
  select(-County.x, -County.y)

# Calculate Total Crime and Population by Town/City and County
crime_rate_town <- joined_data %>%
  group_by(County, `Town/City`) %>%
  summarise(Total_Crimes = n(), Total_Population = sum(`New Population`, na.rm = TRUE)) %>%
  mutate(Crime_Rate = (Total_Crimes / Total_Population) * 10000) %>%
  ungroup()

# Creating Crime Rate Score: Lower Crime Rate means Higher Score
crime_rate_town <- crime_rate_town %>%
  mutate(CrimeRateScore = rescale(-Crime_Rate, to = c(0, 10))) %>%
  # Remove rows with Inf or -Inf values
  filter(!is.infinite(CrimeRateScore)) %>%
  # Capitalize 'Town/City' column
  mutate(`Town/City` = str_to_title(`Town/City`))

# Select only the relevant columns for the final dataset
final_crime_score <- crime_rate_town %>%
  select(`Town/City`, County, Crime_Rate, CrimeRateScore) %>%
  distinct()

# Display the top 10 towns/cities with the highest Crime Rate Scores
top_10_crime_score <- final_crime_score %>%
  arrange(desc(CrimeRateScore)) %>%
  head(10)

# Print the top 10 towns/cities
print(top_10_crime_score)

# View the final crime scores in a table
View(top_10_crime_score)

# Save the combined scores to a CSV file
write_csv(final_crime_score, "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/CrimeScores.csv")
